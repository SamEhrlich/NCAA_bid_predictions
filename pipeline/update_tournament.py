#!/usr/bin/env python3
"""Auto-update docs/data/schedule.json and docs/data/results.json from ESPN.

Pulls ESPN's college-baseball scoreboard JSON for each day in the 2026 tournament
window (May 29 - Jun 24) and writes:
  - schedule.json with firm dates/times/networks per regional
  - results.json with round winners as games go FINAL

Designed to be cheap and re-runnable: re-running is idempotent and only commits
git diffs if something actually changed.

Usage:
    python3 update_tournament.py                # run once for the whole window
    python3 update_tournament.py --dry-run      # show what would change, don't write
"""
from __future__ import annotations

import argparse
import json
import sys
from collections import defaultdict
from datetime import datetime, date, timedelta
from pathlib import Path
from typing import Any

import requests

# ----------------------------------------------------------------------------
# Paths
# ----------------------------------------------------------------------------
SCRIPT_DIR = Path(__file__).resolve().parent
ROOT       = SCRIPT_DIR.parent
DOCS       = ROOT / "docs"
DATA_DIR   = DOCS / "data"
BRACKET    = DATA_DIR / "bracket.json"
SCHEDULE   = DATA_DIR / "schedule.json"
RESULTS    = DATA_DIR / "results.json"

# ----------------------------------------------------------------------------
# Tournament window (inclusive on both ends)
# ----------------------------------------------------------------------------
TOURNAMENT_START = date(2026, 5, 29)   # Selection Mon was 5/25; regionals start Fri 5/29
TOURNAMENT_END   = date(2026, 6, 24)   # CWS finals wrap by 6/22-6/24

# Round windows (used to bucket games into regional/super/cws)
REGIONAL_DATES = {date(2026, 5, 29), date(2026, 5, 30), date(2026, 5, 31), date(2026, 6, 1)}
SUPER_DATES    = {date(2026, 6, 5),  date(2026, 6, 6),  date(2026, 6, 7),  date(2026, 6, 8)}
CWS_DATES      = set()
d = date(2026, 6, 13)
while d <= TOURNAMENT_END:
    CWS_DATES.add(d); d += timedelta(days=1)

# ----------------------------------------------------------------------------
# ESPN shortDisplayName -> our canonical team name (from bracket.json).
# Only entries where ESPN's name differs from ours go here; everything else
# matches by exact string.
# ----------------------------------------------------------------------------
ESPN_TO_OURS: dict[str, str] = {
    "Mississippi St":  "Mississippi St.",
    "Florida St":      "Florida St.",
    "Oklahoma St":     "Oklahoma St.",
    "Oregon St":       "Oregon St.",
    "Washington St":   "Washington St.",
    "Arizona St":      "Arizona St.",
    "S Dakota St":     "South Dakota St.",
    "Missouri St":     "Missouri St.",
    "Tarleton St":     "Tarleton St.",
    "Jax State":       "Jacksonville St.",
    "Texas St":        "Texas St.",
    "Alabama St":      "Alabama St.",
    "Saint Mary's":    "Saint Mary's (CA)",
    "St John's":       "St. John's (NY)",
    "Miami":           "Miami (FL)",
    "Long Island":     "LIU",
    "N Illinois":      "NIU",
    "Coastal":         "Coastal Carolina",
    "Santa Barbara":   "UC Santa Barbara",
    "Southern Miss":   "Southern Miss.",
    "SC Upstate":      "USC Upstate",
    "USC":             "Southern California",
    "Lamar":           "Lamar University",
}

ESPN_SCOREBOARD = "https://site.api.espn.com/apis/site/v2/sports/baseball/college-baseball/scoreboard"


# ----------------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------------
def log(msg: str) -> None:
    print(f"[{datetime.now().strftime('%H:%M:%S')}] {msg}", flush=True)


def load_json(path: Path) -> dict:
    return json.loads(path.read_text())


def write_json_if_changed(path: Path, payload: dict, dry_run: bool = False) -> bool:
    """Write payload to path if it differs from current. Returns True if changed."""
    new = json.dumps(payload, indent=2, sort_keys=False) + "\n"
    if path.exists() and path.read_text() == new:
        return False
    if dry_run:
        log(f"  [dry-run] would write {path.name} ({len(new)} bytes)")
        return True
    path.write_text(new)
    log(f"  wrote {path.name}")
    return True


def to_canonical(espn_short: str) -> str:
    """Map an ESPN shortDisplayName to our canonical team name."""
    return ESPN_TO_OURS.get(espn_short, espn_short)


def parse_iso(s: str) -> datetime:
    # ESPN returns "2026-05-29T16:00Z"
    return datetime.fromisoformat(s.replace("Z", "+00:00"))


def fmt_local(dt: datetime) -> str:
    """Format an ISO UTC datetime as a friendly EDT-ish string."""
    # We don't actually convert TZ — most fans see ET-displayed times. The ESPN
    # data is already in UTC; the display string we render is informational.
    et = dt - timedelta(hours=4)   # rough EDT offset (May/Jun = always EDT)
    weekday = ["Mon","Tue","Wed","Thu","Fri","Sat","Sun"][et.weekday()]
    month   = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"][et.month - 1]
    hh = et.hour % 12 or 12
    ampm = "AM" if et.hour < 12 else "PM"
    minute = f":{et.minute:02d}" if et.minute else ""
    return f"{weekday} {month} {et.day} · {hh}{minute} {ampm} EDT"


def fetch_date(d: date) -> list[dict]:
    """Fetch ESPN scoreboard events for a single date."""
    url = f"{ESPN_SCOREBOARD}?dates={d.strftime('%Y%m%d')}"
    try:
        r = requests.get(url, timeout=20,
                         headers={"User-Agent": "ncaa-elo-tournament-tracker/1.0 (https://samehrlich.github.io/NCAA_bid_predictions/)"})
        r.raise_for_status()
        return r.json().get("events", [])
    except Exception as e:
        log(f"  ESPN fetch failed for {d}: {e}")
        return []


# ----------------------------------------------------------------------------
# Mapping: ESPN game -> our (regional_key OR super_key OR cws_slot)
# ----------------------------------------------------------------------------
def build_team_to_regional(bracket: dict) -> dict[str, str]:
    """team name -> regional key (los_angeles, atlanta, ...)"""
    out = {}
    for r in bracket["regionals"]:
        for t in r["teams"]:
            out[t["team"]] = r["key"]
    return out


def classify_game(event: dict,
                  team_to_regional: dict[str, str],
                  game_date: date) -> dict | None:
    """Identify what slot this game maps to. Returns dict with shape:
       {round: 'regional'|'super'|'cws', regional_key?: str, super_key?: str, ...}
    """
    comp = event.get("competitions", [{}])[0]
    comps = comp.get("competitors", [])
    if len(comps) != 2:
        return None
    team_a = to_canonical(comps[0]["team"].get("shortDisplayName", ""))
    team_b = to_canonical(comps[1]["team"].get("shortDisplayName", ""))

    if game_date in REGIONAL_DATES:
        ra = team_to_regional.get(team_a)
        rb = team_to_regional.get(team_b)
        if ra and ra == rb:
            return {"round": "regional", "regional_key": ra, "team_a": team_a, "team_b": team_b}
        return None
    if game_date in SUPER_DATES:
        # Both teams need to be tournament teams — they should still be in
        # team_to_regional even if they're at a super regional now.
        if team_a in team_to_regional and team_b in team_to_regional:
            return {"round": "super", "team_a": team_a, "team_b": team_b}
        return None
    if game_date in CWS_DATES:
        if team_a in team_to_regional and team_b in team_to_regional:
            return {"round": "cws", "team_a": team_a, "team_b": team_b}
        return None
    return None


def super_key_for_teams(team_a: str, team_b: str, bracket: dict,
                        team_to_regional: dict[str, str]) -> str | None:
    """Find which super_N pairing matches both teams' regionals."""
    ra = team_to_regional.get(team_a)
    rb = team_to_regional.get(team_b)
    if not ra or not rb: return None
    for i, pair in enumerate(bracket["super_pairs"], 1):
        if {pair["a"], pair["b"]} == {ra, rb}:
            return f"super_{i}"
    return None


# ----------------------------------------------------------------------------
# Schedule update
# ----------------------------------------------------------------------------
DEFAULT_REGIONAL_SCHEDULE = {
    "venue": "TBD",
    "games": [
        {"label": "Game 1", "datetime": "TBD", "network": None, "matchup_pattern": "1v4"},
        {"label": "Game 2", "datetime": "TBD", "network": None, "matchup_pattern": "2v3"},
        {"label": "Game 3", "datetime": "TBD", "network": None, "matchup_pattern": "L1vL2"},
        {"label": "Game 4", "datetime": "TBD", "network": None, "matchup_pattern": "W1vW2"},
        {"label": "Game 5", "datetime": "TBD", "network": None, "matchup_pattern": "L4vW3"},
        {"label": "Game 6", "datetime": "TBD", "network": None, "matchup_pattern": "W4vW5"},
        {"label": "Game 7 (if necessary)", "datetime": "TBD", "network": None, "matchup_pattern": "rematch"},
    ],
}


def update_regional_schedule(schedule: dict, regional_key: str,
                              event: dict, classify: dict) -> bool:
    """Update schedule.json[regional][regional_key] with a single ESPN event.
    Returns True if any change was made."""
    sched = schedule.setdefault("regional", {})
    if regional_key not in sched:
        sched[regional_key] = json.loads(json.dumps(DEFAULT_REGIONAL_SCHEDULE))

    comp = event["competitions"][0]
    venue = (comp.get("venue") or {}).get("fullName") or sched[regional_key].get("venue")
    networks = [b for br in comp.get("broadcasts", []) for b in br.get("names", [])]
    network = " · ".join(networks) if networks else None
    game_dt = parse_iso(event["date"])
    dt_str = fmt_local(game_dt)

    # Match this game to a Game N slot.  Use the date + which teams are playing.
    games = sched[regional_key]["games"]
    game_date = game_dt.date()
    changed = False

    if game_date == date(2026, 5, 29):
        # Day 1: Game 1 (1v4) or Game 2 (2v3). Look up team seeds.
        # We need bracket data, so this happens in the caller.
        # Here we just match by which game's matchup_pattern this is.
        # The classify dict has team_a/team_b; lookup is done outside.
        pass

    # For now, simple date-bucket assignment (one Friday game becomes G1 or G2,
    # one Saturday becomes G3 or G4, etc.). The caller passes us an index.
    idx = classify.get("game_index")
    if idx is None or not (0 <= idx < len(games)):
        return False

    g = games[idx]
    if g.get("datetime") != dt_str:
        g["datetime"] = dt_str; changed = True
    if g.get("network") != network:
        g["network"] = network; changed = True

    venue_str = (comp.get("venue") or {}).get("fullName")
    if venue_str and venue_str != "TBD":
        full_venue = venue_str
        city = (comp.get("venue") or {}).get("address", {}).get("city", "")
        st   = (comp.get("venue") or {}).get("address", {}).get("state", "")
        if city or st:
            full_venue = f"{venue_str} · {city}{', ' + st if st else ''}"
        if sched[regional_key].get("venue") != full_venue:
            sched[regional_key]["venue"] = full_venue
            changed = True
    return changed


# ----------------------------------------------------------------------------
# Results update
# ----------------------------------------------------------------------------
def update_results(results: dict, bracket: dict, team_to_regional: dict[str, str],
                   all_events: list[dict]) -> bool:
    """Look at every FINAL game and infer regional/super/cws/champion winners.

    Heuristic:
      - Regional winner = last FINAL game in that regional's date range, whose
        winner has the most W's in the regional (=> the team that survived).
      - Super winner   = team that wins the super-regional series (best-of-3 first to 2).
      - CWS bracket winner = team that wins the CWS double-elim bracket.
      - Champion       = winner of the last CWS finals game.
    """
    changed = False
    regional_wins: dict[str, dict[str, int]] = defaultdict(lambda: defaultdict(int))
    regional_last_dt: dict[str, datetime] = {}
    regional_last_winner: dict[str, str] = {}

    super_wins: dict[str, dict[str, int]] = defaultdict(lambda: defaultdict(int))
    cws_wins:   dict[str, int] = defaultdict(int)
    cws_finals_last_winner: tuple[datetime, str] | None = None
    cws_finals_winner_loss: dict[str, int] = defaultdict(int)

    for ev in all_events:
        status = ev.get("status", {}).get("type", {}).get("name", "")
        if status != "STATUS_FINAL":
            continue
        comp = ev["competitions"][0]
        comps = comp.get("competitors", [])
        if len(comps) != 2:
            continue
        winner = None
        for c in comps:
            if c.get("winner"):
                winner = to_canonical(c["team"].get("shortDisplayName", ""))
        if not winner:
            continue
        team_a = to_canonical(comps[0]["team"].get("shortDisplayName", ""))
        team_b = to_canonical(comps[1]["team"].get("shortDisplayName", ""))
        game_dt = parse_iso(ev["date"])
        gd = game_dt.date()

        if gd in REGIONAL_DATES:
            ra = team_to_regional.get(team_a)
            rb = team_to_regional.get(team_b)
            if ra and ra == rb:
                regional_wins[ra][winner] += 1
                if ra not in regional_last_dt or game_dt > regional_last_dt[ra]:
                    regional_last_dt[ra] = game_dt
                    regional_last_winner[ra] = winner
        elif gd in SUPER_DATES:
            sk = super_key_for_teams(team_a, team_b, bracket, team_to_regional)
            if sk:
                super_wins[sk][winner] += 1
        elif gd in CWS_DATES:
            # CWS uses two double-elim brackets feeding the finals.  Simplification:
            #   - Last 3 games in CWS_DATES (finals) determine champion
            #   - Everything before that contributes to bracket wins
            # We don't try to split bracket1/bracket2 from ESPN data alone — too
            # fragile.  Users still see the per-bracket pick on the leaderboard;
            # we just don't auto-set the bracket-winner fields.
            cws_wins[winner] += 1
            if cws_finals_last_winner is None or game_dt > cws_finals_last_winner[0]:
                cws_finals_last_winner = (game_dt, winner)

    # Regional winners — only commit once all 4 of a regional's days have passed
    today = date.today()
    for rk, last_dt in regional_last_dt.items():
        if last_dt.date() < today or (last_dt.date() == today and last_dt.hour < 12):
            # Heuristic: if a team has 3 wins in the regional, they've won.  Otherwise
            # use the last-game winner as the working assumption.
            wins = regional_wins[rk]
            top = max(wins.items(), key=lambda kv: kv[1], default=(None, 0))
            if top[1] >= 3:
                if results["regional"].get(rk) != top[0]:
                    results["regional"][rk] = top[0]
                    changed = True

    # Super winners — first team to 2 wins in the series
    for sk, wins in super_wins.items():
        top = max(wins.items(), key=lambda kv: kv[1], default=(None, 0))
        if top[1] >= 2:
            if results["super"].get(sk) != top[0]:
                results["super"][sk] = top[0]
                changed = True

    # Champion (best guess: last CWS-finals game's winner, if 2+ CWS-window wins)
    if cws_finals_last_winner and cws_wins.get(cws_finals_last_winner[1], 0) >= 2:
        ch = cws_finals_last_winner[1]
        if results.get("champion") != ch:
            results["champion"] = ch
            changed = True

    if changed:
        results["last_updated"] = datetime.now().strftime("%Y-%m-%d")
    return changed


# ----------------------------------------------------------------------------
# Main
# ----------------------------------------------------------------------------
def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--dry-run", action="store_true")
    parser.add_argument("--start", type=str, help="YYYY-MM-DD, defaults to TOURNAMENT_START")
    parser.add_argument("--end",   type=str, help="YYYY-MM-DD, defaults to TOURNAMENT_END")
    parser.add_argument("--force", action="store_true",
                        help="Run even outside the tournament window (for testing).")
    args = parser.parse_args()

    start = date.fromisoformat(args.start) if args.start else TOURNAMENT_START
    end   = date.fromisoformat(args.end)   if args.end   else TOURNAMENT_END
    today = date.today()
    if not args.force and (today < start - timedelta(days=1) or today > end + timedelta(days=1)):
        log(f"Outside tournament window ({start} to {end}). Exiting cleanly. (use --force to override)")
        return 0

    bracket  = load_json(BRACKET)
    schedule = load_json(SCHEDULE) if SCHEDULE.exists() else {"regional": {}, "super": {}, "cws": {}}
    results  = load_json(RESULTS)  if RESULTS.exists()  else {"regional": {}, "super": {}, "cwsBracket": {}, "champion": None}
    team_to_regional = build_team_to_regional(bracket)

    # Fetch every date in the window
    all_events = []
    d = start
    while d <= end:
        log(f"Fetching {d}...")
        events = fetch_date(d)
        log(f"  got {len(events)} events")
        all_events.extend(events)
        d += timedelta(days=1)

    # ---- Schedule updates ----
    # Group regional events by (regional_key, date), assign Game N indices
    regional_by_key_date: dict[tuple[str, date], list[dict]] = defaultdict(list)
    for ev in all_events:
        try:
            gd = parse_iso(ev["date"]).date()
        except Exception:
            continue
        cls = classify_game(ev, team_to_regional, gd)
        if not cls: continue
        if cls["round"] == "regional":
            regional_by_key_date[(cls["regional_key"], gd)].append(ev)

    sched_changed = False
    # Assignment heuristic: Day 1 (May 29) -> G1, G2 by seed pairing;
    # Day 2 (May 30) -> G3 (elim, both teams lost Friday), G4 (winners final, both won Friday);
    # Day 3 (May 31) -> G5 (elim final), G6 (bracket final);
    # Day 4 (Jun 1)  -> G7.
    for (rkey, gd), evs in regional_by_key_date.items():
        # Friday: order by start time, pair to G1 (1v4) or G2 (2v3) by seeds.
        evs.sort(key=lambda e: e["date"])
        regional = next(r for r in bracket["regionals"] if r["key"] == rkey)
        seed_of = {t["team"]: t["regional_seed"] for t in regional["teams"]}
        if gd == date(2026, 5, 29):
            for ev in evs:
                comps = ev["competitions"][0]["competitors"]
                seeds = sorted([seed_of.get(to_canonical(c["team"]["shortDisplayName"]), 0) for c in comps])
                idx = 0 if seeds == [1, 4] else 1 if seeds == [2, 3] else None
                if idx is None: continue
                if update_regional_schedule(schedule, rkey, ev, {"game_index": idx}):
                    sched_changed = True
        elif gd == date(2026, 5, 30):
            # G3 elim (idx 2) before G4 winners (idx 3), but order may vary by site
            for i, ev in enumerate(evs):
                idx = 2 + i  # naive ordering by start time
                if update_regional_schedule(schedule, rkey, ev, {"game_index": idx}):
                    sched_changed = True
        elif gd == date(2026, 5, 31):
            for i, ev in enumerate(evs):
                idx = 4 + i
                if update_regional_schedule(schedule, rkey, ev, {"game_index": idx}):
                    sched_changed = True
        elif gd == date(2026, 6, 1):
            for ev in evs:
                if update_regional_schedule(schedule, rkey, ev, {"game_index": 6}):
                    sched_changed = True

    # ---- Results updates ----
    res_changed = update_results(results, bracket, team_to_regional, all_events)

    # ---- Write ----
    if sched_changed:
        if "_comment" not in schedule:
            schedule["_comment"] = "Auto-updated by pipeline/update_tournament.py from ESPN scoreboard. Manual edits will be overwritten."
        write_json_if_changed(SCHEDULE, schedule, args.dry_run)
    else:
        log("schedule.json: no changes")
    if res_changed:
        write_json_if_changed(RESULTS, results, args.dry_run)
    else:
        log("results.json: no changes")

    log(f"Done. {len(all_events)} total events scanned.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
