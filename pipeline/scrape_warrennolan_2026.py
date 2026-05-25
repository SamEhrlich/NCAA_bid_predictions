#!/usr/bin/env python3
"""Scrape 2026 D1 baseball schedules from warrennolan.com.

Polite, resumable scraper for warrennolan.com/baseball/2026/schedule/<slug>.

- Reads wn_teams_2026.csv for the team list.
- Writes per-team CSV to wn_2026_raw/<slug>.csv. Skips files that already exist.
- 3-5s randomized delay between requests.
- Aborts hard on 403/429 to avoid getting banned.
- Logs progress to scrape_warrennolan_2026.log.

Run from the ncaa_tournament_elo directory.
"""

from __future__ import annotations

import csv
import random
import re
import sys
import time
from datetime import datetime
from pathlib import Path

import requests
from bs4 import BeautifulSoup

ROOT = Path(__file__).resolve().parent
TEAMS_CSV = ROOT / "wn_teams_2026.csv"
OUT_DIR = ROOT / "wn_2026_raw"
LOG_PATH = ROOT / "scrape_warrennolan_2026.log"
AGG_PATH = ROOT / "d1_sched_2026_wn_raw.csv"

OUT_DIR.mkdir(exist_ok=True)

UA = {
    "User-Agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 "
                  "(KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
    "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    "Accept-Language": "en-US,en;q=0.9",
}

# Conservative pacing
DELAY_MIN = 3.0
DELAY_MAX = 5.0
REQUEST_TIMEOUT = 20

# Abort thresholds — if we see a sustained block, stop
ABORT_AFTER_BLOCKS = 3       # consecutive 403/429 → abort
ABORT_AFTER_EMPTIES = 10     # consecutive parse-failures → abort

# Month abbreviations Warren Nolan uses
MONTHS = {m: i for i, m in enumerate(
    ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'], 1)}

# Default to 2026
SEASON_YEAR = 2026
# Baseball season runs Feb-Jun, so any month gets the season year
# (no year-rollover edge cases)


def log(msg: str) -> None:
    line = f"[{datetime.now().strftime('%H:%M:%S')}] {msg}"
    print(line, flush=True)
    with LOG_PATH.open("a") as f:
        f.write(line + "\n")


def parse_schedule(html: str, team_wn_name: str, team_wn_slug: str) -> list[dict]:
    """Return one dict per game played (W/L outcome present)."""
    soup = BeautifulSoup(html, "html.parser")
    games_out: list[dict] = []
    games = soup.select("li.team-schedule")

    for li in games:
        # Date: three spans (month/day/dow)
        date_box = li.select_one(".team-schedule__game-date")
        if not date_box:
            continue
        month_el = date_box.select_one(".team-schedule__game-date--month")
        day_el   = date_box.select_one(".team-schedule__game-date--day")
        if not (month_el and day_el):
            continue
        mon = (month_el.get_text(strip=True) or "").upper()
        day = (day_el.get_text(strip=True) or "").strip()
        if mon not in MONTHS or not day.isdigit():
            continue
        try:
            game_date = datetime(SEASON_YEAR, MONTHS[mon], int(day)).date()
        except ValueError:
            continue

        # Opponent: text + slug
        opp_a = li.select_one(".team-schedule__opp-line-link")
        if not opp_a:
            continue
        opp_name = opp_a.get_text(strip=True)
        opp_href = opp_a.get("href", "")
        opp_slug = opp_href.rsplit("/", 1)[-1] if opp_href else ""

        # Location marker — "" home, "AT" away, "VS" neutral
        loc_el = li.select_one(".team-schedule__location")
        loc_marker = (loc_el.get_text(strip=True) if loc_el else "") or ""
        loc_marker = loc_marker.upper()

        info_el = li.select_one(".team-schedule__info")
        venue = info_el.get_text(" / ", strip=True) if info_el else ""

        # Result: <span class="--win">W</span>  8 - 4  (possibly " (8 Innings)")
        res_el = li.select_one(".team-schedule__result")
        if not res_el:
            continue
        win_span = res_el.select_one(".team-schedule__result--win")
        loss_span = res_el.select_one(".team-schedule__result--loss")
        if win_span:
            outcome = "W"
        elif loss_span:
            outcome = "L"
        else:
            # Game not played yet (no W/L marker) — skip
            continue

        # Extract scores from the result text
        result_text = res_el.get_text(" ", strip=True)
        m = re.search(r"(\d+)\s*-\s*(\d+)", result_text)
        if not m:
            continue
        team_score = int(m.group(1))
        opp_score = int(m.group(2))

        # Determine home/away/neutral and assign home/away teams
        if loc_marker == "":
            # home game
            home_name, away_name = team_wn_name, opp_name
            home_slug, away_slug = team_wn_slug, opp_slug
            home_score, away_score = team_score, opp_score
            neutral_site = 0
            is_away = 0
        elif loc_marker == "AT":
            # away game
            home_name, away_name = opp_name, team_wn_name
            home_slug, away_slug = opp_slug, team_wn_slug
            home_score, away_score = opp_score, team_score
            neutral_site = 0
            is_away = 1
        elif loc_marker == "VS":
            # neutral site — assign team as away by convention (arbitrary but stable)
            home_name, away_name = opp_name, team_wn_name
            home_slug, away_slug = opp_slug, team_wn_slug
            home_score, away_score = opp_score, team_score
            neutral_site = 1
            is_away = 0
        else:
            log(f"  UNKNOWN location marker: {loc_marker!r} (game {game_date} {opp_name})")
            continue

        # Detect "(N Innings)" extra-innings note
        innings_m = re.search(r"\((\d+)\s+Innings?\)", result_text)
        innings = int(innings_m.group(1)) if innings_m else 9

        # Game result from THE SCRAPED TEAM's perspective (so it joins like the R cached schema)
        team_won = (team_score > opp_score)
        # game_result: W/L from the scraped team's POV — that's what R's "game_result" column holds
        # (R's clean_schedule_data takes "Result" prefix W/L)
        team_game_result = "W" if team_won else "L"

        games_out.append({
            "Date":              game_date.strftime("%m/%d/%Y"),
            "home_team":         home_name,
            "away_team":         away_name,
            "home_score":        home_score,
            "away_score":        away_score,
            "doubleheader_game": 0,  # WN doesn't expose this; default 0
            "neutral_site":      neutral_site,
            "game_result":       team_game_result,
            "team_name":         team_wn_name,
            "team_slug":         team_wn_slug,
            "opp_slug":          opp_slug,
            "is_away":           is_away,
            "innings":           innings,
            "venue":             venue,
            "year":              SEASON_YEAR,
            "season_id":         f"WN{SEASON_YEAR}",  # placeholder
        })

    return games_out


def scrape_one(session: requests.Session, wn_name: str, wn_slug: str) -> tuple[str, int]:
    """Returns (status_label, n_games). status_label is 'ok' | 'block' | 'empty' | 'err'."""
    url = f"https://www.warrennolan.com/baseball/2026/schedule/{wn_slug}"
    try:
        r = session.get(url, timeout=REQUEST_TIMEOUT, headers=UA)
    except requests.RequestException as e:
        log(f"  ERR {wn_name}: {e}")
        return ("err", 0)

    if r.status_code in (403, 429):
        log(f"  BLOCK {r.status_code} on {wn_name} — site may be rate-limiting us")
        return ("block", 0)
    if r.status_code != 200:
        log(f"  HTTP {r.status_code} on {wn_name}")
        return ("err", 0)

    games = parse_schedule(r.text, wn_name, wn_slug)
    out_path = OUT_DIR / f"{wn_slug}.csv"
    if not games:
        # Touch empty marker so we don't retry
        out_path.write_text("")
        return ("empty", 0)

    fieldnames = list(games[0].keys())
    with out_path.open("w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=fieldnames)
        w.writeheader()
        w.writerows(games)
    return ("ok", len(games))


def main() -> int:
    with TEAMS_CSV.open() as f:
        teams = list(csv.DictReader(f))
    log(f"Loaded {len(teams)} teams from {TEAMS_CSV.name}")

    session = requests.Session()
    session.headers.update(UA)

    consec_blocks = 0
    consec_empties = 0
    ok_ct = 0
    skipped_ct = 0
    total_games = 0
    started = time.time()

    for i, t in enumerate(teams, 1):
        wn_name, wn_slug = t["wn_name"], t["wn_slug"]
        out_path = OUT_DIR / f"{wn_slug}.csv"
        if out_path.exists() and out_path.stat().st_size > 0:
            skipped_ct += 1
            continue

        log(f"[{i}/{len(teams)}] {wn_name} ({wn_slug})")
        status, n = scrape_one(session, wn_name, wn_slug)

        if status == "block":
            consec_blocks += 1
            if consec_blocks >= ABORT_AFTER_BLOCKS:
                log(f"ABORT — {consec_blocks} consecutive blocks. Stopping early.")
                return 2
            # Back off hard before next try
            time.sleep(30)
            continue
        consec_blocks = 0

        if status == "empty":
            consec_empties += 1
            if consec_empties >= ABORT_AFTER_EMPTIES:
                log(f"ABORT — {consec_empties} consecutive empty results. Parser likely broken or site changed.")
                return 2
        else:
            consec_empties = 0

        if status == "ok":
            ok_ct += 1
            total_games += n
            log(f"  ✓ {n} games")

        # Polite delay
        time.sleep(random.uniform(DELAY_MIN, DELAY_MAX))

        if i % 25 == 0:
            elapsed_min = (time.time() - started) / 60
            log(f"  ... {i}/{len(teams)} (ok={ok_ct} skipped={skipped_ct} games={total_games}) {elapsed_min:.1f}min elapsed")

    log(f"Scrape complete. ok={ok_ct} skipped={skipped_ct} games={total_games}")

    # Aggregate
    log("Aggregating per-team CSVs...")
    out_rows = []
    cols = None
    for p in sorted(OUT_DIR.glob("*.csv")):
        if p.stat().st_size == 0:
            continue
        with p.open() as f:
            rdr = csv.DictReader(f)
            for row in rdr:
                if cols is None:
                    cols = list(row.keys())
                out_rows.append(row)
    with AGG_PATH.open("w", newline="") as f:
        if cols:
            w = csv.DictWriter(f, fieldnames=cols)
            w.writeheader()
            w.writerows(out_rows)
    log(f"Wrote {AGG_PATH.name} with {len(out_rows):,} rows (raw — each game appears twice, once per team)")
    log("DONE")
    return 0


if __name__ == "__main__":
    sys.exit(main())
