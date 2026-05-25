#!/usr/bin/env python3
"""Export site-ready JSON artifacts from the 21-26 combined schedule.

Reads:
  - d1_sched_2126_combined.csv  (joined cached + 2026 WN scrape)
  - wn_to_ncaa_name_map.py      (for the reverse map: NCAA name -> WN slug for logos)
  - wn_teams_2026.csv           (WN slug list)

Writes (to site/data/):
  - teams.json                    list of all teams w/ current Elo, conf, record, rank, logo, color
  - elo_history/<slug>.json       per-team game-by-game Elo timeline
  - bracket.json                  16 regionals × 4 teams w/ seeds + super-regional pairings
  - odds.json                     64 bracket teams × 4 stages (regional / CWS / finals / champion)
  - conference_elo.json           per-conference avg Elo trajectory
  - calibration.json              predicted-vs-actual win rate by bucket (model credibility)
  - upsets.json                   bracket matchups where seed gap and Elo gap disagree
  - meta.json                     timestamp, fit diagnostics, sim count

All JSONs are gzip-friendly. Total payload < 10 MB uncompressed.
"""
from __future__ import annotations

import csv
import json
import sys
from collections import defaultdict
from datetime import datetime
from pathlib import Path

import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parent
SITE = ROOT / "docs"    # GitHub Pages requires source path to be / or /docs
DATA = SITE / "data"
HIST = DATA / "elo_history"
for d in (SITE, DATA, HIST):
    d.mkdir(parents=True, exist_ok=True)

sys.path.insert(0, str(ROOT))
from wn_to_ncaa_name_map import WN_TO_NCAA

# Reverse map: NCAA canonical name -> WN slug (used for the logo URL)
NCAA_TO_WN_NAME: dict[str, str] = {v: k for k, v in WN_TO_NCAA.items()}
# Read the WN team list to map WN name -> slug
_WN_NAME_TO_SLUG: dict[str, str] = {}
with (ROOT / "wn_teams_2026.csv").open() as f:
    for row in csv.DictReader(f):
        _WN_NAME_TO_SLUG[row["wn_name"]] = row["wn_slug"]


def ncaa_to_wn_slug(ncaa_name: str) -> str | None:
    """Best-effort: map a canonical NCAA name to its WN slug (for the WN logo URL)."""
    wn_name = NCAA_TO_WN_NAME.get(ncaa_name, ncaa_name)
    return _WN_NAME_TO_SLUG.get(wn_name)


# ---------------------------------------------------------------------------
# Team colors. Starter pack for the 64 bracket teams + a sampling of others.
# Fallback is Driveline orange-on-dark; user can extend over time.
# Source: school brand guides / Wikipedia infoboxes (primary athletic color).
# ---------------------------------------------------------------------------
TEAM_COLORS: dict[str, str] = {
    # 2026 top 8 seeds
    "UCLA":                 "#2D68C4",
    "Georgia Tech":         "#B3A369",
    "Georgia":              "#BA0C2F",
    "Auburn":               "#0C2340",
    "North Carolina":       "#7BAFD4",
    "Texas":                "#BF5700",
    "Alabama":              "#9E1B32",
    "Florida":              "#0021A5",
    # 2026 seeds 9-16
    "Southern Miss.":       "#FFC72C",
    "Florida St.":          "#782F40",
    "Oregon":               "#154733",
    "Texas A&M":            "#500000",
    "Nebraska":             "#E41C38",
    "Mississippi St.":      "#660000",
    "Kansas":               "#0051BA",
    "West Virginia":        "#002855",
    # Other 2026 bracket teams
    "Virginia Tech":        "#630031",
    "Cal Poly":             "#154734",
    "Saint Mary's (CA)":    "#06315B",
    "Oklahoma":             "#841617",
    "The Citadel":          "#003366",
    "UIC":                  "#D50032",
    "Boston College":       "#8A100B",
    "Liberty":              "#990000",
    "LIU":                  "#005826",
    "UCF":                  "#000000",
    "NC State":             "#CC0000",
    "Milwaukee":            "#FFCC00",
    "Tennessee":            "#FF8200",
    "East Carolina":        "#592A8A",
    "VCU":                  "#000000",
    "UC Santa Barbara":     "#003660",
    "Tarleton St.":         "#4A1F8C",
    "Holy Cross":           "#522D80",
    "Oklahoma St.":         "#FF7300",
    "USC Upstate":          "#006A4D",
    "Alabama St.":          "#000000",
    "Miami (FL)":           "#F47321",
    "Troy":                 "#8A2432",
    "Rider":                "#943634",
    "Virginia":             "#232D4B",
    "Jacksonville St.":     "#A6192E",
    "Little Rock":          "#A41F35",
    "Coastal Carolina":     "#006F71",
    "NIU":                  "#CC0000",
    "St. John's (NY)":      "#BA0C2F",
    "Oregon St.":           "#DC4405",
    "Washington St.":       "#981E32",
    "Yale":                 "#0F4D92",
    "Southern California":  "#9D2235",
    "Texas St.":            "#501214",
    "Lamar University":     "#D2122E",
    "Ole Miss":             "#CE1126",
    "Arizona St.":          "#8C1D40",
    "South Dakota St.":     "#0033A0",
    "Cincinnati":           "#E00122",
    "Louisiana":            "#CE181E",
    "Lipscomb":             "#3A1B5D",
    "Arkansas":             "#9D2235",
    "Missouri St.":         "#660033",
    "Northeastern":         "#C8102E",
    "Wake Forest":          "#9E7E38",
    "Kentucky":             "#0033A0",
    "Binghamton":           "#005A43",
    # Notable non-tournament 2026 teams (so leaderboard looks alive)
    "Vanderbilt":           "#866D4B",
    "LSU":                  "#461D7C",
    "Arizona":              "#003366",
    "Stanford":             "#8C1515",
    "Duke":                 "#003087",
    "Clemson":              "#F56600",
    "Notre Dame":           "#0C2340",
    "Michigan":             "#00274C",
}
DEFAULT_COLOR = "#FFA300"  # Driveline orange fallback


def color_for(team: str) -> str:
    return TEAM_COLORS.get(team, DEFAULT_COLOR)


def logo_url(slug: str | None) -> str | None:
    if not slug:
        return None
    return f"https://www.warrennolan.com/images/team/new/80x80/{slug}.png"


# ---------------------------------------------------------------------------
# 1. Load + clean games (mirror notebook logic)
# ---------------------------------------------------------------------------
print("Loading combined schedule...")
combined = pd.read_csv(ROOT / "d1_sched_2126_combined.csv", low_memory=False)

def _r_str(x):
    if pd.isna(x): return ''
    if isinstance(x, float) and x.is_integer(): return str(int(x))
    return str(x)

games = combined[combined['game_result'].isin(['W','L'])].copy()
games = games[~((games['year']==2021) & (games['conference']=='Ivy League'))]
games['unique_id'] = (games['home_team'].apply(_r_str) + '_' +
                     games['away_team'].apply(_r_str) + '_' +
                     games['home_score'].apply(_r_str) + '_' +
                     games['away_score'].apply(_r_str) + '_' +
                     games['doubleheader_game'].apply(_r_str) + '_' +
                     games['season_id'].apply(_r_str))
games = games.drop_duplicates('unique_id', keep='first')
games['home_team_win'] = (games['home_score'].astype(float) > games['away_score'].astype(float)).astype(int)
games['Date'] = pd.to_datetime(games['Date'], format='%m/%d/%Y', errors='coerce')
games = games.dropna(subset=['Date']).sort_values('Date').reset_index(drop=True)
print(f"  {len(games):,} unique games, {games['year'].nunique()} seasons")

# ---------------------------------------------------------------------------
# 2. Fit Elo + capture per-game pre/post elo for everyone
# ---------------------------------------------------------------------------
print("Fitting Elo (k=29, h=40, r=0.07) and capturing per-game trajectory...")
K, HFA, REG = 29.0, 40.0, 0.07
all_teams = sorted(set(games['home_team']) | set(games['away_team']))
ratings = {t: 1500.0 for t in all_teams}
starting = dict(ratings)

home_arr   = games['home_team'].to_numpy()
away_arr   = games['away_team'].to_numpy()
neutral_arr= games['neutral_site'].to_numpy()
year_arr   = games['year'].to_numpy()
score_h_arr= games['home_score'].astype(float).to_numpy()
score_a_arr= games['away_score'].astype(float).to_numpy()
hw_arr     = games['home_team_win'].to_numpy()
date_arr   = games['Date'].dt.strftime('%Y-%m-%d').to_numpy()

# Per-team event log: list of dict per game touching that team
team_events: dict[str, list[dict]] = defaultdict(list)
prev_year = year_arr[0]
for i in range(len(games)):
    if year_arr[i] != prev_year:
        for t in ratings:
            ratings[t] += REG * (starting[t] - ratings[t])
        prev_year = year_arr[i]

    h, a = home_arr[i], away_arr[i]
    rh = ratings.setdefault(h, 1500.0)
    ra = ratings.setdefault(a, 1500.0)
    is_neutral = bool(neutral_arr[i])
    adj_h = rh + (0.0 if is_neutral else HFA)
    p_home = 1.0 / (1.0 + 10.0 ** ((ra - adj_h) / 400.0))

    outcome = hw_arr[i]
    new_rh = rh + K * (outcome - p_home)
    new_ra = ra + K * ((1 - outcome) - (1 - p_home))

    # Home team's perspective
    team_events[h].append({
        "date":     date_arr[i],
        "opp":      a,
        "site":     "neutral" if is_neutral else "home",
        "won":      int(outcome == 1),
        "score_for":     int(score_h_arr[i]),
        "score_against": int(score_a_arr[i]),
        "pre_elo":  round(rh, 1),
        "post_elo": round(new_rh, 1),
        "opp_pre_elo":   round(ra, 1),
        "win_prob_pre":  round(p_home, 4),
    })
    # Away team's perspective (mirror)
    team_events[a].append({
        "date":     date_arr[i],
        "opp":      h,
        "site":     "neutral" if is_neutral else "away",
        "won":      int(outcome == 0),
        "score_for":     int(score_a_arr[i]),
        "score_against": int(score_h_arr[i]),
        "pre_elo":  round(ra, 1),
        "post_elo": round(new_ra, 1),
        "opp_pre_elo":   round(rh, 1),
        "win_prob_pre":  round(1.0 - p_home, 4),
    })

    ratings[h] = new_rh
    ratings[a] = new_ra

print(f"  final Elo for {len(ratings)} teams")

# ---------------------------------------------------------------------------
# 3. teams.json — current ratings + metadata
# ---------------------------------------------------------------------------
def safe_slug(name: str) -> str:
    """URL-friendly slug for the team-detail page filename."""
    return (name.replace(' ','-').replace('.','').replace("'",'')
                .replace('(','').replace(')','').replace('&','and').replace('/','-'))

# Compute records from 2026 events only
def record_2026(team: str) -> tuple[int, int]:
    w = l = 0
    for ev in team_events[team]:
        if ev['date'][:4] != '2026': continue
        if ev['won']: w += 1
        else: l += 1
    return w, l

# Most recent conference per team (from combined data)
conf_lookup = (combined[['team_name','conference']]
               .dropna()
               .drop_duplicates('team_name', keep='last')
               .set_index('team_name')['conference']
               .to_dict())

teams_payload = []
for rank, (team, elo) in enumerate(sorted(ratings.items(), key=lambda kv: -kv[1]), 1):
    w, l = record_2026(team)
    slug = safe_slug(team)
    wn_slug = ncaa_to_wn_slug(team)
    teams_payload.append({
        "rank":       rank,
        "team":       team,
        "slug":       slug,
        "conf":       conf_lookup.get(team),
        "elo":        round(elo, 1),
        "wins_2026":  w,
        "losses_2026": l,
        "color":      color_for(team),
        "logo":       logo_url(wn_slug),
        "wn_slug":    wn_slug,
        "games_2026": w + l,
    })

(DATA / "teams.json").write_text(json.dumps(teams_payload, separators=(',', ':')))
print(f"  wrote teams.json ({len(teams_payload)} teams)")

# ---------------------------------------------------------------------------
# 4. elo_history/<slug>.json — per-team game log
# ---------------------------------------------------------------------------
print("Writing per-team Elo histories...")
for team, events in team_events.items():
    slug = safe_slug(team)
    payload = {
        "team":   team,
        "slug":   slug,
        "color":  color_for(team),
        "logo":   logo_url(ncaa_to_wn_slug(team)),
        "final_elo": round(ratings[team], 1),
        "events": events,  # already in chronological order
    }
    (HIST / f"{slug}.json").write_text(json.dumps(payload, separators=(',', ':')))
print(f"  wrote {len(team_events)} elo-history files")

# ---------------------------------------------------------------------------
# 5. bracket.json — the 2026 bracket
# ---------------------------------------------------------------------------
BRACKET_NAME_MAP = {
    "Saint Mary's CA":"Saint Mary's (CA)", "Miami FL":"Miami (FL)", "St. John's NY":"St. John's (NY)",
    "Tarleton State":"Tarleton St.", "Oklahoma State":"Oklahoma St.", "Alabama State":"Alabama St.",
    "Southern Mississippi":"Southern Miss.", "Jacksonville State":"Jacksonville St.",
    "Florida State":"Florida St.", "Oregon State":"Oregon St.", "Washington State":"Washington St.",
    "Texas State":"Texas St.", "Lamar University":"Lamar University", "Arizona State":"Arizona St.",
    "South Dakota State":"South Dakota St.", "Mississippi State":"Mississippi St.",
    "Missouri State":"Missouri St.", "Northern Illinois":"NIU",
}
def res(n): return BRACKET_NAME_MAP.get(n, n)

REGIONALS = {
    'los_angeles':     dict(seed=1, name="Los Angeles", host='UCLA',                  teams=['UCLA','Virginia Tech','Cal Poly',"Saint Mary's CA"]),
    'atlanta':         dict(seed=2, name="Atlanta",     host='Georgia Tech',          teams=['Georgia Tech','Oklahoma','The Citadel','UIC']),
    'athens':          dict(seed=3, name="Athens",      host='Georgia',               teams=['Georgia','Boston College','Liberty','LIU']),
    'auburn':          dict(seed=4, name="Auburn",      host='Auburn',                teams=['Auburn','UCF','NC State','Milwaukee']),
    'chapel_hill':     dict(seed=5, name="Chapel Hill", host='North Carolina',        teams=['North Carolina','Tennessee','East Carolina','VCU']),
    'austin':          dict(seed=6, name="Austin",      host='Texas',                 teams=['Texas','UC Santa Barbara','Tarleton State','Holy Cross']),
    'tuscaloosa':      dict(seed=7, name="Tuscaloosa",  host='Alabama',               teams=['Alabama','Oklahoma State','USC Upstate','Alabama State']),
    'gainesville':     dict(seed=8, name="Gainesville", host='Florida',               teams=['Florida','Miami FL','Troy','Rider']),
    'hattiesburg':     dict(seed=9, name="Hattiesburg", host='Southern Mississippi',  teams=['Southern Mississippi','Virginia','Jacksonville State','Little Rock']),
    'tallahassee':     dict(seed=10,name="Tallahassee", host='Florida State',         teams=['Florida State','Coastal Carolina','Northern Illinois',"St. John's NY"]),
    'eugene':          dict(seed=11,name="Eugene",      host='Oregon',                teams=['Oregon','Oregon State','Washington State','Yale']),
    'college_station': dict(seed=12,name="College Station", host='Texas A&M',         teams=['Texas A&M','Southern California','Texas State','Lamar University']),
    'lincoln':         dict(seed=13,name="Lincoln",     host='Nebraska',              teams=['Nebraska','Ole Miss','Arizona State','South Dakota State']),
    'starkville':      dict(seed=14,name="Starkville",  host='Mississippi State',     teams=['Mississippi State','Cincinnati','Louisiana','Lipscomb']),
    'lawrence':        dict(seed=15,name="Lawrence",    host='Kansas',                teams=['Kansas','Arkansas','Missouri State','Northeastern']),
    'morgantown':      dict(seed=16,name="Morgantown",  host='West Virginia',         teams=['West Virginia','Wake Forest','Kentucky','Binghamton']),
}
for nm, r in REGIONALS.items():
    r['host'] = res(r['host'])
    r['teams'] = [res(t) for t in r['teams']]

# Sanity check ratings coverage
all_bracket = {t for r in REGIONALS.values() for t in r['teams']}
missing = all_bracket - set(ratings)
if missing:
    raise SystemExit(f"Bracket teams missing from ratings: {missing}")

SEED2R = {r['seed']: nm for nm, r in REGIONALS.items()}
SUPER_PAIRS = [(SEED2R[i], SEED2R[17-i]) for i in range(1, 9)]

bracket_payload = {
    "regionals": [
        {
            "key": nm,
            "name": r['name'],
            "seed": r['seed'],
            "host": r['host'],
            "teams": [
                {
                    "team": t,
                    "slug": safe_slug(t),
                    "elo": round(ratings[t], 1),
                    "color": color_for(t),
                    "logo": logo_url(ncaa_to_wn_slug(t)),
                    "regional_seed": idx + 1,  # 1=host, 4=lowest seed
                }
                for idx, t in enumerate(r['teams'])
            ],
        }
        for nm, r in REGIONALS.items()
    ],
    "super_pairs": [{"a": a, "b": b} for a, b in SUPER_PAIRS],
}
(DATA / "bracket.json").write_text(json.dumps(bracket_payload, separators=(',', ':')))
print("  wrote bracket.json")

# ---------------------------------------------------------------------------
# 6. Run multi-stage simulation → odds.json
# ---------------------------------------------------------------------------
print("Running 20k tournament simulations w/ multi-stage tracking...")
N_SIMS = 20000
rng = np.random.default_rng(42)

def sim_game(t1, t2):
    p = 1.0 / (1.0 + 10.0 ** ((ratings[t2] - ratings[t1]) / 400.0))
    return t1 if rng.random() < p else t2

def sim_regional(teams):
    g1 = sim_game(teams[0], teams[3]); g2 = sim_game(teams[1], teams[2])
    g1l = teams[3] if g1==teams[0] else teams[0]; g2l = teams[2] if g2==teams[1] else teams[1]
    wbf = sim_game(g1, g2); wbfl = g2 if wbf==g1 else g1
    lg1 = sim_game(g1l, g2l); lbf = sim_game(wbfl, lg1)
    f1 = sim_game(wbf, lbf)
    return sim_game(wbf, lbf) if f1==lbf else f1

reg_win_ct  = defaultdict(int)   # P(win regional)
cws_ct      = defaultdict(int)   # P(make CWS)
finals_ct   = defaultdict(int)   # P(reach CWS finals)
champ_ct    = defaultdict(int)   # P(win championship)

for _ in range(N_SIMS):
    # Regionals
    reg_winners = {nm: sim_regional(r['teams']) for nm, r in REGIONALS.items()}
    for w in reg_winners.values():
        reg_win_ct[w] += 1
    # Super regionals
    cws_teams = [sim_game(reg_winners[a], reg_winners[b]) for a, b in SUPER_PAIRS]
    for t in cws_teams:
        cws_ct[t] += 1
    # CWS: two 4-team double-elim brackets, then single championship game (2025-style)
    b1 = sim_regional(cws_teams[:4])
    b2 = sim_regional(cws_teams[4:])
    for t in (b1, b2):
        finals_ct[t] += 1
    champ = sim_game(b1, b2)
    champ_ct[champ] += 1

odds_payload = []
for t in sorted(all_bracket):
    odds_payload.append({
        "team":   t,
        "slug":   safe_slug(t),
        "regional_pct":  reg_win_ct[t] / N_SIMS * 100,
        "cws_pct":       cws_ct[t]     / N_SIMS * 100,
        "finals_pct":    finals_ct[t]  / N_SIMS * 100,
        "champ_pct":     champ_ct[t]   / N_SIMS * 100,
    })
(DATA / "odds.json").write_text(json.dumps({"n_sims": N_SIMS, "teams": odds_payload},
                                            separators=(',', ':')))
print(f"  wrote odds.json ({len(odds_payload)} bracket teams, {N_SIMS} sims)")

# ---------------------------------------------------------------------------
# 7. conference_elo.json — average Elo per conference per season
# ---------------------------------------------------------------------------
print("Computing conference Elo trajectories...")
# Use each team's POST-event elo at season end as a snapshot per year
# Group by team's most recent conference (best approximation we have)
team_yearly_elo = defaultdict(dict)  # (team, year) -> last post_elo that year
for team, events in team_events.items():
    for ev in events:
        yr = int(ev['date'][:4])
        team_yearly_elo[team][yr] = ev['post_elo']

conf_yearly = defaultdict(lambda: defaultdict(list))
for team, year_map in team_yearly_elo.items():
    conf = conf_lookup.get(team)
    if not conf or pd.isna(conf): continue
    for yr, e in year_map.items():
        conf_yearly[conf][yr].append(e)

conf_payload = []
for conf, year_map in sorted(conf_yearly.items()):
    series = sorted(year_map.items())
    conf_payload.append({
        "conf":  conf,
        "years": [yr for yr, _ in series],
        "avg":   [round(float(np.mean(elos)), 1) for _, elos in series],
        "n":     [len(elos) for _, elos in series],
    })
(DATA / "conference_elo.json").write_text(json.dumps(conf_payload, separators=(',', ':')))
print(f"  wrote conference_elo.json ({len(conf_payload)} confs)")

# ---------------------------------------------------------------------------
# 7b. season_change.json — biggest gainers/falloffs from end of 2025 to end of 2026
# ---------------------------------------------------------------------------
print("Computing 2025 -> 2026 Elo deltas...")
season_changes = []
for team, year_map in team_yearly_elo.items():
    end_2025 = year_map.get(2025)
    end_2026 = year_map.get(2026)
    if end_2025 is None or end_2026 is None:
        continue  # team didn't play in both seasons
    season_changes.append({
        "team":     team,
        "slug":     safe_slug(team),
        "conf":     conf_lookup.get(team) if not pd.isna(conf_lookup.get(team, pd.NA)) else None,
        "color":    color_for(team),
        "logo":     logo_url(ncaa_to_wn_slug(team)),
        "end_2025": end_2025,
        "end_2026": end_2026,
        "delta":    round(end_2026 - end_2025, 1),
    })
# Top gainers (highest delta first), top falloffs (lowest delta first)
gainers = sorted(season_changes, key=lambda x: -x['delta'])
falloffs = sorted(season_changes, key=lambda x:  x['delta'])
(DATA / "season_change.json").write_text(json.dumps({
    "n_teams_compared": len(season_changes),
    "gainers": gainers,
    "falloffs": falloffs,
}, separators=(',', ':')))
print(f"  wrote season_change.json ({len(season_changes)} teams w/ both 2025 + 2026 data)")

# ---------------------------------------------------------------------------
# 8. calibration.json — predicted vs actual win-rate by probability bucket
# ---------------------------------------------------------------------------
print("Computing calibration buckets...")
# Re-fit baseline to get pre-game probs + actuals
ratings_c = {t: 1500.0 for t in all_teams}
starting_c = dict(ratings_c)
preds = []; acts = []
prev_year = year_arr[0]
for i in range(len(games)):
    if year_arr[i] != prev_year:
        for t in ratings_c: ratings_c[t] += REG * (starting_c[t] - ratings_c[t])
        prev_year = year_arr[i]
    h, a = home_arr[i], away_arr[i]
    rh = ratings_c.setdefault(h, 1500.0); ra = ratings_c.setdefault(a, 1500.0)
    adj_h = rh + (0.0 if neutral_arr[i] else HFA)
    p = 1.0/(1.0+10.0**((ra-adj_h)/400.0))
    preds.append(p); acts.append(hw_arr[i])
    ratings_c[h] = rh + K * (hw_arr[i] - p)
    ratings_c[a] = ra + K * ((1-hw_arr[i]) - (1-p))

preds = np.array(preds); acts = np.array(acts)
bins = np.linspace(0, 1, 11)
calib = []
for lo, hi in zip(bins[:-1], bins[1:]):
    mask = (preds >= lo) & (preds < hi if hi < 1 else preds <= hi)
    n = int(mask.sum())
    if n == 0:
        calib.append({"bin_low": round(float(lo),2), "bin_high": round(float(hi),2),
                      "predicted": None, "actual": None, "n": 0})
    else:
        calib.append({"bin_low": round(float(lo),2), "bin_high": round(float(hi),2),
                      "predicted": round(float(preds[mask].mean()),4),
                      "actual":    round(float(acts[mask].mean()),4),
                      "n": n})
(DATA / "calibration.json").write_text(json.dumps(calib, separators=(',', ':')))
print(f"  wrote calibration.json ({sum(b['n'] for b in calib)} games)")

# ---------------------------------------------------------------------------
# 9. upsets.json — bracket matchups where seed and Elo disagree
# ---------------------------------------------------------------------------
upsets = []
for nm, r in REGIONALS.items():
    teams = r['teams']
    # 1v4 and 2v3 first-round matchups in the regional
    for a, b in [(0,3), (1,2)]:
        ta, tb = teams[a], teams[b]
        # Lower regional_seed index = higher seed (better)
        higher_seed = ta  # by definition (since teams[0] is host #1, teams[1] is #2, etc.)
        elo_diff = ratings[ta] - ratings[tb]
        if elo_diff < 0:
            upsets.append({
                "regional": nm,
                "matchup": f"{ta} (seed {a+1}) vs {tb} (seed {b+1})",
                "higher_seed": ta, "higher_elo": tb,
                "elo_gap": round(abs(elo_diff), 1),
                "win_prob_lower_seed": round(1.0/(1.0+10.0**((ratings[ta]-ratings[tb])/400.0)), 3),
            })
upsets.sort(key=lambda u: -u['elo_gap'])
(DATA / "upsets.json").write_text(json.dumps(upsets, separators=(',', ':')))
print(f"  wrote upsets.json ({len(upsets)} bracket matchups w/ Elo disagreement)")

# ---------------------------------------------------------------------------
# 10. meta.json — global metadata
# ---------------------------------------------------------------------------
meta = {
    "generated_at": datetime.now().isoformat(timespec='seconds'),
    "season":       2026,
    "n_games_fit":  len(games),
    "elo_params":   {"k": K, "hfa": HFA, "regress": REG},
    "n_sims":       N_SIMS,
    "n_teams":      len(teams_payload),
    "n_bracket_teams": len(all_bracket),
}
(DATA / "meta.json").write_text(json.dumps(meta, separators=(',', ':'), indent=2))
print(f"  wrote meta.json")

print(f"\nAll site data written to {DATA}")
