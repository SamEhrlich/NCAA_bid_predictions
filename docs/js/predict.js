// Bracket prediction UI — NCAA tournament-tree layout.
//
// Layout (left half mirrors right):
//
//   ┌── Super-pair (8 pairs total) ──┐
//   │  ┌─2x2─Regional A──┐           │
//   │  │ T1   T2          │ ──┐      │
//   │  │ T3   T4          │   │       ┌─Super─┐
//   │  └──────────────────┘   ├──────►│ winA  │
//   │  ┌─2x2─Regional B──┐   │       │ winB  │
//   │  │ T1   T2          │ ──┘       └───────┘
//   │  │ T3   T4          │
//   │  └──────────────────┘
//   └────────────────────────────────┘
//
// Center column (top→bottom):
//   ┌───CHAMPION TROPHY───┐
//   │      (selected)      │
//   └──────────────────────┘
//   ┌──── CWS Finals (2x1) ──┐  ← pick from bracket1Winner vs bracket2Winner
//   │ B1 winner               │
//   │ B2 winner               │
//   └────────────────────────┘
//   ┌──── CWS Bracket 1 (2x2) ──┐   ← pick winner from supers 1,4,5,8
//   │ s1   s4                    │
//   │ s5   s8                    │
//   └────────────────────────────┘
//   ┌──── CWS Bracket 2 (2x2) ──┐   ← pick winner from supers 2,3,6,7
//   │ s2   s3                    │
//   │ s6   s7                    │
//   └────────────────────────────┘
//
// Picks: 16 regional + 8 super + 2 cws-bracket + 1 champion = 27 picks.
// (Spec said 25; we add the 2 CWS-bracket winners because they're individually
// pickable and necessary to feed into the finals.  These also go into the
// Google Form submission.)

import { loadJSON, fmtElo, makeLogoEl, setActiveNav, $ } from "./common.js";

// ============================================================================
// CONFIG — fill in after you set up the Google Form (4 fields only).
// See the setup checklist at the bottom of predict.html for instructions.
// ============================================================================
const CONFIG = {
  GOOGLE_FORM_ACTION_URL: "https://docs.google.com/forms/d/e/1FAIpQLSfR2KNmrK9l-IgNBtrWks-9cFYGG29xFF1ZQzXhsRZFr9YH2Q/formResponse",
  ENTRIES_CSV_URL:        "https://docs.google.com/spreadsheets/d/e/2PACX-1vRie2_C_-uEaI0Tgu_6zo06NkUPMo9LqZLeQhwuT9Wr3aG1tPN_Xtx-_fIk2azOVITHr5HLZ6UYHMRH/pub?gid=607709982&single=true&output=csv",
  FIELD_IDS: {
    name:     "entry.860814051",   // Name
    email:    "entry.1356704849",  // Email
    champion: "entry.1384293276",  // Champion
    bracket:  "entry.1288032880",  // Bracket (paragraph, full JSON payload)
  },
};

setActiveNav("predict");

const [bracket, meta, schedule, results] = await Promise.all([
  loadJSON("bracket.json"),
  loadJSON("meta.json"),
  loadJSON("schedule.json").catch(() => null),
  loadJSON("results.json").catch(() => null),
]);
$("#nav-meta").textContent =
  `${meta.n_games_fit.toLocaleString()} games · fit ${meta.generated_at.slice(0,10)}`;

// Make results available globally for both the bracket render AND the leaderboard
const RESULTS_AT_BOOT = results || { regional: {}, super: {}, cwsBracket: {}, champion: null };

const TEAM_BY_NAME = {};
for (const r of bracket.regionals) for (const t of r.teams) TEAM_BY_NAME[t.team] = t;
const REGIONAL_BY_KEY = Object.fromEntries(bracket.regionals.map(r => [r.key, r]));

// Super pairs in order super_1..super_8 (already 1v16, 2v15, ..., 8v9)
const SUPER_PAIRS = bracket.super_pairs.map((p, i) => ({
  key: `super_${i + 1}`,
  regionalA: p.a,
  regionalB: p.b,
}));

// Which supers feed which CWS bracket. Matches the simulation in
// pipeline/export_site_data.py (b1 = cws[0,3,4,7]; b2 = cws[1,2,5,6]) — i.e.
// supers 1,4,5,8 in bracket 1; supers 2,3,6,7 in bracket 2.
const BRACKET_SUPERS = {
  cws_bracket_1: ["super_1", "super_4", "super_5", "super_8"],
  cws_bracket_2: ["super_2", "super_3", "super_6", "super_7"],
};

// Visual placement: which supers go on the LEFT side of the screen, top→bottom.
// Pair them by bracket so paired regionals stack visually:
const LEFT_SUPER_KEYS  = ["super_1", "super_4", "super_5", "super_8"];   // bracket 1
const RIGHT_SUPER_KEYS = ["super_2", "super_3", "super_6", "super_7"];   // bracket 2

// ============================================================================
// STATE
// ============================================================================
const LS_KEY = "ncaa_predict_v2";
const state = {
  regional:    {},   // regional key → team name
  super:       {},   // super_N      → team name
  cwsBracket:  {},   // cws_bracket_1 / cws_bracket_2 → team name
  champion:    null, // team name
};

function loadState() {
  try {
    const s = JSON.parse(localStorage.getItem(LS_KEY) || "{}");
    Object.assign(state.regional,   s.regional   || {});
    Object.assign(state.super,      s.super      || {});
    Object.assign(state.cwsBracket, s.cwsBracket || {});
    state.champion = s.champion ?? null;
  } catch {}
}
function saveState() { localStorage.setItem(LS_KEY, JSON.stringify(state)); }
function clearState() {
  state.regional = {}; state.super = {}; state.cwsBracket = {}; state.champion = null;
  localStorage.removeItem(LS_KEY);
}

// ============================================================================
// Pick logic — invalidate downstream picks if they conflict
// ============================================================================
function setRegionalWinner(key, teamName) {
  state.regional[key] = teamName;
  // Find which super this regional feeds, invalidate downstream if necessary
  for (const sp of SUPER_PAIRS) {
    if (sp.regionalA === key || sp.regionalB === key) {
      const current = state.super[sp.key];
      const newOptions = [state.regional[sp.regionalA], state.regional[sp.regionalB]].filter(Boolean);
      if (current && !newOptions.includes(current)) {
        delete state.super[sp.key];
        invalidateCwsAndChamp();
      }
    }
  }
  saveState(); render();
}
function setSuperWinner(superKey, teamName) {
  state.super[superKey] = teamName;
  invalidateCwsAndChamp();
  saveState(); render();
}
function setCwsBracketWinner(bracketKey, teamName) {
  state.cwsBracket[bracketKey] = teamName;
  if (state.champion && !Object.values(state.cwsBracket).includes(state.champion)) {
    state.champion = null;
  }
  saveState(); render();
}
function setChampion(teamName) {
  state.champion = teamName;
  saveState(); render();
}
function invalidateCwsAndChamp() {
  for (const bk of Object.keys(BRACKET_SUPERS)) {
    const validNow = BRACKET_SUPERS[bk].map(s => state.super[s]).filter(Boolean);
    if (state.cwsBracket[bk] && !validNow.includes(state.cwsBracket[bk])) {
      delete state.cwsBracket[bk];
    }
  }
  if (state.champion && !Object.values(state.cwsBracket).includes(state.champion)) {
    state.champion = null;
  }
}

// ============================================================================
// Rendering helpers
// ============================================================================
function el(tag, cls, html) {
  const e = document.createElement(tag);
  if (cls) e.className = cls;
  if (html != null) e.innerHTML = html;
  return e;
}
function teamCell(team, opts = {}) {
  const cell = el("div", "tile-cell");
  // Pick-status coloring: if a result is known for this slot, override the
  // plain "picked" tint with green (correct) or red (wrong).
  if (opts.picked) {
    if (opts.actualWinner && team) {
      cell.classList.add(team.team === opts.actualWinner ? "picked-correct" : "picked-wrong");
    } else {
      cell.classList.add("picked");
    }
  } else if (opts.actualWinner && team && team.team === opts.actualWinner) {
    // Not the user's pick, but they were the actual winner — mark with a subtle accent
    cell.classList.add("actual-winner");
  }
  if (opts.dimmed) cell.classList.add("dimmed");
  if (!team) {
    cell.classList.add("empty");
    cell.innerHTML = `<span class="placeholder">${opts.placeholder || "—"}</span>`;
    return cell;
  }
  const img = makeLogoEl(team);
  img.className = "logo";
  cell.appendChild(img);
  const r = el("div", "row");
  const nm = el("span", "nm", team.team);
  r.appendChild(nm);
  if (opts.seedLabel) {
    const sub = el("span", "sub", `#${opts.seedLabel}`);
    r.appendChild(sub);
  }
  cell.appendChild(r);
  if (opts.onClick && !opts.dimmed) {
    cell.addEventListener("click", () => opts.onClick(team));
  }
  return cell;
}

function regionalTile(r) {
  const tile = el("div", "tile tile-regional");
  const head = el("div", "tile-head clickable", "");
  head.appendChild(el("span", "", r.name));
  head.addEventListener("click", () => openRegionalModal(r.key));
  tile.appendChild(head);

  const actualWinner = RESULTS_AT_BOOT.regional?.[r.key] || null;
  const grid = el("div", "tile-2x2");
  for (const t of r.teams) {
    grid.appendChild(teamCell(t, {
      picked: state.regional[r.key] === t.team,
      actualWinner,
      seedLabel: t.regional_seed,
      onClick: tt => setRegionalWinner(r.key, tt.team),
    }));
  }
  tile.appendChild(grid);
  return tile;
}

function superTile(sp) {
  const tile = el("div", "tile tile-super");
  const head = el("div", "tile-head clickable", "");
  head.appendChild(el("span", "", sp.key.replace("_", " ").toUpperCase()));
  head.addEventListener("click", () => openScheduleModal("super", sp.key));
  tile.appendChild(head);

  const actualWinner = RESULTS_AT_BOOT.super?.[sp.key] || null;
  const grid = el("div", "tile-2x1");
  for (const regKey of [sp.regionalA, sp.regionalB]) {
    const win = state.regional[regKey];
    const team = win ? TEAM_BY_NAME[win] : null;
    grid.appendChild(teamCell(team, {
      picked: state.super[sp.key] === win,
      actualWinner,
      placeholder: `winner of ${REGIONAL_BY_KEY[regKey].name}`,
      onClick: t => setSuperWinner(sp.key, t.team),
    }));
  }
  tile.appendChild(grid);
  return tile;
}

function superPair(sp, side) {
  const wrap = el("div", `super-pair side-${side}`);
  if (state.regional[sp.regionalA] && state.regional[sp.regionalB]) {
    wrap.classList.add("pair-complete");
  }

  const regs = el("div", "regionals");
  regs.appendChild(regionalTile(REGIONAL_BY_KEY[sp.regionalA]));
  regs.appendChild(regionalTile(REGIONAL_BY_KEY[sp.regionalB]));

  const conn = el("div", "connector", '<span class="vline"></span>');

  const superSlotWrap = el("div", "super-slot");
  superSlotWrap.appendChild(superTile(sp));

  if (side === "left") {
    wrap.appendChild(regs);
    wrap.appendChild(conn);
    wrap.appendChild(superSlotWrap);
  } else {
    wrap.appendChild(superSlotWrap);
    wrap.appendChild(conn);
    wrap.appendChild(regs);
  }
  return wrap;
}

function cwsBracketTile(bracketKey, label) {
  const tile = el("div", "tile tile-cws");
  const head = el("div", "tile-head clickable", "");
  head.appendChild(el("span", "", label));
  head.addEventListener("click", () => openScheduleModal("cws", "bracket"));
  tile.appendChild(head);

  const actualWinner = RESULTS_AT_BOOT.cwsBracket?.[bracketKey] || null;
  const grid = el("div", "tile-2x2");
  const supers = BRACKET_SUPERS[bracketKey];
  for (const sk of supers) {
    const w = state.super[sk];
    const team = w ? TEAM_BY_NAME[w] : null;
    grid.appendChild(teamCell(team, {
      picked: state.cwsBracket[bracketKey] === w,
      actualWinner,
      placeholder: `winner of ${sk.replace("_", " ").toUpperCase()}`,
      onClick: t => setCwsBracketWinner(bracketKey, t.team),
    }));
  }
  tile.appendChild(grid);
  return tile;
}

function finalsTile() {
  const tile = el("div", "tile tile-finals");
  const head = el("div", "tile-head clickable", "");
  head.appendChild(el("span", "", "CWS Finals"));
  head.addEventListener("click", () => openScheduleModal("cws", "finals"));
  tile.appendChild(head);

  const actualChamp = RESULTS_AT_BOOT.champion || null;
  const grid = el("div", "tile-2x1");
  for (const bk of ["cws_bracket_1", "cws_bracket_2"]) {
    const w = state.cwsBracket[bk];
    const team = w ? TEAM_BY_NAME[w] : null;
    grid.appendChild(teamCell(team, {
      picked: state.champion === w,
      actualWinner: actualChamp,
      placeholder: `winner of ${bk.replace("_", " ").toUpperCase()}`,
      onClick: t => setChampion(t.team),
    }));
  }
  tile.appendChild(grid);
  return tile;
}

function championTrophy() {
  const wrap = el("div", "champion-trophy");
  const team = state.champion ? TEAM_BY_NAME[state.champion] : null;
  if (!team) wrap.classList.add("empty");
  wrap.appendChild(el("div", "label", "🏆 National Champion"));
  if (team) {
    const img = makeLogoEl(team);
    img.className = "champion-logo";
    wrap.appendChild(img);
  }
  wrap.appendChild(el("div", "name", team ? team.team : "Pick a champion below"));
  return wrap;
}

// ============================================================================
// Main render
// ============================================================================
function render() {
  const board = $("#bracket-board");
  board.innerHTML = "";

  const left = el("div", "bracket-side left");
  const center = el("div", "bracket-center");
  const right = el("div", "bracket-side right");

  // Left side: super-pairs from LEFT_SUPER_KEYS
  for (const sk of LEFT_SUPER_KEYS) {
    const sp = SUPER_PAIRS.find(s => s.key === sk);
    left.appendChild(superPair(sp, "left"));
  }
  // Right side
  for (const sk of RIGHT_SUPER_KEYS) {
    const sp = SUPER_PAIRS.find(s => s.key === sk);
    right.appendChild(superPair(sp, "right"));
  }

  // Center: champion trophy → finals → bracket 1 → bracket 2
  center.appendChild(championTrophy());
  center.appendChild(el("div", "center-connector" + (state.champion ? " lit" : "")));
  center.appendChild(finalsTile());
  center.appendChild(el("div", "center-connector" + (state.cwsBracket.cws_bracket_1 ? " lit" : "")));
  center.appendChild(cwsBracketTile("cws_bracket_1", "CWS Bracket 1"));
  center.appendChild(el("div", "center-connector" + (state.cwsBracket.cws_bracket_2 ? " lit" : "")));
  center.appendChild(cwsBracketTile("cws_bracket_2", "CWS Bracket 2"));

  board.appendChild(left);
  board.appendChild(center);
  board.appendChild(right);

  refreshStatus();
}

// ============================================================================
// Status / completion
// ============================================================================
const TOTAL_PICKS = 16 + 8 + 2 + 1;
function pickCount() {
  return Object.keys(state.regional).length
       + Object.keys(state.super).length
       + Object.keys(state.cwsBracket).length
       + (state.champion ? 1 : 0);
}
function isComplete() {
  return Object.keys(state.regional).length === 16
      && Object.keys(state.super).length === 8
      && Object.keys(state.cwsBracket).length === 2
      && state.champion != null;
}
function refreshStatus() {
  const n = pickCount();
  const status = $("#predict-status");
  status.textContent = `${n} of ${TOTAL_PICKS} picks made`;
  status.classList.toggle("complete", isComplete());
  $("#submit-bracket").disabled = !isComplete();
}

// ============================================================================
// Buttons
// ============================================================================
$("#reset").addEventListener("click", () => {
  if (!confirm("Clear your entire bracket?")) return;
  clearState(); render();
});
$("#random").addEventListener("click", () => {
  clearState();
  for (const r of bracket.regionals) {
    state.regional[r.key] = r.teams[Math.floor(Math.random() * 4)].team;
  }
  for (const sp of SUPER_PAIRS) {
    const opts = [state.regional[sp.regionalA], state.regional[sp.regionalB]];
    state.super[sp.key] = opts[Math.floor(Math.random() * 2)];
  }
  for (const bk of Object.keys(BRACKET_SUPERS)) {
    const opts = BRACKET_SUPERS[bk].map(s => state.super[s]);
    state.cwsBracket[bk] = opts[Math.floor(Math.random() * opts.length)];
  }
  const finalsOpts = Object.values(state.cwsBracket);
  state.champion = finalsOpts[Math.floor(Math.random() * finalsOpts.length)];
  saveState(); render();
});
$("#autopick").addEventListener("click", () => {
  clearState();
  for (const r of bracket.regionals) {
    state.regional[r.key] = [...r.teams].sort((a, b) => b.elo - a.elo)[0].team;
  }
  for (const sp of SUPER_PAIRS) {
    const a = TEAM_BY_NAME[state.regional[sp.regionalA]];
    const b = TEAM_BY_NAME[state.regional[sp.regionalB]];
    state.super[sp.key] = (a.elo >= b.elo ? a : b).team;
  }
  for (const bk of Object.keys(BRACKET_SUPERS)) {
    const teams = BRACKET_SUPERS[bk]
      .map(s => TEAM_BY_NAME[state.super[s]])
      .filter(Boolean)
      .sort((a, b) => b.elo - a.elo);
    state.cwsBracket[bk] = teams[0]?.team;
  }
  const finalsTeams = Object.values(state.cwsBracket).map(n => TEAM_BY_NAME[n]).filter(Boolean)
    .sort((a, b) => b.elo - a.elo);
  state.champion = finalsTeams[0]?.team;
  saveState(); render();
});

// ============================================================================
// Submit → Google Form
// ============================================================================
$("#submit-bracket").addEventListener("click", () => {
  const name = $("#entry-name").value.trim();
  const email = $("#entry-email").value.trim();
  const result = $("#submit-result");
  result.className = "submit-result";
  result.textContent = "";

  if (!name) {
    result.classList.add("error"); result.textContent = "Name is required."; return;
  }
  if (!isComplete()) {
    result.classList.add("error"); result.textContent = "Bracket isn't complete yet."; return;
  }

  const payload = {
    name, email,
    regional:   { ...state.regional },
    super:      { ...state.super },
    cwsBracket: { ...state.cwsBracket },
    champion:   state.champion,
    submitted_at: new Date().toISOString(),
  };

  if (!CONFIG.GOOGLE_FORM_ACTION_URL || CONFIG.FIELD_IDS.name === "entry.000000000") {
    result.classList.add("success");
    result.textContent = "✓ Bracket saved locally. (Backend not configured yet.)";
    return;
  }

  // Only 4 fields go to the Google Form: name, email, champion, full-bracket JSON.
  // The bracket JSON contains every pick so nothing is lost.
  const fd = new FormData();
  fd.append(CONFIG.FIELD_IDS.name, name);
  if (email) fd.append(CONFIG.FIELD_IDS.email, email);
  fd.append(CONFIG.FIELD_IDS.champion, state.champion);
  fd.append(CONFIG.FIELD_IDS.bracket, JSON.stringify(payload));

  fetch(CONFIG.GOOGLE_FORM_ACTION_URL, { method: "POST", mode: "no-cors", body: fd })
    .then(() => {
      result.classList.add("success");
      result.textContent = "✓ Bracket submitted! Thanks for entering.";
      setTimeout(loadEntries, 1500);
    })
    .catch(e => {
      result.classList.add("error");
      result.textContent = `Submit failed: ${e.message}`;
    });
});

// ============================================================================
// Entries leaderboard with March-Madness-style scoring
// ============================================================================
// Points per correct pick — each round contributes 160 total possible pts.
// Total max = 640.
const POINTS = { regional: 10, super: 20, cwsBracket: 80, champion: 160 };

let RESULTS = null;          // loaded from data/results.json
let ENTRIES = [];            // parsed + scored entries

async function loadResults() {
  try {
    RESULTS = await loadJSON("results.json");
  } catch {
    RESULTS = { regional: {}, super: {}, cwsBracket: {}, champion: null };
  }
}

// Score one entry's bracket against the current RESULTS file. Returns
// { total, byRound: {regional, super, cwsBracket, champion}, correct: {...} }.
function scoreEntry(picks) {
  const r = RESULTS || { regional: {}, super: {}, cwsBracket: {}, champion: null };
  let total = 0;
  const byRound = { regional: 0, super: 0, cwsBracket: 0, champion: 0 };
  const correct = { regional: {}, super: {}, cwsBracket: {}, champion: null };

  for (const key of Object.keys(picks.regional || {})) {
    const actual = r.regional?.[key];
    if (actual && picks.regional[key] === actual) {
      total += POINTS.regional;
      byRound.regional += POINTS.regional;
      correct.regional[key] = true;
    } else if (actual) {
      correct.regional[key] = false;
    }
  }
  for (const key of Object.keys(picks.super || {})) {
    const actual = r.super?.[key];
    if (actual && picks.super[key] === actual) {
      total += POINTS.super;
      byRound.super += POINTS.super;
      correct.super[key] = true;
    } else if (actual) {
      correct.super[key] = false;
    }
  }
  for (const key of Object.keys(picks.cwsBracket || {})) {
    const actual = r.cwsBracket?.[key];
    if (actual && picks.cwsBracket[key] === actual) {
      total += POINTS.cwsBracket;
      byRound.cwsBracket += POINTS.cwsBracket;
      correct.cwsBracket[key] = true;
    } else if (actual) {
      correct.cwsBracket[key] = false;
    }
  }
  if (r.champion && picks.champion === r.champion) {
    total += POINTS.champion;
    byRound.champion = POINTS.champion;
    correct.champion = true;
  } else if (r.champion) {
    correct.champion = false;
  }

  return { total, byRound, correct };
}

// "Possible so far" = max points achievable given what results are already known.
function maxPossibleSoFar() {
  let max = 0;
  for (const v of Object.values(RESULTS?.regional || {}))   if (v) max += POINTS.regional;
  for (const v of Object.values(RESULTS?.super || {}))      if (v) max += POINTS.super;
  for (const v of Object.values(RESULTS?.cwsBracket || {})) if (v) max += POINTS.cwsBracket;
  if (RESULTS?.champion) max += POINTS.champion;
  return max;
}

async function loadEntries() {
  const list = $("#entries-list");
  if (!CONFIG.ENTRIES_CSV_URL) {
    list.innerHTML = `<p class="loading">Leaderboard backend not yet configured.</p>`;
    return;
  }
  try {
    await loadResults();
    const r = await fetch(CONFIG.ENTRIES_CSV_URL);
    const text = await r.text();
    const rows = parseCSV(text);
    if (rows.length <= 1) {
      list.innerHTML = `<p class="loading">No entries yet — be the first!</p>`;
      $("#entries-sub").textContent = "0 entries";
      return;
    }
    const header = rows[0];
    const idx = {
      ts:    header.findIndex(h => h.toLowerCase().includes("timestamp")),
      name:  header.findIndex(h => h.toLowerCase() === "name"),
      email: header.findIndex(h => h.toLowerCase() === "email"),
      champ: header.findIndex(h => h.toLowerCase() === "champion"),
      brack: header.findIndex(h => h.toLowerCase() === "bracket"),
    };
    // Skip blank rows (deleted-but-not-removed Sheet rows leave empty cells).
    const data = rows.slice(1).filter(row => {
      const name    = (row[idx.name]  ?? "").trim();
      const bracket = (row[idx.brack] ?? "").trim();
      const champ   = (row[idx.champ] ?? "").trim();
      return name || bracket || champ;   // keep only rows with real content
    });

    ENTRIES = data.map(row => {
      let picks = {};
      try { picks = JSON.parse(row[idx.brack] || "{}"); } catch {}
      const score = scoreEntry(picks);
      return {
        timestamp: row[idx.ts] || "",
        name:      row[idx.name] || "anonymous",
        email:     row[idx.email] || "",
        champion:  row[idx.champ] || "",
        picks,
        score,
      };
    });
    // Sort by score desc, ties broken by earliest submission (oldest first)
    ENTRIES.sort((a, b) => b.score.total - a.score.total || a.timestamp.localeCompare(b.timestamp));

    const maxSoFar = maxPossibleSoFar();
    const subText = maxSoFar > 0
      ? `${ENTRIES.length} entries · max possible so far: ${maxSoFar} / 640 pts`
      : `${ENTRIES.length} entries · scoring activates as results are entered`;
    $("#entries-sub").textContent = subText;

    list.innerHTML = "";
    for (let i = 0; i < ENTRIES.length; i++) {
      const e = ENTRIES[i];
      const row = document.createElement("div");
      row.className = "entry-row";
      row.dataset.idx = String(i);
      row.innerHTML = `
        <span class="rank">${i + 1}</span>
        <span class="nm">${esc(e.name)}
          <span class="pick"> → champ: ${esc(e.champion || "?")}</span></span>
        <span class="score">${e.score.total} <span class="score-lbl">pts</span></span>
      `;
      row.addEventListener("click", () => openEntryModal(i));
      list.appendChild(row);
    }
  } catch (e) {
    list.innerHTML = `<p class="loading">Couldn't load entries: ${e.message}</p>`;
  }
}

// ============================================================================
// Entry detail modal
// ============================================================================
function openEntryModal(entryIdx) {
  const e = ENTRIES[entryIdx];
  if (!e) return;
  const modal = $("#entry-modal");
  const body = $("#entry-modal-body");
  body.innerHTML = "";

  // Header
  const head = document.createElement("div");
  head.className = "modal-head";
  head.innerHTML = `
    <div>
      <div class="modal-name">${esc(e.name)}</div>
      <div class="modal-sub">${esc(e.timestamp)}</div>
    </div>
    <div class="modal-score">
      <div class="modal-score-num">${e.score.total}</div>
      <div class="modal-score-lbl">points</div>
    </div>
  `;
  body.appendChild(head);

  // Section helper
  const section = (title, picks, actualByKey, perPickPts, keyLabel) => {
    const sec = document.createElement("div");
    sec.className = "modal-section";
    const earnedKeys = Object.keys(picks).filter(k => actualByKey?.[k] && picks[k] === actualByKey[k]);
    const decidedKeys = Object.keys(actualByKey || {}).filter(k => actualByKey[k]);
    sec.innerHTML = `<div class="modal-section-head">
      <span class="modal-section-title">${title}</span>
      <span class="modal-section-meta">${earnedKeys.length}/${decidedKeys.length || Object.keys(picks).length} correct · ${earnedKeys.length * perPickPts} pts</span>
    </div>`;
    const grid = document.createElement("div");
    grid.className = "modal-grid";
    for (const k of Object.keys(picks)) {
      const pick = picks[k];
      const actual = actualByKey?.[k];
      let status = "pending";
      if (actual && pick === actual)   status = "correct";
      else if (actual)                 status = "wrong";
      const item = document.createElement("div");
      item.className = `modal-pick modal-pick-${status} has-logo`;
      const label = keyLabel ? keyLabel(k) : k;
      const pickTeam = pick ? TEAM_BY_NAME[pick] : null;
      const actualTeam = (actual && status !== "correct") ? TEAM_BY_NAME[actual] : null;
      const logoImg = pickTeam && pickTeam.logo
        ? `<img class="modal-logo" src="${pickTeam.logo}" alt="" onerror="this.style.visibility='hidden'" loading="lazy">`
        : `<span class="modal-logo modal-logo-placeholder"></span>`;
      const actualImg = actualTeam && actualTeam.logo
        ? `<img class="modal-actual-logo" src="${actualTeam.logo}" alt="" onerror="this.style.visibility='hidden'" loading="lazy">`
        : "";
      item.innerHTML = `
        ${logoImg}
        <span class="modal-key">${esc(label)}</span>
        <span class="modal-team">${esc(pick || "—")}</span>
        ${actual && status !== "correct" ? `<span class="modal-actual">${actualImg}actual: ${esc(actual)}</span>` : ""}
        <span class="modal-status">${status === "correct" ? "✓" : status === "wrong" ? "✗" : "·"}</span>
      `;
      grid.appendChild(item);
    }
    sec.appendChild(grid);
    return sec;
  };

  const regKeyLabel = k => {
    const map = {
      los_angeles:"Los Angeles", atlanta:"Atlanta", athens:"Athens", auburn:"Auburn",
      chapel_hill:"Chapel Hill", austin:"Austin", tuscaloosa:"Tuscaloosa", gainesville:"Gainesville",
      hattiesburg:"Hattiesburg", tallahassee:"Tallahassee", eugene:"Eugene", college_station:"College Station",
      lincoln:"Lincoln", starkville:"Starkville", lawrence:"Lawrence", morgantown:"Morgantown",
    };
    return map[k] || k;
  };
  body.appendChild(section("Regional Winners (10 pts each)",   e.picks.regional   || {}, RESULTS?.regional,   POINTS.regional,   regKeyLabel));
  body.appendChild(section("Super Regionals (20 pts each)",    e.picks.super      || {}, RESULTS?.super,      POINTS.super,      k => k.replace("_", " ").toUpperCase()));
  body.appendChild(section("CWS Bracket Winners (80 pts each)",e.picks.cwsBracket || {}, RESULTS?.cwsBracket, POINTS.cwsBracket, k => k.replace(/_/g, " ").replace(/cws bracket/i, "CWS Bracket")));

  // Champion
  const champSec = document.createElement("div");
  champSec.className = "modal-section";
  const champStatus = !RESULTS?.champion ? "pending"
    : e.picks.champion === RESULTS.champion ? "correct" : "wrong";
  const champTeam = e.picks.champion ? TEAM_BY_NAME[e.picks.champion] : null;
  const actualChampTeam = (RESULTS?.champion && champStatus !== "correct") ? TEAM_BY_NAME[RESULTS.champion] : null;
  const champLogo = champTeam && champTeam.logo
    ? `<img class="modal-logo" src="${champTeam.logo}" alt="" onerror="this.style.visibility='hidden'" loading="lazy">`
    : `<span class="modal-logo modal-logo-placeholder"></span>`;
  const actualChampLogo = actualChampTeam && actualChampTeam.logo
    ? `<img class="modal-actual-logo" src="${actualChampTeam.logo}" alt="" onerror="this.style.visibility='hidden'" loading="lazy">`
    : "";
  champSec.innerHTML = `
    <div class="modal-section-head">
      <span class="modal-section-title">National Champion (160 pts)</span>
      <span class="modal-section-meta">${champStatus === "correct" ? `+${POINTS.champion} pts` : "—"}</span>
    </div>
    <div class="modal-grid">
      <div class="modal-pick modal-pick-${champStatus} has-logo">
        ${champLogo}
        <span class="modal-key">Champion</span>
        <span class="modal-team">${esc(e.picks.champion || "—")}</span>
        ${RESULTS?.champion && champStatus !== "correct" ? `<span class="modal-actual">${actualChampLogo}actual: ${esc(RESULTS.champion)}</span>` : ""}
        <span class="modal-status">${champStatus === "correct" ? "✓" : champStatus === "wrong" ? "✗" : "·"}</span>
      </div>
    </div>
  `;
  body.appendChild(champSec);

  modal.classList.add("open");
}

function closeEntryModal() {
  $("#entry-modal").classList.remove("open");
}

// ============================================================================
// Regional / Super / CWS schedule modal — shows the per-game breakdown for an
// event (header click).  Reads from schedule.json.
// ============================================================================
function getMatchupText(pattern, regKey) {
  if (!regKey) return "TBD";
  const r = REGIONAL_BY_KEY[regKey];
  if (!r) return "TBD";
  const seedToName = Object.fromEntries(r.teams.map(t => [t.regional_seed, t.team]));
  switch (pattern) {
    case "1v4": return `#1 ${seedToName[1]} vs #4 ${seedToName[4]}`;
    case "2v3": return `#2 ${seedToName[2]} vs #3 ${seedToName[3]}`;
    case "L1vL2": return "Loser G1 vs Loser G2";
    case "W1vW2": return "Winner G1 vs Winner G2";
    case "L4vW3": return "Loser G4 vs Winner G3";
    case "W4vW5": return "Winner G4 vs Winner G5";
    case "rematch": return "Rematch of G6";
    default: return "TBD";
  }
}

// Map a matchup_pattern to {bracket: "winners"|"elim", round: 1..4}.
// Used to lay out regional games in a proper tournament-tree grid.
function regionalGameSlot(pattern) {
  switch (pattern) {
    case "1v4":     return { bracket: "winners", round: 1 };
    case "2v3":     return { bracket: "winners", round: 1 };
    case "L1vL2":   return { bracket: "elim",    round: 2 };
    case "W1vW2":   return { bracket: "winners", round: 2 };
    case "L4vW3":   return { bracket: "elim",    round: 3 };
    case "W4vW5":   return { bracket: "winners", round: 3 };
    case "rematch": return { bracket: "winners", round: 4 };
    default:        return { bracket: "winners", round: 1 };
  }
}

// Resolve the two teams in a game given its matchup pattern + regional context.
// Returns [team1, team2] where each is either a team object (known) or a
// placeholder { placeholder: "..." } (TBD / derived).
function resolveGameTeams(pattern, regKey) {
  const r = regKey ? REGIONAL_BY_KEY[regKey] : null;
  const bySeed = r ? Object.fromEntries(r.teams.map(t => [t.regional_seed, t])) : {};
  switch (pattern) {
    case "1v4":     return [bySeed[1], bySeed[4]];
    case "2v3":     return [bySeed[2], bySeed[3]];
    case "L1vL2":   return [{ placeholder: "Loser Game 1" },   { placeholder: "Loser Game 2" }];
    case "W1vW2":   return [{ placeholder: "Winner Game 1" },  { placeholder: "Winner Game 2" }];
    case "L4vW3":   return [{ placeholder: "Loser Game 4" },   { placeholder: "Winner Game 3" }];
    case "W4vW5":   return [{ placeholder: "Winner Game 4" },  { placeholder: "Winner Game 5" }];
    case "rematch": return [{ placeholder: "Winner Game 6" },  { placeholder: "Loser Game 6" }];
    case "AvB":     return [{ placeholder: "Team A" },         { placeholder: "Team B" }];
    default:        return [{ placeholder: "TBD" },            { placeholder: "TBD" }];
  }
}

function gameTeamRow(team) {
  const row = el("div", "game-team");
  if (team && team.team) {
    const img = makeLogoEl(team);
    img.className = "game-logo";
    row.appendChild(img);
    const meta = el("div", "game-team-meta");
    const nm = el("span", "game-team-nm", team.team);
    const sd = el("span", "game-team-seed", `#${team.regional_seed}`);
    meta.appendChild(nm); meta.appendChild(sd);
    row.appendChild(meta);
  } else {
    row.classList.add("game-team-tbd");
    row.textContent = team?.placeholder || "TBD";
  }
  return row;
}

// Build a single game-tile DOM element with header (date/time/network) and
// body (two team rows with logos).
function gameTile(g, regKey, opts = {}) {
  const tile = el("div", "game-tile" + (opts.tentative ? " tentative" : ""));
  const [t1, t2] = resolveGameTeams(g.matchup_pattern, regKey);
  tile.innerHTML = `
    <div class="game-header">
      <div class="game-label">${esc(g.label)}</div>
      <div class="game-when">${esc(g.datetime || "TBD")}${g.network ? ` <span class="net">${esc(g.network)}</span>` : ""}</div>
    </div>
  `;
  const body = el("div", "game-body");
  body.appendChild(gameTeamRow(t1));
  body.appendChild(gameTeamRow(t2));
  tile.appendChild(body);
  return tile;
}

function openRegionalModal(regKey) {
  const r = REGIONAL_BY_KEY[regKey];
  if (!r) return;
  const sched = schedule?.regional?.[regKey] || schedule?.regional?._default;
  const actual = RESULTS_AT_BOOT.regional?.[regKey];
  const userPick = state.regional[regKey];

  const modal = $("#entry-modal");
  const body = $("#entry-modal-body");
  body.innerHTML = "";

  // Header
  const head = document.createElement("div");
  head.className = "modal-head";
  head.innerHTML = `
    <div>
      <div class="modal-name">${esc(r.name)} Regional</div>
      <div class="modal-sub">#${r.seed} ${esc(r.host)} · ${esc(sched?.venue || "")}</div>
    </div>
    <div class="modal-score">
      ${actual ? `<div class="modal-score-num" style="font-size:18px;">${esc(actual)}</div><div class="modal-score-lbl">Regional Winner</div>`
              : `<div class="modal-score-lbl" style="color:var(--text-dim);font-size:13px;">In progress</div>`}
    </div>
  `;
  body.appendChild(head);

  // Teams in this regional (with current state — picked / actual winner)
  const teamsSec = document.createElement("div");
  teamsSec.className = "modal-section";
  teamsSec.innerHTML = `<div class="modal-section-head">
    <span class="modal-section-title">Field</span>
    <span class="modal-section-meta">${userPick ? `your pick: ${esc(userPick)}` : "no pick yet"}</span>
  </div>`;
  const teamsGrid = document.createElement("div");
  teamsGrid.className = "modal-grid";
  for (const t of r.teams) {
    let status = "pending";
    if (actual && t.team === actual) status = "correct";
    else if (actual && t.team === userPick) status = "wrong";
    const isPick = userPick === t.team;
    const item = document.createElement("div");
    item.className = `modal-pick modal-pick-${status}`;
    item.innerHTML = `
      <span class="modal-key">#${t.regional_seed}${isPick ? " · your pick" : ""}</span>
      <span class="modal-team">${esc(t.team)}</span>
      <span class="modal-status">${status === "correct" ? "✓" : status === "wrong" ? "✗" : "·"}</span>
    `;
    teamsGrid.appendChild(item);
  }
  teamsSec.appendChild(teamsGrid);
  body.appendChild(teamsSec);

  // Schedule — bracket-flow layout: top row Winners' Bracket, bottom Elim
  const schedSec = document.createElement("div");
  schedSec.className = "modal-section";
  schedSec.innerHTML = `<div class="modal-section-head">
    <span class="modal-section-title">Schedule</span>
    <span class="modal-section-meta">${sched?.games?.length || 0} games · double elimination</span>
  </div>`;

  // Group games by bracket + round
  const N_ROUNDS = 4;
  const winners = [[], [], [], []];   // index = round - 1
  const elim    = [[], [], [], []];
  for (const g of (sched?.games || [])) {
    const slot = regionalGameSlot(g.matchup_pattern);
    const target = slot.bracket === "winners" ? winners : elim;
    target[slot.round - 1].push(g);
  }

  const flow = el("div", "bracket-flow");

  // Helper: builds a bracket row and tags columns that merge into the next.
  function buildRow(gamesByRound, label) {
    const sec = el("div", "bracket-section");
    sec.appendChild(el("div", "bracket-section-title", label));
    const row = el("div", `bracket-row cols-${N_ROUNDS}`);
    const cols = [];
    for (let r = 0; r < N_ROUNDS; r++) {
      const col = el("div", "bracket-col");
      for (const g of gamesByRound[r]) {
        col.appendChild(gameTile(g, regKey,
                                  { tentative: /if necessary/i.test(g.label) }));
      }
      if (gamesByRound[r].length === 0) col.appendChild(el("div", "bracket-col-empty", ""));
      cols.push({ el: col, count: gamesByRound[r].length });
      row.appendChild(col);
    }
    // Mark columns that have multiple games feeding into a single next column
    for (let i = 0; i < cols.length - 1; i++) {
      if (cols[i].count >= 2 && cols[i + 1].count === 1) {
        cols[i].el.classList.add("merger");
      }
    }
    sec.appendChild(row);
    return sec;
  }

  flow.appendChild(buildRow(winners, "Winners' Bracket"));
  flow.appendChild(buildRow(elim,    "Elimination Bracket"));

  schedSec.appendChild(flow);
  body.appendChild(schedSec);

  modal.classList.add("open");
}

function openScheduleModal(kind, key) {
  // Super or CWS bracket/finals schedule popup
  let sched, title, sub;
  if (kind === "super") {
    sched = schedule?.super?._default;
    title = key.replace("_", " ").toUpperCase();
    sub = "Best-of-3 super regional · higher seed hosts";
  } else if (kind === "cws") {
    sched = schedule?.cws?.[key];
    title = key === "finals" ? "CWS Finals" : "CWS Bracket Play";
    sub = "Charles Schwab Field · Omaha, NE";
  }
  if (!sched) return;

  const modal = $("#entry-modal");
  const body = $("#entry-modal-body");
  body.innerHTML = "";

  const head = document.createElement("div");
  head.className = "modal-head";
  head.innerHTML = `
    <div>
      <div class="modal-name">${esc(title)}</div>
      <div class="modal-sub">${esc(sub)}</div>
    </div>
    <div class="modal-score">
      <div class="modal-score-lbl" style="color:var(--text-dim);font-size:13px;">${esc(sched.venue || "")}</div>
    </div>
  `;
  body.appendChild(head);

  const schedSec = document.createElement("div");
  schedSec.className = "modal-section";
  schedSec.innerHTML = `<div class="modal-section-head">
    <span class="modal-section-title">Schedule</span>
    <span class="modal-section-meta">${sched.games?.length || 0} games</span>
  </div>`;

  // Linear horizontal flow — one game per column (G1 → G2 → G3)
  const flow = el("div", "bracket-flow");
  const row = el("div", `bracket-row cols-${Math.max(1, sched.games?.length || 1)}`);
  for (const g of (sched.games || [])) {
    const col = el("div", "bracket-col");
    col.appendChild(gameTile(g, null, { tentative: /if necessary/i.test(g.label) }));
    row.appendChild(col);
  }
  flow.appendChild(row);
  schedSec.appendChild(flow);
  body.appendChild(schedSec);

  modal.classList.add("open");
}
function parseCSV(text) {
  const rows = []; let i = 0, field = "", row = [], inQ = false;
  while (i < text.length) {
    const c = text[i];
    if (inQ) {
      if (c === '"' && text[i + 1] === '"') { field += '"'; i += 2; }
      else if (c === '"') { inQ = false; i++; }
      else { field += c; i++; }
    } else {
      if (c === '"') { inQ = true; i++; }
      else if (c === ",") { row.push(field); field = ""; i++; }
      else if (c === "\n" || c === "\r") {
        if (field || row.length) { row.push(field); rows.push(row); }
        field = ""; row = [];
        if (c === "\r" && text[i + 1] === "\n") i += 2; else i++;
      } else { field += c; i++; }
    }
  }
  if (field || row.length) { row.push(field); rows.push(row); }
  return rows;
}
function esc(s) {
  return String(s).replace(/[&<>"']/g, c => ({"&":"&amp;","<":"&lt;",">":"&gt;","\"":"&quot;","'":"&#39;"}[c]));
}

// ============================================================================
// Modal close handlers
// ============================================================================
$("#entry-modal-close").addEventListener("click", closeEntryModal);
$("#entry-modal").addEventListener("click", (e) => {
  if (e.target.id === "entry-modal") closeEntryModal();
});
document.addEventListener("keydown", (e) => {
  if (e.key === "Escape") closeEntryModal();
});

// ============================================================================
// Boot
// ============================================================================
loadState();
render();
loadEntries();
