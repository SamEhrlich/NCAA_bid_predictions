// D3 Elo line chart with hover tooltip.
//
// Renders one team's Elo trajectory across one or more seasons.
// - line color = team's primary color
// - dots = each game (no W/L coloring per spec)
// - hover dot → tooltip w/ date, opponent, score, home/away/neutral, pre/post Elo, Δ
// - clicking a schedule row highlights the corresponding dot
//
// Public API:
//   renderEloChart(containerEl, scheduleEl, teamData, opts)
//     opts.year — restrict to a single season (number) or null for all years

import { fmtFullDate, makeLogoEl } from "./common.js";

const MARGIN = { top: 16, right: 22, bottom: 30, left: 50 };

export function renderEloChart(containerEl, scheduleEl, teamData, opts = {}) {
  containerEl.innerHTML = "";
  scheduleEl.innerHTML = "";

  // Filter events by year if requested
  const events = (opts.year
    ? teamData.events.filter(e => e.date.startsWith(String(opts.year)))
    : teamData.events
  ).map(e => ({ ...e, _dt: new Date(e.date) }));

  if (events.length === 0) {
    containerEl.innerHTML = `<p class="loading">No games for ${opts.year || "selected season"}.</p>`;
    return;
  }

  // ---------- SVG setup ----------
  const width  = containerEl.clientWidth;
  const height = 460;
  const innerW = width  - MARGIN.left - MARGIN.right;
  const innerH = height - MARGIN.top  - MARGIN.bottom;

  const svg = d3.select(containerEl)
    .append("svg")
    .attr("viewBox", `0 0 ${width} ${height}`)
    .attr("preserveAspectRatio", "xMidYMid meet");

  const g = svg.append("g")
    .attr("transform", `translate(${MARGIN.left},${MARGIN.top})`);

  // ---------- scales ----------
  const x = d3.scaleTime()
    .domain(d3.extent(events, d => d._dt))
    .range([0, innerW]);

  const yMin = d3.min(events, d => Math.min(d.pre_elo, d.post_elo));
  const yMax = d3.max(events, d => Math.max(d.pre_elo, d.post_elo));
  const pad = Math.max(20, (yMax - yMin) * 0.08);
  const y = d3.scaleLinear()
    .domain([yMin - pad, yMax + pad])
    .range([innerH, 0])
    .nice();

  // ---------- gridlines ----------
  g.append("g")
    .attr("class", "elo-grid")
    .call(d3.axisLeft(y).tickSize(-innerW).tickFormat(""));

  // Reference line at 1500
  if (y.domain()[0] < 1500 && y.domain()[1] > 1500) {
    g.append("line")
      .attr("class", "elo-1500")
      .attr("x1", 0).attr("x2", innerW)
      .attr("y1", y(1500)).attr("y2", y(1500));
  }

  // ---------- axes ----------
  const xAxis = d3.axisBottom(x)
    .ticks(d3.timeMonth.every(1))
    .tickFormat(d3.timeFormat("%b %y"));
  g.append("g")
    .attr("class", "elo-axis")
    .attr("transform", `translate(0,${innerH})`)
    .call(xAxis);

  g.append("g")
    .attr("class", "elo-axis")
    .call(d3.axisLeft(y).ticks(8));

  // ---------- line ----------
  const line = d3.line()
    .x(d => x(d._dt))
    .y(d => y(d.post_elo));

  g.append("path")
    .datum(events)
    .attr("class", "elo-line")
    .attr("stroke", teamData.color)
    .attr("d", line);

  // ---------- tooltip ----------
  const tooltip = d3.select(containerEl)
    .append("div")
    .attr("class", "tooltip");

  function tooltipHTML(d) {
    const wl = d.won ? '<span class="tt-result-w">W</span>' : '<span class="tt-result-l">L</span>';
    const delta = d.post_elo - d.pre_elo;
    const dCls = delta >= 0 ? "tt-elo-up" : "tt-elo-down";
    const dSym = delta >= 0 ? "+" : "";
    const siteLbl = d.site === "home" ? "vs" : d.site === "away" ? "@" : "vs (neutral)";
    return `
      <div class="tt-date">${fmtFullDate(d.date)}</div>
      <div class="tt-opp">${siteLbl} ${d.opp}</div>
      <div class="tt-row"><span>Result</span><span>${wl} ${d.score_for}–${d.score_against}</span></div>
      <div class="tt-row"><span>Pre-game Elo</span><span>${d.pre_elo.toFixed(1)}</span></div>
      <div class="tt-row"><span>Post-game Elo</span><span>${d.post_elo.toFixed(1)}</span></div>
      <div class="tt-row"><span>Δ</span><span class="${dCls}">${dSym}${delta.toFixed(1)}</span></div>
      <div class="tt-row"><span>Opp pre-game Elo</span><span>${d.opp_pre_elo.toFixed(1)}</span></div>
      <div class="tt-row"><span>Win prob pre-game</span><span>${(d.win_prob_pre * 100).toFixed(1)}%</span></div>
    `;
  }

  // ---------- dots ----------
  const dots = g.selectAll(".elo-dot")
    .data(events)
    .join("circle")
    .attr("class", "elo-dot")
    .attr("cx", d => x(d._dt))
    .attr("cy", d => y(d.post_elo))
    .attr("r", 3.2)
    .attr("fill", teamData.color)
    .attr("data-idx", (_, i) => i);

  dots.on("mouseenter", function (event, d) {
    d3.select(this).attr("r", 6);
    tooltip.html(tooltipHTML(d)).classed("show", true);
    // highlight schedule row
    scheduleEl.querySelectorAll(".sched-row").forEach(r => r.classList.remove("active"));
    const idx = +this.getAttribute("data-idx");
    const row = scheduleEl.querySelector(`.sched-row[data-idx="${idx}"]`);
    if (row) {
      row.classList.add("active");
      row.scrollIntoView({ block: "nearest", behavior: "smooth" });
    }
  });
  dots.on("mousemove", function (event) {
    const [mx, my] = d3.pointer(event, containerEl);
    const ttW = tooltip.node().offsetWidth;
    const ttH = tooltip.node().offsetHeight;
    let left = mx + 14;
    let top  = my + 14;
    // keep on-screen
    if (left + ttW > containerEl.clientWidth - 4) left = mx - ttW - 14;
    if (top + ttH > containerEl.clientHeight)     top  = my - ttH - 14;
    tooltip.style("left", `${left}px`).style("top", `${top}px`);
  });
  dots.on("mouseleave", function () {
    d3.select(this).attr("r", 3.2);
    tooltip.classed("show", false);
    scheduleEl.querySelectorAll(".sched-row").forEach(r => r.classList.remove("active"));
  });

  // ---------- schedule list ----------
  for (let i = 0; i < events.length; i++) {
    const d = events[i];
    const row = document.createElement("div");
    row.className = "sched-row";
    row.dataset.idx = String(i);
    const delta = d.post_elo - d.pre_elo;
    const dCls = delta >= 0 ? "pos" : "neg";
    const dSym = delta >= 0 ? "+" : "";
    const siteLbl = d.site === "home" ? "vs" : d.site === "away" ? "@" : "n.";
    row.innerHTML = `
      <span class="date">${d.date.slice(5).replace("-","/")}</span>
      <span class="site">${siteLbl}</span>
      <span class="opp">${d.opp}</span>
      <span class="result">${d.won
        ? '<span class="w">W</span>'
        : '<span class="l">L</span>'} ${d.score_for}-${d.score_against}</span>
      <span class="delta ${dCls}">${dSym}${delta.toFixed(1)}</span>
    `;
    row.addEventListener("mouseenter", () => {
      const dot = g.select(`.elo-dot[data-idx="${i}"]`);
      dot.attr("r", 6);
      tooltip.html(tooltipHTML(d)).classed("show", true);
      // place tooltip near the row's corresponding dot
      const cx = +dot.attr("cx") + MARGIN.left;
      const cy = +dot.attr("cy") + MARGIN.top;
      tooltip.style("left", `${cx + 14}px`).style("top", `${cy + 14}px`);
      scheduleEl.querySelectorAll(".sched-row").forEach(r => r.classList.remove("active"));
      row.classList.add("active");
    });
    row.addEventListener("mouseleave", () => {
      g.select(`.elo-dot[data-idx="${i}"]`).attr("r", 3.2);
      tooltip.classed("show", false);
      row.classList.remove("active");
    });
    scheduleEl.appendChild(row);
  }
}
