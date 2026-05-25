// Shared helpers used across pages.

export const DATA_URL = "data";

export async function loadJSON(path) {
  const r = await fetch(`${DATA_URL}/${path}`);
  if (!r.ok) throw new Error(`Failed to load ${path}: ${r.status}`);
  return r.json();
}

export function fmtPct(p, digits = 1) {
  if (p == null || Number.isNaN(p)) return "—";
  return `${p.toFixed(digits)}%`;
}

export function fmtElo(e) {
  if (e == null) return "—";
  return e.toFixed(1);
}

export function fmtDate(s) {
  // s = "YYYY-MM-DD"
  if (!s) return "";
  const [y, m, d] = s.split("-");
  const months = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"];
  return `${months[parseInt(m,10)-1]} ${parseInt(d,10)}`;
}

export function fmtFullDate(s) {
  if (!s) return "";
  const [y, m, d] = s.split("-");
  const months = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"];
  return `${months[parseInt(m,10)-1]} ${parseInt(d,10)}, ${y}`;
}

export function makeLogoEl(team) {
  const img = document.createElement("img");
  if (team.logo) {
    img.src = team.logo;
    img.alt = team.team || team.name || "";
    img.loading = "lazy";
    img.onerror = () => img.classList.add("logo-fallback");
  } else {
    img.classList.add("logo-fallback");
  }
  return img;
}

export function teamLink(team_or_slug, label) {
  const slug = typeof team_or_slug === "string" ? team_or_slug : team_or_slug.slug;
  const text = label || (typeof team_or_slug === "object" ? team_or_slug.team : team_or_slug);
  const a = document.createElement("a");
  a.href = `team.html?slug=${encodeURIComponent(slug)}`;
  a.textContent = text;
  return a;
}

export function setActiveNav(linkName) {
  document.querySelectorAll(".nav-links a").forEach(a => {
    if (a.dataset.nav === linkName) a.classList.add("active");
    else a.classList.remove("active");
  });
}

export function getQueryParam(name) {
  return new URLSearchParams(window.location.search).get(name);
}

export function $(selector) { return document.querySelector(selector); }
export function $$(selector) { return [...document.querySelectorAll(selector)]; }
