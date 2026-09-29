
(function () {
"use strict";

var PER_PAGE = 24;
var page = 1;
var lastCount = 0;
var currentQuery = "";
var enginePromise = null;
var markerSeq = 0;

var EXAMPLES = [
  { key: "Conditions", desc: "Introduces integer variables and guards. A counter limits step execution up to a defined value." },
  { key: "LikeAlgorithm", desc: "Models a recommendation system where Like/Dislike interactions reconfigure what the user sees." },
  { key: "GRG", desc: "Complex Guarded Reactive Graph using activation flags to manage component states." },
  { key: "TIMER", desc: "Fundamental timed-system example. Uses clocks and invariants to force timeouts." },
  { key: "Counter", desc: "Cascading activation rules create a progressive logical sequence of steps." },
  { key: "Vending (max eur1)", desc: "Vending machine with mutual exclusion: inserting 1€ blocks 50ct coins." },
  { key: "Vending (max 3prod)", desc: "Inventory management: disables purchase options as soon as stock reaches zero." },
];

function escapeHtml(s) {
  var d = document.createElement("div");
  d.innerText = (s === null || s === undefined) ? "" : s;
  return d.innerHTML;
}
function icon(name) {
  return (window.RTAIcon ? window.RTAIcon(name) : "");
}


function buildPreview(code) {
  var nodes = {}, order = [], edges = [], init = null;
  function add(id) {
    if (!nodes[id]) { nodes[id] = { id: id, level: -1, slot: 0, init: false }; order.push(id); }
    return nodes[id];
  }
  String(code || "").split(/\r?\n/).forEach(function (raw) {
    var line = raw.replace(/\/\/.*$/, "").trim();
    if (!line) return;
    var mi = /^init\s+([A-Za-z_][\w.]*)/.exec(line);
    if (mi) { init = mi[1]; add(init).init = true; return; }
    var mt = /^([A-Za-z_][\w.]*)\s*(?:-\s*[A-Za-z_][\w.]*\s*)?(?:--->|-->|->)\s*([A-Za-z_][\w.]*)/.exec(line);
    if (mt) {
      add(mt[1]); add(mt[2]);
      var dup = edges.some(function (e) { return e.a === mt[1] && e.b === mt[2]; });
      if (!dup) edges.push({ a: mt[1], b: mt[2], self: mt[1] === mt[2] });
    }
  });
  if (!order.length) return null;
  if (!init || !nodes[init]) init = order[0];

  var adj = {};
  edges.forEach(function (e) { if (!e.self) (adj[e.a] = adj[e.a] || []).push(e.b); });
  nodes[init].level = 0;
  var q = [init];
  while (q.length) {
    var cur = q.shift();
    (adj[cur] || []).forEach(function (n) {
      if (nodes[n].level === -1) { nodes[n].level = nodes[cur].level + 1; q.push(n); }
    });
  }
  order.forEach(function (id) { if (nodes[id].level === -1) nodes[id].level = 0; });

  var perLevel = {};
  order.forEach(function (id) { var L = nodes[id].level; nodes[id].slot = perLevel[L] || 0; perLevel[L] = nodes[id].slot + 1; });
  var maxLevel = 0, maxCount = 1;
  order.forEach(function (id) { maxLevel = Math.max(maxLevel, nodes[id].level); });
  Object.keys(perLevel).forEach(function (k) { maxCount = Math.max(maxCount, perLevel[k]); });

  var CW = 64, RH = 40, PAD = 26;
  var W = PAD * 2 + maxLevel * CW;
  var H = PAD * 2 + (maxCount - 1) * RH;
  order.forEach(function (id) {
    var n = nodes[id], count = perLevel[n.level];
    n.x = PAD + n.level * CW;
    n.y = H / 2 - ((count - 1) * RH) / 2 + n.slot * RH;
  });
  return { nodes: nodes, order: order, edges: edges, w: W, h: H };
}

function previewPlaceholder() {
  return '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 120 60" class="preview-empty" aria-hidden="true">' +
    '<circle cx="30" cy="30" r="9" fill="none" stroke="#cbd5e1" stroke-width="1.5"/>' +
    '<circle cx="90" cy="18" r="9" fill="none" stroke="#cbd5e1" stroke-width="1.5"/>' +
    '<circle cx="90" cy="42" r="9" fill="none" stroke="#cbd5e1" stroke-width="1.5"/>' +
    '<line x1="39" y1="27" x2="79" y2="20" stroke="#cbd5e1" stroke-width="1.5"/>' +
    '<line x1="39" y1="33" x2="79" y2="40" stroke="#cbd5e1" stroke-width="1.5"/></svg>';
}

function previewSVG(p) {
  if (!p) return previewPlaceholder();
  var mid = "rtaArw" + (++markerSeq);
  var R = 11;
  var s = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 ' + p.w + ' ' + p.h + '" preserveAspectRatio="xMidYMid meet" role="img" aria-label="Model graph preview">';
  s += '<defs><marker id="' + mid + '" viewBox="0 0 8 8" refX="7" refY="4" markerWidth="5" markerHeight="5" orient="auto-start-reverse"><path d="M0 0L8 4L0 8z" fill="#9aa4b2"/></marker></defs>';
  p.edges.forEach(function (e) {
    var a = p.nodes[e.a], b = p.nodes[e.b];
    if (e.self) {
      s += '<path d="M ' + (a.x + R - 2) + ' ' + (a.y - 6) + ' c 10 -12 22 -4 12 6" fill="none" stroke="#9aa4b2" stroke-width="1.4" marker-end="url(#' + mid + ')"/>';
      return;
    }
    var dx = b.x - a.x, dy = b.y - a.y, d = Math.sqrt(dx * dx + dy * dy) || 1;
    var ux = dx / d, uy = dy / d;
    s += '<line x1="' + (a.x + ux * (R + 1)) + '" y1="' + (a.y + uy * (R + 1)) +
         '" x2="' + (b.x - ux * (R + 4)) + '" y2="' + (b.y - uy * (R + 4)) +
         '" stroke="#9aa4b2" stroke-width="1.4" marker-end="url(#' + mid + ')"/>';
  });
  p.order.forEach(function (id) {
    var n = p.nodes[id];
    var lbl = id.length > 7 ? id.slice(0, 6) + "." : id;
    if (n.init) s += '<circle cx="' + n.x + '" cy="' + n.y + '" r="' + (R + 3) + '" fill="none" stroke="#2bc044" stroke-width="1.6"/>';
    s += '<circle cx="' + n.x + '" cy="' + n.y + '" r="' + R + '" fill="' + (n.init ? "#f1fdea" : "#ffffff") + '" stroke="' + (n.init ? "#25961b" : "#8d97a5") + '" stroke-width="1.6"/>';
    s += '<text x="' + n.x + '" y="' + (n.y + 3) + '" text-anchor="middle" font-family="JetBrains Mono, monospace" font-size="8" font-weight="600" fill="#374151">' + escapeHtml(lbl) + '</text>';
  });
  return s + '</svg>';
}

function ensureEngine() {
  if (window.RTA && typeof window.RTA.getExamples === "function") return Promise.resolve(true);
  if (enginePromise) return enginePromise;
  enginePromise = new Promise(function (resolve) {
    var tries = 0;
    function check() {
      if (window.RTA && typeof window.RTA.getExamples === "function") {
        console.info("[gallery] RTA engine ready");
        resolve(true);
        return;
      }
      tries++;
      if (tries > 50) {
        console.warn("[gallery] RTA engine unavailable (main.js não inicializou).");
        resolve(false);
        return;
      }
      setTimeout(check, 100);
    }
    check();
  });
  return enginePromise;
}


var examplesCache = null;
function loadExampleSources() {
  if (examplesCache) return examplesCache;
  examplesCache = fetch("js/static/examples.json?v=2", { credentials: "same-origin" })
    .then(function (r) {
      if (!r.ok) throw new Error("HTTP " + r.status);
      return r.json();
    })
    .then(function (obj) {
      console.info("[gallery] example sources loaded from examples.json");
      return obj;
    })
    .catch(function (e) {
      console.warn("[gallery] examples.json unavailable:", e);
      return {};
    });
  return examplesCache;
}

function loadExamplePreviews() {
  loadExampleSources().then(function (codes) {
    EXAMPLES.forEach(function (ex) {
      var box = document.querySelector('[data-preview-example="' + ex.key.replace(/"/g, '\\"') + '"]');
      if (!box) return;
      box.innerHTML = codes[ex.key] ? previewSVG(buildPreview(codes[ex.key])) : previewPlaceholder();
    });
  });
}


function makePreviewBox() {
  var box = document.createElement("div");
  box.className = "model-preview";
  box.innerHTML = '<div class="preview-skeleton"></div>';
  return box;
}

function renderExamples() {
  var grid = document.getElementById("examplesGrid");
  grid.innerHTML = "";
  var chip = document.getElementById("heroExCount");
  if (chip) chip.textContent = EXAMPLES.length + " built-in examples";
  EXAMPLES.forEach(function (ex, i) {
    var card = document.createElement("a");
    card.href = "editor.html?example=" + encodeURIComponent(ex.key);
    card.className = "model-card example-card";
    card.style.animationDelay = (i * 30) + "ms";
    var prev = makePreviewBox();
    prev.setAttribute("data-preview-example", ex.key);
    var body = document.createElement("div");
    body.className = "model-card-body";
    body.innerHTML =
      '<div class="model-card-title">' + escapeHtml(ex.key) + '</div>' +
      '<div class="model-card-desc">' + escapeHtml(ex.desc) + '</div>' +
      '<div class="model-card-meta"><span>' + icon("box") + 'Built-in example</span></div>';
    card.appendChild(prev);
    card.appendChild(body);
    grid.appendChild(card);
  });
  loadExamplePreviews();
}

function formatDate(s) {
  try {
    return new Date(s.replace(" ", "T")).toLocaleDateString("en-US", { year: "numeric", month: "short", day: "numeric" });
  } catch (e) { return ""; }
}

function renderModelCard(m, i) {
  var card = document.createElement("div");
  card.className = "model-card";
  card.tabIndex = 0;
  card.setAttribute("role", "button");
  card.style.animationDelay = ((i || 0) * 30) + "ms";

  var prev = makePreviewBox();
  prev.innerHTML = m.code ? previewSVG(buildPreview(m.code)) : previewPlaceholder();

  var body = document.createElement("div");
  body.className = "model-card-body";
  var date = formatDate(m.created_at);
  body.innerHTML =
    '<div class="model-card-title">' + escapeHtml(m.title) + '</div>' +
    '<div class="model-card-author">' + icon("user") + 'by ' + escapeHtml(m.username) + '</div>' +
    '<div class="model-card-desc">' + escapeHtml(m.description || "") + '</div>' +
    '<div class="model-card-meta"><span>' + icon("eye") + (m.views || 0) + '</span>' +
    (date ? '<span>' + icon("cal") + date + '</span>' : '') + '</div>';
  card.appendChild(prev);
  card.appendChild(body);
  card.onclick = function () { openModelModal(m); };
  card.onkeydown = function (e) { if (e.key === "Enter" || e.key === " ") { e.preventDefault(); openModelModal(m); } };
  return card;
}


async function loadCommunity(p, q) {
  page = p || 1;
  currentQuery = q || "";
  var grid = document.getElementById("communityGrid");
  grid.innerHTML = '<p class="text-muted">Loading...</p>';
  try {
    var res = await RTACommunity.listModels(currentQuery, page);
    var models = res.models || [];
    lastCount = models.length;
    document.getElementById("communityCount").textContent = models.length ? "(" + models.length + ")" : "";
    if (!models.length) {
      grid.innerHTML = '<p class="text-muted">No community models published yet. Be the first — open the editor and use "Publish Model".</p>';
      renderPagination();
      return;
    }
    grid.innerHTML = "";
    models.forEach(function (m, i) { grid.appendChild(renderModelCard(m, i)); });
    renderPagination();
  } catch (e) {
    lastCount = 0;
    grid.innerHTML = '<p class="text-muted">Could not load community models. Check that the API/database is configured (see README).</p>';
    renderPagination();
  }
}

function renderPagination() {
  var el = document.getElementById("pagination");
  if (!el) return;
  el.innerHTML = "";
  var prev = document.createElement("button");
  prev.className = "u-btn"; prev.textContent = "Previous";
  prev.disabled = page <= 1;
  prev.onclick = function () { loadCommunity(page - 1, currentQuery); };
  var info = document.createElement("span");
  info.className = "page-info"; info.textContent = "Page " + page;
  var next = document.createElement("button");
  next.className = "u-btn"; next.textContent = "Next";
  next.disabled = lastCount < PER_PAGE;
  next.onclick = function () { loadCommunity(page + 1, currentQuery); };
  el.appendChild(prev); el.appendChild(info); el.appendChild(next);
}

function openModelModal(m) {
  document.getElementById("modelModalTitle").textContent = m.title;
  document.getElementById("modelModalAuthor").innerHTML = icon("user") + " by " + escapeHtml(m.username);
  document.getElementById("modelModalDesc").textContent = m.description || "";
  document.getElementById("modelModalPreview").innerHTML = previewSVG(buildPreview(m.code));
  document.getElementById("modelModalOpen").onclick = function () {
    window.location.href = "editor.html?model=" + m.id;
  };
  document.getElementById("modelModal").style.display = "flex";
}

async function loadMyModels() {
  var section = document.getElementById("myModelsSection");
  var list = document.getElementById("myModelsList");
  try {
    var me = await RTACommunity.me();
    if (!me || !me.id) { section.style.display = "none"; return; }
  } catch (e) { section.style.display = "none"; return; }
  section.style.display = "block";
  list.innerHTML = "Loading...";
  try {
    var res = await RTACommunity.myModels();
    var models = res.models || [];
    if (!models.length) {
      list.innerHTML = '<p class="text-muted">You have not published any model yet. Open the editor and use "Publish Model".</p>';
      return;
    }
    list.innerHTML = "";
    models.forEach(function (m) {
      var row = document.createElement("div");
      row.className = "my-model-row";
      var label = document.createElement("span");
      label.className = "mmr-label";
      label.innerHTML = icon("folder") + escapeHtml(m.title) +
        (m.is_public ? "" : ' <span class="mmr-badge">private</span>');
      var actions = document.createElement("div");
      actions.className = "mmr-actions";
      var openBtn = document.createElement("a");
      openBtn.className = "u-btn"; openBtn.textContent = "Open";
      openBtn.href = "editor.html?model=" + m.id;
      var delBtn = document.createElement("button");
      delBtn.className = "u-btn";
      delBtn.style.borderColor = "#991b1b"; delBtn.style.color = "#991b1b";
      delBtn.textContent = "Delete";
      delBtn.onclick = async function () {
        if (!confirm('Delete "' + m.title + '"? This action cannot be undone.')) return;
        try { await RTACommunity.deleteModel(m.id); loadMyModels(); loadCommunity(page, currentQuery); }
        catch (e) { alert(e); }
      };
      actions.appendChild(openBtn); actions.appendChild(delBtn);
      row.appendChild(label); row.appendChild(actions);
      list.appendChild(row);
    });
  } catch (e) {
    list.innerHTML = '<p class="text-muted">Error loading your models.</p>';
  }
}

document.addEventListener("DOMContentLoaded", function () {
  renderExamples();
  loadCommunity(1, "");
  loadMyModels();
  document.getElementById("modelModalClose").onclick = function () {
    document.getElementById("modelModal").style.display = "none";
  };
  document.getElementById("modelModal").addEventListener("click", function (e) {
    if (e.target === this) this.style.display = "none";
  });
  document.getElementById("searchBtn").onclick = function () {
    loadCommunity(1, document.getElementById("searchInput").value.trim());
  };
  document.getElementById("searchInput").addEventListener("keydown", function (e) {
    if (e.key === "Enter") document.getElementById("searchBtn").click();
  });
});
})();