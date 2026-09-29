
(function () {
"use strict";

var API = "api/";

async function callJson(path, body) {
  var r = await fetch(API + path, {
    method: "POST",
    credentials: "same-origin",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(body || {})
  });
  var data = {};
  try { data = await r.json(); } catch (e) {}
  if (!r.ok) throw (data.error || "Unknown error.");
  return data;
}

window.RTACommunity = {
  async me() {
    var r = await fetch(API + "auth.php?action=me", { credentials: "same-origin" });
    return r.json();
  },
  async register(username, email, password) {
    return callJson("auth.php?action=register", { username: username, email: email, password: password });
  },
  async login(login, password) {
    return callJson("auth.php?action=login", { login: login, password: password });
  },
  async logout() {
    return callJson("auth.php?action=logout", {});
  },
  async listModels(q, page) {
    var url = API + "models.php?action=list&q=" + encodeURIComponent(q || "") + "&page=" + (page || 1);
    var r = await fetch(url, { credentials: "same-origin" });
    return r.json();
  },
  async myModels() {
    var r = await fetch(API + "models.php?action=mine", { credentials: "same-origin" });
    return r.json();
  },
  async getModel(id) {
    var r = await fetch(API + "models.php?action=get&id=" + encodeURIComponent(id), { credentials: "same-origin" });
    var data = await r.json();
    if (!r.ok) throw (data.error || "Error loading model.");
    return data;
  },
  async createModel(payload) { return callJson("models.php?action=create", payload); },
  async updateModel(id, payload) { return callJson("models.php?action=update&id=" + encodeURIComponent(id), payload); },
  async deleteModel(id) { return callJson("models.php?action=delete&id=" + encodeURIComponent(id), {}); }
};

window.RTAIcon = function (name) {
  var P = {
    user:   '<path d="M8 2a3 3 0 1 1 0 6 3 3 0 0 1 0-6zm0 7c3.3 0 6 1.6 6 3.6V14H2v-1.4C2 10.6 4.7 9 8 9z"/>',
    eye:    '<path fill-rule="evenodd" d="M8 3C4.8 3 2 5.5 1 8c1 2.5 3.8 5 7 5s6-2.5 7-5c-1-2.5-3.8-5-7-5zm0 7.5A2.5 2.5 0 1 1 8 5.5a2.5 2.5 0 0 1 0 5z"/>',
    box:    '<path fill-rule="evenodd" d="M8 1 2 4v8l6 3 6-3V4zm0 1.9 3.9 1.95L8 6.8 4.1 4.85zM3.6 6.2 7.2 8v4.6L3.6 10.8zm8.8 0v4.6L8.8 12.6V8z"/>',
    folder: '<path d="M1.5 3h5L8 5h6.5v8.5h-13z"/>',
    search: '<path fill-rule="evenodd" d="M7 1a6 6 0 1 1 0 12A6 6 0 0 1 7 1zm0 2a4 4 0 1 0 0 8 4 4 0 0 0 0-8zm4.9 7.5 3 3-1.4 1.4-3-3z"/>',
    plus:   '<path d="M7 1h2v6h6v2H9v6H7V9H1V7h6z"/>',
    cal:    '<path fill-rule="evenodd" d="M2 3h12v11H2zm1.5 3v6.5h9V6z"/><path d="M5 1.5h1.2v3H5zm4.8 0H11v3H9.8z"/>',
    ext:    '<path d="M6 2h8v8h-2V5.4L5.7 11.7 4.3 10.3 10.6 4H6z"/>'
  };
  return '<svg class="ic" viewBox="0 0 16 16" aria-hidden="true" focusable="false">' + (P[name] || "") + '</svg>';
};


function renderAccountWidget(user) {
  var el = document.getElementById("account-widget");
  if (!el) return;
  if (user && user.id) {
    el.innerHTML =
      '<span class="acc-user">' + RTAIcon("user") + escapeHtml(user.username) + '</span>' +
      '<button class="acc-btn" id="logoutBtn">Sign out</button>';
    document.getElementById("logoutBtn").onclick = async function () {
      try { await RTACommunity.logout(); } catch (e) {}
      window.location.reload();
    };
  } else {
    el.innerHTML = '<button class="acc-btn" id="openAuthBtn">' + RTAIcon("user") + 'Sign in / Create account</button>';
    document.getElementById("openAuthBtn").onclick = function () { openAuthModal("login"); };
  }
}

function escapeHtml(s) {
  var d = document.createElement("div");
  d.innerText = (s === null || s === undefined) ? "" : s;
  return d.innerHTML;
}

function openAuthModal(tab) {
  var modal = document.getElementById("authModal");
  if (!modal) return;
  modal.style.display = "flex";
  document.querySelectorAll(".rta-tab").forEach(function (t) {
    t.classList.toggle("active", t.dataset.tab === tab);
  });
  document.querySelectorAll(".rta-modal-pane").forEach(function (p) {
    p.classList.toggle("active", p.id === tab + "Pane");
  });
  setTimeout(function () {
    var first = document.querySelector("#" + tab + "Pane input");
    if (first) first.focus();
  }, 60);
}
window.openAuthModal = openAuthModal;


function forceAutocomplete() {
  function set(id, attrs) {
    var el = document.getElementById(id);
    if (!el) return;
    for (var k in attrs) el.setAttribute(k, attrs[k]);
  }
  set("loginUser", { autocomplete: "username", name: "username" });
  set("loginPass", { autocomplete: "current-password", name: "password" });
  set("regUser",   { autocomplete: "username", name: "username" });
  set("regEmail",  { autocomplete: "email", name: "email" });
  set("regPass",   { autocomplete: "new-password", name: "new-password" });
}

function initAuthModal() {
  var modal = document.getElementById("authModal");
  if (!modal) return;
  forceAutocomplete();
  document.getElementById("authModalClose").onclick = function () { modal.style.display = "none"; };
  modal.addEventListener("click", function (e) { if (e.target === modal) modal.style.display = "none"; });
  document.querySelectorAll(".rta-tab").forEach(function (t) {
    t.onclick = function () { openAuthModal(t.dataset.tab); };
  });

  async function doLogin() {
    var login = document.getElementById("loginUser").value.trim();
    var pass = document.getElementById("loginPass").value;
    var errEl = document.getElementById("loginError");
    errEl.textContent = "";
    if (!login || !pass) { errEl.textContent = "Fill in all fields."; return; }
    try {
      await RTACommunity.login(login, pass);
      window.location.reload(); 
    } catch (e) { errEl.textContent = e; }
  }
  async function doRegister() {
    var username = document.getElementById("regUser").value.trim();
    var email = document.getElementById("regEmail").value.trim();
    var pass = document.getElementById("regPass").value;
    var errEl = document.getElementById("registerError");
    errEl.textContent = "";
    if (!username || !email || !pass) { errEl.textContent = "Fill in all fields."; return; }
    try {
      await RTACommunity.register(username, email, pass);
      window.location.reload();
    } catch (e) { errEl.textContent = e; }
  }

  var loginForm = document.getElementById("loginForm");
  var regForm = document.getElementById("registerForm");
  if (loginForm) {
    loginForm.addEventListener("submit", function (e) { e.preventDefault(); doLogin(); });
  } else {
    document.getElementById("loginBtn").onclick = doLogin;
  }
  if (regForm) {
    regForm.addEventListener("submit", function (e) { e.preventDefault(); doRegister(); });
  } else {
    document.getElementById("registerBtn").onclick = doRegister;
  }
}

document.addEventListener("DOMContentLoaded", async function () {
  initAuthModal();
  try {
    var user = await RTACommunity.me();
    window.RTA_CURRENT_USER = (user && user.id) ? user : null;
  } catch (e) {
    window.RTA_CURRENT_USER = null;
  }
  renderAccountWidget(window.RTA_CURRENT_USER);
  document.dispatchEvent(new CustomEvent("rta-auth-ready", { detail: window.RTA_CURRENT_USER }));
});
})();