
(function () {
"use strict";

var STORAGE_KEY  = "re_project_v2";
var EXPANDED_KEY = "re_project_expanded_v1";
var LEGACY_KEY   = "rta_user_custom_models";

var currentFileId = null;
var dirty = false;
var suppressDirty = false;
var expanded = loadExpanded();


function loadExpanded() {
  try {
    var raw = localStorage.getItem(EXPANDED_KEY);
    if (raw) return new Set(JSON.parse(raw));
  } catch (e) {}
  return new Set(["examples-root", "my-root"]);
}
function saveExpanded() {
  try { localStorage.setItem(EXPANDED_KEY, JSON.stringify(Array.from(expanded))); } catch (e) {}
}
function loadStore() {
  try {
    var raw = localStorage.getItem(STORAGE_KEY);
    if (raw) return JSON.parse(raw);
  } catch (e) {}
  return { folders: {}, files: {} };
}
function saveStore(store) { localStorage.setItem(STORAGE_KEY, JSON.stringify(store)); }
function uid(prefix) { return prefix + "_" + Date.now().toString(36) + "_" + Math.random().toString(36).slice(2, 7); }
function editorValue() { return (typeof editor !== "undefined") ? editor.getValue() : ""; }
function setEditorValue(v) {
  if (typeof editor === "undefined") return;
  suppressDirty = true;
  editor.setValue(v);
  setTimeout(function () { suppressDirty = false; setDirty(false); }, 0);
}
function childrenOf(store, parentId) {
  var folders = Object.keys(store.folders)
    .filter(function (id) { return store.folders[id].parentId === parentId; })
    .map(function (id) { return { id: id, type: "folder", name: store.folders[id].name }; });
  var files = Object.keys(store.files)
    .filter(function (id) { return store.files[id].parentId === parentId; })
    .map(function (id) { return { id: id, type: "file", name: store.files[id].name }; });
  return folders.concat(files).sort(function (a, b) {
    if (a.type !== b.type) return a.type === "folder" ? -1 : 1;
    return a.name.localeCompare(b.name);
  });
}
function findFileByName(store, name, parentId, excludeId) {
  var lower = String(name).toLowerCase(), found = null;
  Object.keys(store.files).forEach(function (id) {
    if (excludeId && id === excludeId) return;
    var f = store.files[id];
    if ((f.parentId || null) === (parentId || null) && f.name.toLowerCase() === lower) {
      found = { id: id, file: f };
    }
  });
  return found;
}
function uniqueName(store, base, parentId, excludeId) {
  if (!findFileByName(store, base, parentId, excludeId)) return base;
  var stem = base.replace(/\.Re$/i, ""), i = 2, cand;
  do { cand = stem + " (" + i + ").Re"; i++; } while (findFileByName(store, cand, parentId, excludeId));
  return cand;
}


function setStatusName(name) {
  var sb = document.getElementById("sb-model");
  if (sb) { sb.textContent = name; sb.style.color = ""; }
}
function flashSaved(name) {
  var sb = document.getElementById("sb-model");
  if (!sb) return;
  sb.textContent = "✓ Saved: " + name;
  sb.style.color = "#86EFAC";
  setTimeout(function () { setStatusName(name); }, 900);
}
function setDirty(v) {
  if (dirty === v) return;
  dirty = v;
  updateDirtyDot();
}
function updateDirtyDot() {
  var row = document.querySelector("#project-tree .tree-item.selected");
  if (!row) return;
  var dot = row.querySelector(".tree-dirty");
  if (dirty && currentFileId && row.dataset.id === currentFileId) {
    if (!dot) {
      dot = document.createElement("span");
      dot.className = "tree-dirty";
      dot.textContent = "●";
      dot.title = "Unsaved changes";
      dot.style.cssText = "color:#d97706;font-size:9px;margin-right:4px;";
      row.insertBefore(dot, row.firstChild);
    }
  } else if (dot) dot.remove();
}
window.addEventListener("beforeunload", function (e) {
  if (dirty) { e.preventDefault(); e.returnValue = ""; }
});


function openFile(id, name, content) {
  currentFileId = id;
  setEditorValue(content);
  if (typeof showCanvasTab === "function") showCanvasTab("editorTab");
  if (typeof loadAndRender === "function") loadAndRender();
  setStatusName(name);
  renderTree();
}
function highlightSelected(id) {
  document.querySelectorAll("#project-tree .tree-item").forEach(function (el) {
    el.classList.toggle("selected", el.dataset.id === id);
  });
  updateDirtyDot();
}


function renderTree(selectName) {
  var root = document.getElementById("project-tree");
  if (!root) return;
  root.innerHTML = "";
  var store = loadStore();

  var exOpen = expanded.has("examples-root");
  var exHeader = document.createElement("div");
  exHeader.className = "tree-item";
  exHeader.style.paddingLeft = "0px";
  exHeader.style.fontWeight = "bold";
  exHeader.innerHTML =
    '<span class="glyphicon ' + (exOpen ? "glyphicon-triangle-bottom" : "glyphicon-triangle-right") + '" style="margin-right:4px;font-size:9px;"></span>' +
    '<span class="glyphicon glyphicon-book" style="margin-right:5px;"></span>Exemplos';
  exHeader.onclick = function () { toggleExpand("examples-root"); };
  root.appendChild(exHeader);
  if (exOpen && typeof RTA !== "undefined" && typeof RTA.getExamples === "function") {
    try {
      var examples = JSON.parse(RTA.getExamples());
      Object.keys(examples).forEach(function (key) {
        renderExampleNode(root, key, examples[key], 1);
      });
    } catch (e) {}
  }

  var myOpen = expanded.has("my-root");
  var myHeader = document.createElement("div");
  myHeader.className = "tree-item";
  myHeader.style.paddingLeft = "0px";
  myHeader.style.fontWeight = "bold";
  myHeader.innerHTML =
    '<span class="glyphicon ' + (myOpen ? "glyphicon-triangle-bottom" : "glyphicon-triangle-right") + '" style="margin-right:4px;font-size:9px;"></span>' +
    '<span class="glyphicon glyphicon-hdd" style="margin-right:5px;"></span>Meus Ficheiros';
  myHeader.onclick = function () { toggleExpand("my-root"); };
  myHeader.ondragover = function (e) { e.preventDefault(); };
  myHeader.ondrop = function (e) {
    e.preventDefault();
    var id = e.dataTransfer.getData("text/rta-file-id");
    if (id) moveFile(id, null);
  };
  root.appendChild(myHeader);
  if (myOpen) {
    childrenOf(store, null).forEach(function (node) {
      var row = renderNode(root, node, store, 1);
      if (node.type === "folder") attachFolderDrop(row, node.id);
    });
  }

  if (selectName) {
    var hit = findFileByName(store, selectName, null, null) ||
              Object.keys(store.files).map(function (id) { return { id: id, file: store.files[id] }; })
                .filter(function (x) { return x.file.name === selectName; })[0];
    if (hit) highlightSelected(hit.id);
  } else {
    highlightSelected(currentFileId);
  }
}
function toggleExpand(id) {
  if (expanded.has(id)) expanded.delete(id); else expanded.add(id);
  saveExpanded();
  renderTree();
}
function attachFolderDrop(row, folderId) {
  row.ondragover = function (e) { e.preventDefault(); };
  row.ondrop = function (e) {
    e.preventDefault(); e.stopPropagation();
    var id = e.dataTransfer.getData("text/rta-file-id");
    if (id) moveFile(id, folderId);
  };
}
function renderNode(container, node, store, depth) {
  var row = document.createElement("div");
  row.className = "tree-item";
  row.style.paddingLeft = (12 + depth * 14) + "px";
  row.dataset.id = node.id;
  row.dataset.type = node.type;
  row.draggable = node.type === "file";
  if (node.type === "folder") {
    var isOpen = expanded.has(node.id);
    row.innerHTML =
      '<span class="glyphicon ' + (isOpen ? "glyphicon-triangle-bottom" : "glyphicon-triangle-right") + '" style="margin-right:4px;font-size:9px;"></span>' +
      '<span class="glyphicon glyphicon-folder-' + (isOpen ? "open" : "close") + '" style="margin-right:5px;"></span>';
    row.appendChild(document.createTextNode(node.name));
    row.onclick = function () { toggleExpand(node.id); };
    row.oncontextmenu = function (e) { showFolderMenu(e, node.id, store); };
    container.appendChild(row);
    if (isOpen) {
      var kids = childrenOf(store, node.id);
      kids.forEach(function (k) { renderNode(container, k, store, depth + 1); });
      if (!kids.length) {
        var empty = document.createElement("div");
        empty.className = "tree-item tree-empty";
        empty.style.paddingLeft = (12 + (depth + 1) * 14) + "px";
        empty.style.opacity = "0.5";
        empty.style.fontStyle = "italic";
        empty.textContent = "(vazio)";
        container.appendChild(empty);
      }
    }
  } else {
    row.innerHTML = '<span class="glyphicon glyphicon-file" style="margin-right:5px;opacity:.7;"></span>';
    row.appendChild(document.createTextNode(node.name));
    row.title = node.name + " — clique direito para opções";
    row.onclick = function () {
      var f = loadStore().files[node.id];
      if (f) openFile(node.id, f.name, f.content);
    };
    row.oncontextmenu = function (e) { showFileMenu(e, node); };
    row.ondragstart = function (e) { e.dataTransfer.setData("text/rta-file-id", node.id); };
    container.appendChild(row);
  }
  return row;
}
function renderExampleNode(container, key, content, depth) {
  var row = document.createElement("div");
  row.className = "tree-item";
  row.style.paddingLeft = (12 + depth * 14) + "px";
  row.dataset.id = "example:" + key;
  row.innerHTML = '<span class="glyphicon glyphicon-file" style="margin-right:5px;opacity:.5;"></span>';
  row.appendChild(document.createTextNode(key + ".Re"));
  row.title = "Exemplo incorporado (só leitura)";
  row.onclick = function () {
    currentFileId = null;
    setEditorValue(content);
    if (typeof showCanvasTab === "function") showCanvasTab("editorTab");
    if (typeof loadAndRender === "function") loadAndRender();
    setStatusName(key + ".Re");
    renderTree();
    highlightSelected(row.dataset.id);
  };
  container.appendChild(row);
}
function moveFile(fileId, newParentId) {
  var store = loadStore();
  if (!store.files[fileId]) return;
  var f = store.files[fileId];
  var name = f.name;
  if (findFileByName(store, name, newParentId, fileId)) {
    name = uniqueName(store, name, newParentId, fileId);
    f.name = name;
  }
  f.parentId = newParentId;
  saveStore(store);
  renderTree();
}


function placeMenu(menu, x, y) {
  document.body.appendChild(menu);
  var r = menu.getBoundingClientRect();
  if (x + r.width  > window.innerWidth)  x = window.innerWidth  - r.width  - 4;
  if (y + r.height > window.innerHeight) y = window.innerHeight - r.height - 4;
  menu.style.left = x + "px";
  menu.style.top  = y + "px";
}
function showFileMenu(e, fileNode) {
  e.preventDefault();
  e.stopPropagation();
  var old = document.getElementById("file-context-menu");
  if (old) old.remove();
  var menu = document.createElement("div");
  menu.id = "file-context-menu";
  menu.className = "custom-context-menu";
  menu.style.display = "block";
  menu.innerHTML =
    "<ul>" +
    '<li data-act="open"><span class="glyphicon glyphicon-open"></span> Open</li>' +
    '<li data-act="rename"><span class="glyphicon glyphicon-pencil"></span> Rename (F2)</li>' +
    '<li data-act="dup"><span class="glyphicon glyphicon-duplicate"></span> Duplicate</li>' +
    '<li data-act="dl"><span class="glyphicon glyphicon-download-alt"></span> Download .Re</li>' +
    '<li data-act="del" style="color:#991b1b;"><span class="glyphicon glyphicon-trash"></span> Delete</li>' +
    "</ul>";
  placeMenu(menu, e.clientX, e.clientY);
  menu.addEventListener("click", function (ev) {
    var li = ev.target.closest("li");
    if (!li) return;
    var act = li.dataset.act;
    menu.remove();
    var store = loadStore();
    var f = store.files[fileNode.id];
    if (!f) return;
    if (act === "open")        openFile(fileNode.id, f.name, f.content);
    else if (act === "rename") renameFile(fileNode.id);
    else if (act === "dup")    duplicateFile(fileNode.id);
    else if (act === "dl")     downloadFile(fileNode.id);
    else if (act === "del")    deleteFileById(fileNode.id);
  });
  setTimeout(function () {
    document.addEventListener("click", function close() {
      menu.remove();
      document.removeEventListener("click", close);
    });
  }, 0);
}
function renameFile(id) {
  var store = loadStore();
  var f = store.files[id];
  if (!f) return;
  var newName = prompt("New name:", f.name);
  if (!newName || newName === f.name) return;
  if (!/\.Re$/i.test(newName)) newName += ".Re";
  if (findFileByName(store, newName, f.parentId, id)) {
    alert("Já existe um ficheiro com esse nome nesta pasta.");
    return;
  }
  f.name = newName;
  saveStore(store);
  renderTree();
  if (currentFileId === id) setStatusName(newName);
}
function duplicateFile(id) {
  var store = loadStore();
  var f = store.files[id];
  if (!f) return;
  var name = uniqueName(store, f.name.replace(/\.Re$/i, "") + " copy.Re", f.parentId, null);
  var nid = uid("file");
  store.files[nid] = { name: name, parentId: f.parentId, content: f.content };
  saveStore(store);
  if (f.parentId) expanded.add(f.parentId); else expanded.add("my-root");
  saveExpanded();
  renderTree(name);
}
function downloadFile(id) {
  var store = loadStore();
  var f = store.files[id];
  if (!f) return;
  var blob = new Blob([f.content], { type: "text/plain" });
  var a = document.createElement("a");
  a.href = URL.createObjectURL(blob);
  a.download = f.name;
  document.body.appendChild(a);
  a.click();
  a.remove();
  URL.revokeObjectURL(a.href);
}
function deleteFileById(id) {
  var store = loadStore();
  var f = store.files[id];
  if (!f) return;
  if (!confirm('Apagar "' + f.name + '"? Esta ação não pode ser desfeita.')) return;
  delete store.files[id];
  saveStore(store);
  if (currentFileId === id) {
    currentFileId = null;
    setDirty(false);
    setStatusName("No model loaded");
  }
  renderTree();
}


function showFolderMenu(e, folderId, store) {
  e.preventDefault();
  e.stopPropagation();
  var old = document.getElementById("folder-context-menu");
  if (old) old.remove();
  var menu = document.createElement("div");
  menu.id = "folder-context-menu";
  menu.className = "custom-context-menu";
  menu.style.display = "block";
  menu.innerHTML =
    "<ul>" +
    '<li data-act="new-file"><span class="glyphicon glyphicon-file"></span> New file here</li>' +
    '<li data-act="new-folder"><span class="glyphicon glyphicon-folder-open"></span> New subfolder</li>' +
    '<li data-act="rename"><span class="glyphicon glyphicon-pencil"></span> Rename</li>' +
    '<li data-act="delete" style="color:#991b1b;"><span class="glyphicon glyphicon-trash"></span> Delete folder</li>' +
    "</ul>";
  placeMenu(menu, e.clientX, e.clientY);
  menu.addEventListener("click", function (ev) {
    var li = ev.target.closest("li");
    if (!li) return;
    var act = li.dataset.act;
    var s = loadStore();
    if (act === "new-file") {
      var fname = prompt("New file name:", "novo.Re");
      if (fname) {
        if (!/\.Re$/i.test(fname)) fname += ".Re";
        var fid = uid("file");
        s.files[fid] = { name: fname, parentId: folderId, content: "name " + fname.replace(/\.Re$/i, "") + "\ninit start\n" };
        saveStore(s);
        expanded.add(folderId); saveExpanded();
        renderTree();
      }
    } else if (act === "new-folder") {
      var fdname = prompt("Subfolder name:", "Nova pasta");
      if (fdname) {
        s.folders[uid("folder")] = { name: fdname, parentId: folderId };
        saveStore(s);
        expanded.add(folderId); saveExpanded();
        renderTree();
      }
    } else if (act === "rename") {
      var newName = prompt("New name:", s.folders[folderId].name);
      if (newName) { s.folders[folderId].name = newName; saveStore(s); renderTree(); }
    } else if (act === "delete") {
      if (confirm('Apagar a pasta "' + s.folders[folderId].name + '" e todo o seu conteúdo?')) {
        deleteFolderRecursive(s, folderId);
        saveStore(s);
        renderTree();
      }
    }
    menu.remove();
  });
  setTimeout(function () {
    document.addEventListener("click", function close() {
      menu.remove();
      document.removeEventListener("click", close);
    });
  }, 0);
}
function deleteFolderRecursive(store, folderId) {
  Object.keys(store.folders).forEach(function (id) {
    if (store.folders[id].parentId === folderId) deleteFolderRecursive(store, id);
  });
  Object.keys(store.files).forEach(function (id) {
    if (store.files[id].parentId === folderId) delete store.files[id];
  });
  delete store.folders[folderId];
}


window.saveUserModel = function () {
  var store = loadStore();
  if (currentFileId && store.files[currentFileId]) {
    store.files[currentFileId].content = editorValue();
    saveStore(store);
    setDirty(false);
    renderTree();
    flashSaved(store.files[currentFileId].name);
    return;
  }
  window.overwriteUserModel();
};

window.overwriteUserModel = function () {
  var store = loadStore();
  var def = (currentFileId && store.files[currentFileId]) ? store.files[currentFileId].name : "modelo.Re";
  var name = prompt("Guardar como (.Re):", def);
  if (!name) return;
  if (!/\.Re$/i.test(name)) name += ".Re";
  var existing = findFileByName(store, name, null, null);
  if (existing && existing.id !== currentFileId) {
    if (!confirm("Já existe '" + name + "' na raiz. Substituir o conteúdo dele?")) return;
    existing.file.content = editorValue();
    currentFileId = existing.id;
  } else if (existing) {
    existing.file.content = editorValue();
  } else {
    var id = uid("file");
    store.files[id] = { name: name, parentId: null, content: editorValue() };
    currentFileId = id;
    expanded.add("my-root"); saveExpanded();
  }
  saveStore(store);
  setDirty(false);
  renderTree();
  setStatusName(name);
  flashSaved(name);
};

window.deleteUserModel = function () {
  if (!currentFileId) { alert("Nenhum ficheiro guardado está aberto."); return; }
  deleteFileById(currentFileId);
};

window.createNewModel = function () {
  var name = prompt("Nome do novo ficheiro (.Re):", "novo.Re");
  if (!name) return;
  if (!/\.Re$/i.test(name)) name += ".Re";
  var store = loadStore();
  var nameObj = findFileByName(store, name, null, null);
  if (nameObj) { alert("Já existe um ficheiro com esse nome na raiz."); return; }
  var id = uid("file");
  var template = "name " + name.replace(/\.Re$/i, "") + "\ninit start\n";
  store.files[id] = { name: name, parentId: null, content: template };
  saveStore(store);
  expanded.add("my-root"); saveExpanded();
  openFile(id, name, template);
};

window.updateProjectTree = function (selectName) { renderTree(selectName); };


function isTypingTarget(t) {
  if (!t) return false;
  var tag = (t.tagName || "").toLowerCase();
  if (tag === "input" || tag === "textarea" || tag === "select") return true;
  return !!(t.closest && t.closest(".CodeMirror"));
}
document.addEventListener("keydown", function (e) {
  var key = (e.key || "").toLowerCase();
  if ((e.ctrlKey || e.metaKey) && key === "s" && !e.shiftKey) {
    e.preventDefault();
    window.saveUserModel();
    return;
  }
  if ((e.ctrlKey || e.metaKey) && key === "s" && e.shiftKey) {
    e.preventDefault();
    window.overwriteUserModel();
    return;
  }
  if (e.key === "F2" && !isTypingTarget(e.target) && currentFileId) {
    e.preventDefault();
    renameFile(currentFileId);
  }
});
function attachDirtyTracking() {
  var tries = 0;
  var iv = setInterval(function () {
    tries++;
    if (typeof editor !== "undefined") {
      editor.on("changes", function () { if (!suppressDirty) setDirty(true); });
      clearInterval(iv);
    } else if (tries > 100) clearInterval(iv);
  }, 100);
}
function migrateLegacy() {
  try {
    var raw = localStorage.getItem(LEGACY_KEY);
    if (!raw) return;
    var legacy = JSON.parse(raw) || {};
    var store = loadStore();
    var count = 0;
    Object.keys(legacy).forEach(function (name) {
      if (!findFileByName(store, name + ".Re", null, null)) {
        store.files[uid("file")] = { name: name + ".Re", parentId: null, content: legacy[name] };
        count++;
      }
    });
    if (count) saveStore(store);
    localStorage.removeItem(LEGACY_KEY);
  } catch (e) {}
}
document.addEventListener("DOMContentLoaded", function () {
  migrateLegacy();
  var menu = document.getElementById("project-context-menu");
  if (menu) {
    var ul = menu.querySelector("ul");
    if (ul && !ul.querySelector('[data-act="new-root-folder"]')) {
      var li = document.createElement("li");
      li.dataset.act = "new-root-folder";
      li.innerHTML = '<span class="glyphicon glyphicon-folder-close"></span> New folder';
      li.onclick = function () {
        var name = prompt("Nome da pasta:", "Nova pasta");
        if (name) {
          var store = loadStore();
          store.folders[uid("folder")] = { name: name, parentId: null };
          saveStore(store);
          expanded.add("my-root"); saveExpanded();
          renderTree();
        }
      };
      ul.appendChild(li);
    }
  }
  renderTree();
  attachDirtyTracking();
});
})();