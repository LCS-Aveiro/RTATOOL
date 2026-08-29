(function () {
  var tooltip = null;
  var symbolCache = {};
  var refreshTimer = null;

  function ensureTooltip() {
    if (!tooltip) {
      tooltip = document.createElement("div");
      tooltip.id = "rta-hover-tooltip";
      tooltip.style.display = "none";
      document.body.appendChild(tooltip);
    }
    return tooltip;
  }

  function escapeHtml(s) {
    return String(s)
      .replaceAll("&", "&amp;")
      .replaceAll("<", "&lt;")
      .replaceAll(">", "&gt;")
      .replaceAll("\"", "&quot;")
      .replaceAll("'", "&#039;");
  }

  function parseSymbols(code) {
    var map = {};

    function add(name, type, line, declaration, extra) {
      if (!name) return;

      if (!map[name]) {
        map[name] = {
          name: name,
          type: type,
          line: line,
          declaration: declaration,
          extra: extra || "",
          uses: []
        };
      }
    }

    var lines = code.split("\n");

    lines.forEach(function (line, index) {
      var n = index + 1;
      var m;

      // clock x
      m = line.match(/^\s*clock\s+([A-Za-z_][\w.]*)/);
      if (m) {
        add(m[1], "clock", n, line.trim());
      }

      // int / float / bool / dyn int[]
      m = line.match(/^\s*(?:dyn\s+)?(int|float|bool)(\[\])?\s+([A-Za-z_][\w.]*)/);
      if (m) {
        add(m[3], m[1] + (m[2] || ""), n, line.trim());
      }

      // def f(a,b)
      m = line.match(/^\s*def\s+([A-Za-z_][\w.]*)\s*\(([^)]*)\)/);
      if (m) {
        add(m[1], "function", n, line.trim(), "params: " + (m[2] || "").trim());
      }

      // inv s0: ...
      m = line.match(/^\s*inv\s+([A-Za-z_][\w.]*)\s*:\s*(.*)$/);
      if (m) {
        add(m[1], "state", n, line.trim());
      }

      // edges: s0 ---> s1 : label
      m = line.match(
        /^\s*([A-Za-z_][\w.]*)\s*(?:-\s*[\w.]+\s*)?(?:--->|-->|->>|--!|--x)\s*([A-Za-z_][\w.]*)(?:\s*:\s*([A-Za-z_][\w.]*))?/
      );

      if (m) {
        add(m[1], "state", n, line.trim());
        add(m[2], "state", n, line.trim());

        if (m[3]) {
          add(m[3], "label", n, line.trim());
        }
      }
    });

    lines.forEach(function (line, index) {
      Object.keys(map).forEach(function (name) {
        if (line.includes(name)) {
          map[name].uses.push({
            line: index + 1,
            text: line.trim()
          });
        }
      });
    });

    return map;
  }

  function renderTooltip(sym, x, y) {
    var el = ensureTooltip();

    var usesHtml = sym.uses
      .slice(0, 6)
      .map(function (u) {
        return `<div style="opacity:0.85;">L${u.line}: ${escapeHtml(u.text)}</div>`;
      })
      .join("");

    el.innerHTML = `
      <div style="font-weight:700; margin-bottom:4px;">
        ${escapeHtml(sym.name)}
        <span style="opacity:0.65; font-weight:400;">(${escapeHtml(sym.type)})</span>
      </div>
      <div style="opacity:0.9;">Declared line: ${sym.line}</div>
      ${sym.extra ? `<div style="opacity:0.9;">${escapeHtml(sym.extra)}</div>` : ""}
      <div style="margin-top:6px; font-family:monospace; font-size:11px; background:#f8f9fa; padding:6px; border-radius:3px;">
        ${escapeHtml(sym.declaration)}
      </div>
      ${
        sym.uses.length
          ? `<div style="margin-top:8px; font-size:11px;">${usesHtml}</div>`
          : ""
      }
    `;

    el.style.display = "block";
    el.style.left = Math.min(x + 14, window.innerWidth - 360) + "px";
    el.style.top = Math.min(y + 14, window.innerHeight - 220) + "px";
  }

  function hideTooltip() {
    if (tooltip) tooltip.style.display = "none";
  }

  function refreshCache(cm) {
    clearTimeout(refreshTimer);
    refreshTimer = setTimeout(function () {
      symbolCache = parseSymbols(cm.getValue());
    }, 250);
  }

  function attachHoverDocs(cm) {
    refreshCache(cm);

    cm.on("changes", function () {
      refreshCache(cm);
      hideTooltip();
    });

    var wrapper = cm.getWrapperElement();

    wrapper.addEventListener("mousemove", function (e) {
      var pos = cm.coordsChar({ left: e.clientX, top: e.clientY });
      var token = cm.getTokenAt(pos);

      if (!token || !token.string) {
        hideTooltip();
        return;
      }

      var word = token.string.trim();

      if (!word) {
        hideTooltip();
        return;
      }

      var sym = symbolCache[word];

      if (sym) {
        renderTooltip(sym, e.pageX, e.pageY);
      } else {
        hideTooltip();
      }
    });

    wrapper.addEventListener("mouseleave", function () {
      hideTooltip();
    });
  }

  document.addEventListener("DOMContentLoaded", function () {
    var tries = 0;

    var iv = setInterval(function () {
      tries++;

      if (typeof editor !== "undefined") {
        attachHoverDocs(editor);
        clearInterval(iv);
      } else if (tries > 50) {
        clearInterval(iv);
      }
    }, 100);
  });
})();