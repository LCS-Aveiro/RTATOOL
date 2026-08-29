(function () {
  'use strict';

  var watchExpressions = [];
  var breakpoints = [];
  var breakpointPaused = false;
  var handlingBreakpoint = false;
  var mutedConditions = {};


  window.addWatchExpression = function (expr) {
    expr = expr.trim();
    if (!expr) return;
    if (watchExpressions.some(w => w.expr === expr)) return;
    watchExpressions.push({ expr: expr });
    saveWatchState();
    refreshWatches();
    renderWatchPanel();
  };

  window.removeWatchExpression = function (index) {
    watchExpressions.splice(index, 1);
    saveWatchState();
    refreshWatches();
    renderWatchPanel();
  };

  window.refreshWatches = function () {
    if (watchExpressions.length === 0) return;
    var container = document.getElementById('watch-list');
    if (!container) return;
    watchExpressions.forEach(function (w, i) {
      var el = document.getElementById('watch-val-' + i);
      if (!el) return;
      try {
        var result = JSON.parse(RTA.evalWatchExpression(w.expr));
        el.textContent = formatWatchValue(result);
        el.className = 'watch-value ' + getValueClass(result);
        el.title = result.value !== undefined ? String(result.value) : '';
      } catch (e) {
        el.textContent = 'ERR';
        el.className = 'watch-value val-error';
      }
    });
  };

  function formatWatchValue(result) {
    if (result.type === 'error') return '⚠';
    if (result.type === 'bool') return result.value ? '✓ true' : '✗ false';
    if (result.type === 'float') return Number(result.value).toFixed(2);
    return String(result.value);
  }

  function getValueClass(result) {
    if (result.type === 'error') return 'val-error';
    if (result.type === 'bool') return result.value ? 'val-true' : 'val-false';
    return 'val-number';
  }

  window.renderWatchPanel = function () {
    var container = document.getElementById('watch-panel-container');
    if (!container) return;
    var html = '<div class="watch-panel">';
    html += '<div class="watch-header"><span>👁 Watch</span><span>' + watchExpressions.length + '</span></div>';
    html += '<div class="watch-list" id="watch-list">';
    if (watchExpressions.length === 0) {
      html += '<div style="padding:8px 10px; color:#9ca3af; font-size:11px;">Sem expressões. Adiciona abaixo.</div>';
    } else {
      watchExpressions.forEach(function (w, i) {
        html += '<div class="watch-item">';
        html += '<span class="watch-expr" title="' + escHtml(w.expr) + '">' + escHtml(w.expr) + '</span>';
        html += '<span class="watch-value" id="watch-val-' + i + '">…</span>';
        html += '<span class="watch-remove" onclick="removeWatchExpression(' + i + ')">×</span>';
        html += '</div>';
      });
    }
    html += '</div>';
    html += '<div class="watch-input-row">';
    html += '<input type="text" id="watch-input" placeholder="ex: t >= 5, x + 1, floor(t)" ';
    html += 'onkeydown="if(event.key===\'Enter\'){addWatchExpression(this.value);this.value=\'\';}">';
    html += '<button onclick="addWatchExpression(document.getElementById(\'watch-input\').value);document.getElementById(\'watch-input\').value=\'\';">+</button>';
    html += '</div></div>';
    container.innerHTML = html;
  };


  window.addBreakpoint = function (cond) {
    cond = cond.trim();
    if (!cond) return;
    if (breakpoints.some(b => b.condition === cond)) return;
    breakpoints.push({ condition: cond, enabled: true });
    delete mutedConditions[cond];
    saveBreakpointState();
    renderBreakpointPanel();
  };

  window.removeBreakpoint = function (index) {
    if (breakpoints[index]) delete mutedConditions[breakpoints[index].condition];
    breakpoints.splice(index, 1);
    saveBreakpointState();
    renderBreakpointPanel();
  };

  window.toggleBreakpoint = function (index) {
    breakpoints[index].enabled = !breakpoints[index].enabled;
    delete mutedConditions[breakpoints[index].condition];
    saveBreakpointState();
    renderBreakpointPanel();
  };

  window.renderBreakpointPanel = function () {
    var container = document.getElementById('bp-panel-container');
    if (!container) return;
    var html = '<div class="bp-panel">';
    html += '<div class="bp-header"><span>⏸ Breakpoints</span><span>' + breakpoints.filter(b => b.enabled).length + ' ativos</span></div>';
    html += '<div class="bp-list">';
    if (breakpoints.length === 0) {
      html += '<div style="padding:6px 10px; color:#9ca3af; font-size:11px;">Sem breakpoints.</div>';
    } else {
      breakpoints.forEach(function (bp, i) {
        html += '<div class="bp-item">';
        html += '<input type="checkbox" class="bp-toggle" ' + (bp.enabled ? 'checked' : '') + ' onchange="toggleBreakpoint(' + i + ')">';
        html += '<span class="bp-cond' + (bp.enabled ? '' : ' disabled') + '">' + escHtml(bp.condition) + '</span>';
        html += '<span class="watch-remove" onclick="removeBreakpoint(' + i + ')">×</span>';
        html += '</div>';
      });
    }
    html += '</div>';
    html += '<div class="watch-input-row">';
    html += '<input type="text" id="bp-input" placeholder="ex: counter > 5, t >= 50" ';
    html += 'onkeydown="if(event.key===\'Enter\'){addBreakpoint(this.value);this.value=\'\';}">';
    html += '<button onclick="addBreakpoint(document.getElementById(\'bp-input\').value);document.getElementById(\'bp-input\').value=\'\';">+</button>';
    html += '</div></div>';
    container.innerHTML = html;
  };


  window.checkBreakpointsAfterStep = function () {
    if (handlingBreakpoint) return false;
    if (breakpoints.length === 0) return false;

    var activeBps = breakpoints.filter(b => b.enabled);
    if (activeBps.length === 0) return false;

    handlingBreakpoint = true;
    try {
      var result = JSON.parse(RTA.checkBreakpoints(JSON.stringify(activeBps)));
      var triggeredNow = (result && result.triggered) ? result.conditions : [];

      Object.keys(mutedConditions).forEach(function (c) {
        if (triggeredNow.indexOf(c) === -1) delete mutedConditions[c];
      });

      var fresh = triggeredNow.filter(function (c) { return !mutedConditions[c]; });

      if (fresh.length > 0) {
        breakpointPaused = true;


        if (typeof autoPlayTimer !== 'undefined' && autoPlayTimer) {
          clearInterval(autoPlayTimer);
          autoPlayTimer = null;
        }
        if (typeof stopAutoDelay === 'function') stopAutoDelay();

        fresh.forEach(function (c) { mutedConditions[c] = true; });

        if (typeof renderGlobalPanel === 'function' &&
            typeof lastModelData !== 'undefined' && lastModelData) {
          renderGlobalPanel(lastModelData, 'sidePanel');
          renderGlobalPanel(lastModelData, 'sidePanel-bottom');
        }

        var panel = document.querySelector('.bp-panel');
        if (panel) {
          panel.classList.add('bp-triggered-flash');
          setTimeout(function () { panel.classList.remove('bp-triggered-flash'); }, 2000);
        }

        var msg = '⏸ Breakpoint atingido!\n' + fresh.map(function (c) { return '• ' + c; }).join('\n');
        showBreakpointToast(msg);
        console.log('[RTA Breakpoint]', msg);
        handlingBreakpoint = false;
        return true;
      }
    } catch (e) {
      console.error('[Breakpoint check error]', e);
    }
    handlingBreakpoint = false;
    return false;
  };

  window.isBreakpointPaused = function () {
    return breakpointPaused;
  };

  window.resumeFromBreakpoint = function () {
    breakpointPaused = false;

  };

  function showBreakpointToast(msg) {
    var existing = document.getElementById('bp-toast');
    if (existing) existing.remove();

    var toast = document.createElement('div');
    toast.id = 'bp-toast';
    toast.style.cssText = 'position:fixed; top:60px; right:20px; z-index:99999; background:#92400e; color:white; padding:12px 16px; border-radius:6px; font-size:12px; font-family:IBM Plex Sans,sans-serif; box-shadow:0 4px 20px rgba(0,0,0,0.3); max-width:350px; white-space:pre-line;';
    toast.innerHTML = escHtml(msg) + '<br><br><button onclick="resumeFromBreakpoint();document.getElementById(\'bp-toast\').remove();" style="background:#fbbf24;color:#000;border:none;padding:4px 12px;border-radius:3px;cursor:pointer;font-weight:600;">Continuar ▶</button>';
    document.body.appendChild(toast);

    setTimeout(function () {
      var t = document.getElementById('bp-toast');
      if (t) t.remove();
    }, 8000);
  }


  function saveWatchState() {
    try { localStorage.setItem('rta_watches', JSON.stringify(watchExpressions)); } catch (e) {}
  }
  function loadWatchState() {
    try {
      var saved = localStorage.getItem('rta_watches');
      if (saved) watchExpressions = JSON.parse(saved);
    } catch (e) { watchExpressions = []; }
  }
  function saveBreakpointState() {
    try { localStorage.setItem('rta_breakpoints', JSON.stringify(breakpoints)); } catch (e) {}
  }
  function loadBreakpointState() {
    try {
      var saved = localStorage.getItem('rta_breakpoints');
      if (saved) breakpoints = JSON.parse(saved);
    } catch (e) { breakpoints = []; }
  }

  function escHtml(str) {
    return String(str)
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;');
  }


  document.addEventListener('DOMContentLoaded', function () {
    loadWatchState();
    loadBreakpointState();
    var tries = 0;
    var iv = setInterval(function () {
      tries++;
      var simPanel = document.getElementById('rpane-sim');
      if (simPanel) {
        if (!document.getElementById('watch-panel-container')) {
          var watchContainer = document.createElement('div');
          watchContainer.id = 'watch-panel-container';
          simPanel.appendChild(watchContainer);
          var bpContainer = document.createElement('div');
          bpContainer.id = 'bp-panel-container';
          bpContainer.style.marginTop = '8px';
          simPanel.appendChild(bpContainer);
          renderWatchPanel();
          renderBreakpointPanel();
        }
        clearInterval(iv);
      } else if (tries > 50) {
        clearInterval(iv);
      }
    }, 100);
  });
})();