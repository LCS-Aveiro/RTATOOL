(function () {
  "use strict";

  const MAX_STEPS = 1000;

  const state = {
    steps: [],
    currentIndex: -1,
    viewIndex: -1,
    pendingLabel: "Start",
    skipNext: false,
    replaying: false,
    playing: false,
    timer: null,
    speed: 1000,
    filter: "",
    showClocks: true,
    showVars: true,
    chart: null
  };

  let originalUpdateAllViews = null;

  const palette = [
    "#2563eb",
    "#dc2626",
    "#16a34a",
    "#d97706",
    "#7c3aed",
    "#0891b2",
    "#db2777",
    "#65a30d",
    "#0f766e",
    "#9333ea"
  ];

  const eventMarkerPlugin = {
  id: "traceEventMarkers",
  afterDatasetsDraw(chart) {
    const events = chart.$traceEvents;
    if (!events || !events.length || !chart.chartArea) return;

    const ctx = chart.ctx;
    const area = chart.chartArea;
    let lastLabelX = -Infinity;

    ctx.save();
    ctx.lineWidth = 1;
    ctx.strokeStyle = "rgba(220, 38, 38, 0.22)";
    ctx.fillStyle = "rgba(220, 38, 38, 0.75)";
    ctx.font = "9px IBM Plex Sans, sans-serif";

    events.slice(0, 80).forEach((ev) => {
      const x = chart.scales.x.getPixelForValue(ev.x);
      if (x < area.left || x > area.right) return;

      ctx.beginPath();
      ctx.moveTo(x, area.top);
      ctx.lineTo(x, area.bottom);
      ctx.stroke();

      if (x - lastLabelX > 14) {
        ctx.save();
        ctx.translate(x + 3, area.top + 4);
        ctx.rotate(Math.PI / 2);
        ctx.fillText(ev.label, 0, 0);
        ctx.restore();
        lastLabelX = x;
      }
    });

    ctx.restore();
  }
};

  function esc(value) {
    return String(value ?? "")
      .replaceAll("&", "&amp;")
      .replaceAll("<", "&lt;")
      .replaceAll(">", "&gt;")
      .replaceAll('"', "&quot;")
      .replaceAll("'", "&#039;");
  }

  function delayFromLabel(label) {
    const m = String(label || "").match(/delay\s*\(\s*([0-9]*\.?[0-9]+)/i);
    return m ? parseFloat(m[1]) : 0;
  }

  function isNumeric(value) {
    const n = Number(value);
    return Number.isFinite(n);
  }

  function visibleSteps() {
    const f = state.filter.trim().toLowerCase();

    const all = state.steps.map((step, originalIndex) => ({
      step,
      originalIndex
    }));

    if (!f) return all;

    return all.filter((item) =>
      String(item.step.label).toLowerCase().includes(f)
    );
  }

  function setReplayMode(on) {
    state.replaying = on;

    const sidePanel = document.getElementById("sidePanel");
    if (sidePanel) {
      sidePanel.classList.toggle("trace-replay-disabled", on);
    }
  }

  function record(json) {
    let data = null;

    try {
      data = typeof json === "string" ? JSON.parse(json) : json;
    } catch (err) {
      return;
    }

    if (!data || data.error || !data.panelData) return;

    let label = state.pendingLabel;

    if (!label && data.lastTransition && data.lastTransition.label) {
      label = data.lastTransition.label;
    }

    if (!label) {
      label = "step";
    }

    const prevTime = state.steps.length
      ? state.steps[state.steps.length - 1].time
      : 0;

    const step = {
      label,
      time: prevTime + delayFromLabel(label),
      panelData: data.panelData,
      data
    };

    state.steps.push(step);

    if (state.steps.length > MAX_STEPS) {
      state.steps.shift();
    }

    state.currentIndex = state.steps.length - 1;

    const visible = visibleSteps();
    state.viewIndex = visible.length ? visible.length - 1 : -1;

    state.pendingLabel = "";

    renderAll();
  }

  function showByViewIndex(index) {
    const list = visibleSteps();

    if (!list.length) return;

    index = Math.max(0, Math.min(index, list.length - 1));

    state.viewIndex = index;
    state.currentIndex = list[index].originalIndex;

    setReplayMode(true);

    if (originalUpdateAllViews) {
      originalUpdateAllViews(JSON.stringify(list[index].step.data));
    }

    renderAll();
  }

  function goLive() {
    stopPlay();
    setReplayMode(false);

    if (!state.steps.length) return;

    state.currentIndex = state.steps.length - 1;

    const visible = visibleSteps();
    state.viewIndex = visible.length ? visible.length - 1 : -1;

    if (originalUpdateAllViews) {
      originalUpdateAllViews(JSON.stringify(state.steps[state.currentIndex].data));
    }

    renderAll();
  }

  function next() {
    const list = visibleSteps();
    if (!list.length) return;
    showByViewIndex(state.viewIndex + 1);
  }

  function prev() {
    const list = visibleSteps();
    if (!list.length) return;
    showByViewIndex(state.viewIndex - 1);
  }

  function first() {
    showByViewIndex(0);
  }

  function last() {
    const list = visibleSteps();
    showByViewIndex(list.length - 1);
  }

  function play() {
    if (state.playing) return;

    const list = visibleSteps();
    if (!list.length) return;

    if (state.viewIndex >= list.length - 1) {
      state.viewIndex = -1;
    }

    state.playing = true;
    updateControls();
    tick();
  }

  function tick() {
    if (!state.playing) return;

    const list = visibleSteps();

    if (!list.length) {
      stopPlay();
      return;
    }

    const nextIndex = state.viewIndex + 1;

    if (nextIndex >= list.length) {
      stopPlay();
      return;
    }

    showByViewIndex(nextIndex);

    if (state.playing) {
      state.timer = setTimeout(tick, state.speed);
    }
  }

  function stopPlay() {
    state.playing = false;

    if (state.timer) {
      clearTimeout(state.timer);
      state.timer = null;
    }

    updateControls();
  }

  function togglePlay() {
    if (state.playing) stopPlay();
    else play();
  }

  function updateControls() {
    const playBtn = document.getElementById("tracePlayBtn");
    const counter = document.getElementById("traceCounter");
    const range = document.getElementById("traceRange");

    const list = visibleSteps();

    if (playBtn) {
      playBtn.innerHTML = state.playing
        ? '<span class="glyphicon glyphicon-pause"></span> Pause'
        : '<span class="glyphicon glyphicon-play"></span> Play';
    }

    if (counter) {
      if (!list.length) {
        counter.textContent = "0/0";
      } else {
        counter.textContent =
          `${Math.max(0, state.viewIndex + 1)}/${list.length} ` +
          `(${state.steps.length} total)`;
      }
    }

    if (range) {
      range.max = Math.max(0, list.length - 1);
      range.value = state.viewIndex >= 0 ? state.viewIndex : 0;
    }
  }

  function renderList() {
    const listEl = document.getElementById("traceList");
    if (!listEl) return;

    const list = visibleSteps();

    if (!list.length) {
      listEl.innerHTML = `
        <div style="padding:10px; color:#6b7280; font-size:11px;">
          Ainda não há passos gravados.
        </div>
      `;
      return;
    }

    listEl.innerHTML = list
      .map((item, i) => {
        const clocks = Object.entries(item.step.panelData.clocks || {})
          .map(([k, v]) => `${k}=${Number(v).toFixed(2)}`)
          .join(" ");

        const active = i === state.viewIndex ? "active" : "";

        return `
          <div class="trace-item ${active}" data-trace-index="${i}">
            <div class="trace-item-title">
              #${item.originalIndex} — ${esc(item.step.label)}
            </div>
            <div class="trace-item-sub">
              t=${item.step.time.toFixed(2)}
              ${clocks ? " · " + esc(clocks) : ""}
            </div>
          </div>
        `;
      })
      .join("");

    const activeEl = listEl.querySelector(".trace-item.active");
    if (activeEl) {
      activeEl.scrollIntoView({ block: "nearest" });
    }
  }

  function ensureChart() {
    const canvas = document.getElementById("traceChart");
    if (!canvas || !window.Chart) return;

    if (state.chart) return;

    if (!canvas.parentElement.classList.contains("trace-chart-inner")) {
    const inner = document.createElement("div");
    inner.className = "trace-chart-inner";
    canvas.parentNode.appendChild(inner);
    inner.appendChild(canvas);
  }

    state.chart = new Chart(canvas, {
      type: "line",
      data: {
        datasets: []
      },
      options: {
        responsive: true,
        maintainAspectRatio: false,
        animation: false,
        interaction: {
          mode: "nearest",
          axis: "x",
          intersect: false
        },
        scales: {
          x: {
            type: "linear",
            title: {
              display: true,
              text: "passo"
            },
            ticks: {
              font: {
                size: 10
              }
            }
          },
          y: {
            title: {
              display: true,
              text: "valor"
            },
            ticks: {
              font: {
                size: 10
              }
            }
          }
        },
        plugins: {
          legend: {
            labels: {
              boxWidth: 12,
              font: {
                size: 10
              }
            }
          }
        }
      },
      plugins: [eventMarkerPlugin]
    });
  }

  function updateChart() {
    const wrap = document.querySelector(".trace-chart-wrap");

    if (!wrap) return;

    if (!window.Chart) {
      wrap.innerHTML = `
        <div class="trace-chart-message">
          Chart.js não está carregado.
        </div>
      `;
      return;
    }

    ensureChart();

    if (!state.chart) return;

    const list = visibleSteps();

    if (!list.length) {
      state.chart.data.datasets = [];
      state.chart.$traceEvents = [];
      state.chart.update();
      return;
    }

    const useTime = list[list.length - 1].step.time > 0;

    const xOf = (item, index) => {
      return useTime ? item.step.time : index;
    };

    const clockKeys = new Set();
    const varKeys = new Set();

    list.forEach(({ step }) => {
      if (state.showClocks) {
        Object.keys(step.panelData.clocks || {}).forEach((k) => {
          clockKeys.add(k);
        });
      }

      if (state.showVars) {
        Object.entries(step.panelData.variables || {}).forEach(([k, v]) => {
          if (!k.startsWith("__") && isNumeric(v)) {
            varKeys.add(k);
          }
        });
      }
    });

    const datasets = [];
    let colorIndex = 0;

    function makeDataset(label, source) {
      const color = palette[colorIndex % palette.length];
      colorIndex++;

      const data = list.map((item, index) => {
        const raw =
          source === "clock"
            ? item.step.panelData.clocks?.[label]
            : item.step.panelData.variables?.[label];

        const y = Number(raw);

        return {
          x: xOf(item, index),
          y: Number.isFinite(y) ? y : null
        };
      });

      return {
        label,
        data,
        borderColor: color,
        backgroundColor: color,
        borderWidth: 1.2,
        pointRadius: 1,
        pointHoverRadius: 3,
        tension: 0.15,
        spanGaps: true
      };
    }

    Array.from(clockKeys).forEach((key) => {
      datasets.push(makeDataset(key, "clock"));
    });

    Array.from(varKeys).forEach((key) => {
      datasets.push(makeDataset(key, "variable"));
    });

    const events = [];

    list.forEach((item, index) => {
      const delay = delayFromLabel(item.step.label);

      if (!delay && item.step.label !== "Start") {
        events.push({
          x: xOf(item, index),
          label: item.step.label
        });
      }
    });

    state.chart.data.datasets = datasets;
    state.chart.$traceEvents = events;

    state.chart.options.scales.x.title.text = useTime ? "tempo" : "passo";

    state.chart.update();
  }

  function renderAll() {
    updateControls();
    renderList();
    updateChart();
  }

  function initControls() {
    const liveBtn = document.getElementById("traceLiveBtn");
    const firstBtn = document.getElementById("traceFirstBtn");
    const prevBtn = document.getElementById("tracePrevBtn");
    const playBtn = document.getElementById("tracePlayBtn");
    const nextBtn = document.getElementById("traceNextBtn");
    const lastBtn = document.getElementById("traceLastBtn");
    const range = document.getElementById("traceRange");
    const speed = document.getElementById("traceSpeed");
    const filter = document.getElementById("traceFilter");
    const showClocks = document.getElementById("traceShowClocks");
    const showVars = document.getElementById("traceShowVars");
    const listEl = document.getElementById("traceList");

    if (!playBtn) return;

    liveBtn.onclick = goLive;
    firstBtn.onclick = first;
    prevBtn.onclick = prev;
    playBtn.onclick = togglePlay;
    nextBtn.onclick = next;
    lastBtn.onclick = last;

    range.oninput = function () {
      showByViewIndex(parseInt(this.value, 10));
    };

    speed.onchange = function () {
      state.speed = parseInt(this.value, 10);
    };

    filter.oninput = function () {
      state.filter = this.value;
      const visible = visibleSteps();
      state.viewIndex = visible.length ? visible.length - 1 : -1;
      renderAll();
    };

    showClocks.onchange = function () {
      state.showClocks = this.checked;
      updateChart();
    };

    showVars.onchange = function () {
      state.showVars = this.checked;
      updateChart();
    };

    listEl.addEventListener("click", function (event) {
      const el = event.target.closest("[data-trace-index]");
      if (!el) return;

      const index = parseInt(el.dataset.traceIndex, 10);
      showByViewIndex(index);
    });

    renderAll();
  }

  function wrapUpdateAllViews() {
    if (typeof window.updateAllViews !== "function") {
      setTimeout(wrapUpdateAllViews, 80);
      return;
    }

    originalUpdateAllViews = window.updateAllViews;

    window.updateAllViews = function (json) {
      originalUpdateAllViews(json);

      if (state.replaying) return;

      if (state.skipNext) {
        state.skipNext = false;
        return;
      }

      record(json);
    };
  }

  window.TraceRecorder = {
    setLabel(label) {
      state.pendingLabel = label;
    },

    skipNext() {
      state.skipNext = true;
    },

    reset(label = "Start") {
      stopPlay();
      setReplayMode(false);

      state.steps = [];
      state.currentIndex = -1;
      state.viewIndex = -1;
      state.pendingLabel = label;
      state.skipNext = false;

      renderAll();
    },

    isReplaying() {
      return state.replaying;
    },

    resize() {
      if (state.chart) {
        state.chart.resize();
      }
    },

    goLive
  };

  function init() {
    initControls();
    wrapUpdateAllViews();
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init);
  } else {
    init();
  }
})();