
var currentEdgeStyle = 'straight';
let autoPlayTimer = null;


function renderGlobalPanel(data, targetId) {
    var containerId = targetId || 'sidePanel';
    var panelDiv    = document.getElementById(containerId);
    if (!panelDiv) return;

    panelDiv.innerHTML = '';
    var panelData = data.panelData;
    lastModelData = data;

    var t      = i18n[currentLang];
    var suffix = containerId === 'sidePanel' ? '' : ('-' + containerId);

    // BOTOES DE TOPO (Undo e Auto-Play)
    var topBtns = document.createElement('div');
    topBtns.style.display = 'flex';
    topBtns.style.gap = '5px';
    topBtns.style.marginBottom = '10px';

    var undoBtn = document.createElement('button');
    undoBtn.className = 'btn btn-warning btn-sm';
    undoBtn.style.flex = '1';
    undoBtn.innerHTML = '<span class="glyphicon glyphicon-step-backward"></span> Undo';
    undoBtn.disabled  = !panelData.canUndo;
    undoBtn.onclick   = function () {
        if (autoPlayTimer) toggleAutoPlay(); // Para auto-play se estiver a correr
        var json = RTA.undo();
        if (jsTextHistory.length > 1) jsTextHistory.pop();
        updateAllViews(json);
    };

    var autoPlayBtn = document.createElement('button');
    autoPlayBtn.className = autoPlayTimer ? 'btn btn-danger btn-sm' : 'btn btn-info btn-sm';
    autoPlayBtn.style.flex = '1';
    autoPlayBtn.innerHTML = autoPlayTimer ? '<span class="glyphicon glyphicon-stop"></span> Parar' : '<span class="glyphicon glyphicon-random"></span> Auto-Play';
    autoPlayBtn.onclick = toggleAutoPlay;

    topBtns.appendChild(undoBtn);
    topBtns.appendChild(autoPlayBtn);
    panelDiv.appendChild(topBtns);

    // VARIÁVEIS E RELÓGIOS
    if (
        (panelData.clocks   && Object.keys(panelData.clocks).length   > 0) ||
        (panelData.variables && Object.keys(panelData.variables).length > 0)
    ) {
        var varHeader       = document.createElement('div');
        varHeader.className = 'sec-label';
        varHeader.innerText = t.stat_header || 'Estado Atual:';
        panelDiv.appendChild(varHeader);

        var infoList       = document.createElement('ul');
        infoList.className = "list-unstyled";
        infoList.style.cssText = "font-size:12px; background:#fff; padding:10px; border:1px solid var(--border); border-radius:2px;";

        for (let [k, v] of Object.entries(panelData.clocks || {})) {
            let li       = document.createElement('li');
            li.innerHTML = `<span class="text-info">🕒 ${k}</span>: <b>${v.toFixed(2)}s</b>`;
            infoList.appendChild(li);
        }
        for (let [k, v] of Object.entries(panelData.variables || {})) {
            if (k.startsWith("__")) continue;
            let li       = document.createElement('li');
            li.innerHTML = `<span class="text-success"># ${k}</span>: <b>${v}</b>`;
            infoList.appendChild(li);
        }
        panelDiv.appendChild(infoList);
    }

    // NOVO: PENDENTES / AGENDADOS
    if (panelData.pending && panelData.pending.length > 0) {
        var pendHeader = document.createElement('div');
        pendHeader.className = 'sec-label';
        pendHeader.style.marginTop = '10px';
        pendHeader.innerText = '⏳ Hiper-Arestas Agendadas:';
        panelDiv.appendChild(pendHeader);

        var pendList = document.createElement('ul');
        pendList.className = "list-unstyled";
        pendList.style.cssText = "font-size:11px; background:#fff8e1; padding:8px; border:1px solid #ffe082; border-radius:2px;";
        panelData.pending.forEach(p => {
            let li = document.createElement('li');
            let color = p.op === 'on' ? '#2563EB' : '#DC2626';
            let opTxt = p.op === 'on' ? 'Ligar' : 'Desligar';
            li.innerHTML = `<b style="color:${color}">${p.label} (${opTxt})</b> aos <b>${p.target.toFixed(2)}s</b> do relógio [${p.clock}]`;
            pendList.appendChild(li);
        });
        panelDiv.appendChild(pendList);
    }

    // TRANSIÇÕES DISPONÍVEIS
    panelDiv.appendChild(document.createElement('hr'));
    var transHeader       = document.createElement('div');
    transHeader.className = 'sec-label';
    transHeader.innerText = t.enabled_trans || 'Transições Disponíveis:';
    panelDiv.appendChild(transHeader);

    var isDeadlock = panelData.enabled.length === 0 ||
        (panelData.enabled.length === 1 && panelData.enabled[0].label === "deadlock");

    if (isDeadlock) {
        var dead       = document.createElement('div');
        dead.className = "alert alert-danger text-center";
        dead.style.cssText = "padding:5px; font-size:12px; font-weight:bold;";
        dead.innerText = (t.deadlock || "DEADLOCK") + " (LOOP)";
        panelDiv.appendChild(dead);
        if(autoPlayTimer) toggleAutoPlay(); // Para o auto-play se bater em deadlock
    }

    if (panelData.enabled.length > 0) {
        panelData.enabled.forEach(function (edge) {
            if (edge.isDelay) _renderDelayControl(panelDiv, edge, suffix, containerId);
            else _renderTransitionButton(panelDiv, edge);
        });
    }

    if (containerId === 'sidePanel') {
        _renderLayoutPanel(panelDiv, t, suffix);
    }
}

// O MOTOR DO AUTO-PLAY
function toggleAutoPlay() {
    if (autoPlayTimer) {
        clearInterval(autoPlayTimer);
        autoPlayTimer = null;
        updateAllViews(JSON.stringify(lastModelData)); // Atualiza UI para repor botões
    } else {
        autoPlayTimer = setInterval(() => {
            if(!lastModelData || !lastModelData.panelData || !lastModelData.panelData.enabled) return;
            let enabled = lastModelData.panelData.enabled;
            if(enabled.length === 0 || (enabled.length === 1 && enabled[0].label === 'deadlock')) {
                toggleAutoPlay(); return;
            }
            
            // Escolhe uma ação aleatória da lista
            let valid = enabled.filter(e => e.label !== 'deadlock');
            if(valid.length === 0) return;
            let choice = valid[Math.floor(Math.random() * valid.length)];
            
            if (choice.isDelay) {
                // Se escolheu Delay, avança o tempo por um valor aleatório (ex: até 1.5s)
                let rDelay = parseFloat((0.1 + Math.random() * 1.5).toFixed(2));
                updateAllViews(RTA.advanceTime(rDelay));
            } else {
                // Se escolheu transição, clica nela
                var json = RTA.takeStep(JSON.stringify(choice));
                var newStateText = RTA.getCurrentStateText();
                jsTextHistory.push({ label: choice.label + " ->", text: newStateText });
                updateAllViews(json);
            }
        }, 1200); // 1.2 segundos por passo
        updateAllViews(JSON.stringify(lastModelData)); 
    }
}

function _renderDelayControl(panelDiv, edge, suffix, containerId) {
    var btnGroup       = document.createElement('div');
    btnGroup.style.display     = 'flex';
    btnGroup.style.gap         = '5px';
    btnGroup.style.width       = '100%';
    btnGroup.style.marginBottom = '5px';

    var input    = document.createElement('input');
    input.type   = 'number';
    input.className = 'form-control input-sm';
    input.value  = storedDelayValue;
    input.step   = '0.001';
    input.min    = '0.000001';
    input.id     = 'delayInputVal' + suffix;
    input.style.flex     = '1';
    input.style.minWidth = '0';
    input.onchange = function () {
        var val = parseFloat(this.value);
        if (!isNaN(val)) {
            storedDelayValue = val;
            var otherId = containerId === 'sidePanel' ? 'delayInputVal-sidePanel-bottom' : 'delayInputVal';
            var otherInp = document.getElementById(otherId);
            if (otherInp) otherInp.value = val;
        }
    };

    var btn        = document.createElement('button');
    btn.className  = 'btn btn-default btn-sm';
    btn.innerHTML  = '⏱ Delay';
    btn.style.whiteSpace = 'nowrap';
    btn.style.flexShrink = '0';
    btn.onclick    = function () {
        storedDelayValue = parseFloat(input.value);
        updateAllViews(RTA.advanceTime(storedDelayValue));
    };

    btnGroup.appendChild(input);
    btnGroup.appendChild(btn);
    panelDiv.appendChild(btnGroup);
}

function _renderTransitionButton(panelDiv, edge) {
    var btn         = document.createElement('button');
    var displayName = (edge.tId === edge.label) ? edge.label : edge.tId + " (" + edge.label + ")";
    btn.className   = 'sim-btn';

    if (edge.label === 'deadlock') {
        btn.style.backgroundColor = '#FECACA';
        btn.style.borderColor     = '#EF4444';
        btn.style.color           = '#7F1D1D';
    }

    var icon       = document.createElement('span');
    icon.className = 'glyphicon glyphicon-play-circle';
    icon.style.color = 'var(--gray-500)';
    if (edge.label === 'deadlock') icon.style.color = '#B91C1C';
    btn.appendChild(icon);

    var txt       = document.createElement('span');
    txt.innerText = displayName;
    btn.appendChild(txt);

    if (edge.p !== undefined && !window.isPossibilisticView) {
        var pSpan       = document.createElement('span');
        pSpan.className = 'sim-prob';
        pSpan.innerText = 'P=' + edge.p.toFixed(3);
        btn.appendChild(pSpan);
    }

    btn.onclick = function () {
        stopAutoDelay();
        var json         = RTA.takeStep(JSON.stringify(edge));
        var newStateText = RTA.getCurrentStateText();
        jsTextHistory.push({ label: edge.label + " ->", text: newStateText });
        updateAllViews(json);
    };
    panelDiv.appendChild(btn);
}

function _renderLayoutPanel(panelDiv, t, suffix) {
    var panelGroup       = document.createElement('div');
    panelGroup.className = 'panel-group';
    panelGroup.id        = 'layoutSettingsGroup' + suffix;
    panelGroup.style.marginTop = '15px';

    var layoutPanel       = document.createElement('div');
    layoutPanel.className = 'panel panel-default';

    var panelHeading       = document.createElement('div');
    panelHeading.className = 'panel-heading';
    panelHeading.style.padding = '0';
    panelHeading.innerHTML = `
        <h4 class="panel-title" style="font-size:11px;">
            <a data-toggle="collapse" href="#collapseLayout${suffix}" style="text-decoration:none; display:block;">
                <span class="glyphicon glyphicon-cog"></span> ${t.layout_conf_title} <span class="caret"></span>
            </a>
        </h4>`;

    var collapseBody       = document.createElement('div');
    collapseBody.id        = 'collapseLayout' + suffix;
    collapseBody.className = 'panel-collapse collapse';

    var panelBody       = document.createElement('div');
    panelBody.className = 'panel-body';
    renderLayoutControls(panelBody);

    collapseBody.appendChild(panelBody);
    layoutPanel.appendChild(panelHeading);
    layoutPanel.appendChild(collapseBody);
    panelGroup.appendChild(layoutPanel);
    panelDiv.appendChild(panelGroup);
}


function renderLayoutControls(container) {
    var t = i18n[currentLang];

    var layoutGroup   = document.createElement('div');
    layoutGroup.className = 'form-group';
    var layoutLabel   = document.createElement('label');
    layoutLabel.innerText  = t.layout_label;
    layoutLabel.style.fontSize = '12px';

    var layoutSelect  = document.createElement('select');
    layoutSelect.className = 'form-control input-sm';
    layoutSelect.innerHTML = `
        <option value="preset">${t.opt_preset}</option>
        <option value="dagre" selected>${t.opt_dagre}</option>
        <option value="cose">${t.opt_cose}</option>
        <option value="circle">${t.opt_circle}</option>
        <option value="grid">${t.opt_grid}</option>
        <option value="random">${t.opt_random}</option>
    `;
    layoutSelect.onchange = function (e) {
        if (!currentCytoscapeInstance) return;
        var name    = e.target.value;
        var options = { name: name, fit: true, padding: 50, animate: true };
        if (name === 'dagre') options.rankDir = 'LR';
        if (name === 'cose')  { options.componentSpacing = 40; options.nodeRepulsion = 8000; }
        currentCytoscapeInstance.layout(options).run();
    };
    layoutGroup.appendChild(layoutLabel);
    layoutGroup.appendChild(layoutSelect);
    container.appendChild(layoutGroup);

    var styleGroup   = document.createElement('div');
    styleGroup.className = 'form-group';
    var styleLabel   = document.createElement('label');
    styleLabel.innerText  = t.edge_style_label;
    styleLabel.style.fontSize = '12px';

    var styleSelect  = document.createElement('select');
    styleSelect.className = 'form-control input-sm';
    styleSelect.innerHTML = `
        <option value="straight">${t.opt_straight}</option>
        <option value="taxi">${t.opt_taxi}</option>
        <option value="bezier">${t.opt_bezier}</option>
    `;
    styleSelect.value    = currentEdgeStyle || 'straight';
    styleSelect.onchange = function (e) { changeEdgeStyle(e.target.value); };
    styleGroup.appendChild(styleLabel);
    styleGroup.appendChild(styleSelect);
    container.appendChild(styleGroup);

    container.appendChild(document.createElement('hr'));

    var btnGroup       = document.createElement('div');
    btnGroup.className = 'btn-group-vertical btn-block';

    var saveBtn       = document.createElement('button');
    saveBtn.className = 'btn btn-default btn-sm';
    saveBtn.innerText = t.btn_save_layout;
    saveBtn.onclick   = exportAllLayoutsToFile;

    var loadBtn       = document.createElement('button');
    loadBtn.className = 'btn btn-default btn-sm';
    loadBtn.innerText = t.btn_load_layout;
    loadBtn.onclick   = function () { document.getElementById('hiddenFileInput').click(); };

    if (!document.getElementById('hiddenFileInput')) {
        var fileInput    = document.createElement('input');
        fileInput.type   = 'file';
        fileInput.id     = 'hiddenFileInput';
        fileInput.style.display = 'none';
        fileInput.accept = '.json,application/json';
        fileInput.onchange = function (e) {
            var file = e.target.files[0];
            if (!file) return;
            var reader = new FileReader();
            reader.onload = function (evt) {
                importAllLayoutsFromFile(currentCytoscapeInstance, null, evt.target.result);
            };
            reader.readAsText(file);
            e.target.value = '';
        };
        document.body.appendChild(fileInput);
    }

    btnGroup.appendChild(saveBtn);
    btnGroup.appendChild(loadBtn);
    container.appendChild(btnGroup);
}