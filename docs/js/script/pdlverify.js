// Rastreador global para saber qual caixa de input tem o foco (útil se tivermos múltiplos inputs de fórmulas)
let lastFocusedInput = null;

document.addEventListener('focusin', function(e) {
    if (e.target.tagName === 'INPUT' && e.target.type === 'text') {
        lastFocusedInput = e.target;
    }
});

// -------------------------------------------------------------
// RENDERIZAÇÃO DOS BOTÕES DE AJUDA (ESTADOS, VARIÁVEIS, REGRAS)
// -------------------------------------------------------------
function renderPdlHelpers(data) {
    if (!data || !data.graphElements) return;

    var uniqueStates = new Set();
    var actionMap = new Map();

    // Extrai estados e ações do grafo atual
    data.graphElements.forEach(function (el) {
        var cls = el.classes || "";
        var d = el.data;
        if (cls.indexOf('state-node') !== -1 && d && d.label) {
            uniqueStates.add(d.label);
        }
        if (cls.indexOf('event-node') !== -1 && d && (d.action_name || d.label)) {
            var display = d.hover_label || d.label.replace(/\\n/g, ' ').replace('\n', ' ');
            var technicalName = d.action_name || d.label;
            actionMap.set(display, technicalName);
        }
    });

    var allVars = {};
    if (data.panelData) {
        if (data.panelData.variables) Object.assign(allVars, data.panelData.variables);
        if (data.panelData.clocks) Object.assign(allVars, data.panelData.clocks);
    }

    // Popula todas as abas lógicas simultaneamente (PDL e LTL)
    ['pdl', 'ltl'].forEach(prefix => {
        populateHelperForTab(prefix, uniqueStates, actionMap, allVars);
    });
}

function populateHelperForTab(prefix, uniqueStates, actionMap, allVars) {
    var statesDiv = document.getElementById(prefix + '-states-list');
    var actionsDiv = document.getElementById(prefix + '-actions-list');
    var varsDiv = document.getElementById(prefix + '-vars-list');

    if (statesDiv) statesDiv.innerHTML = '';
    if (actionsDiv) actionsDiv.innerHTML = '';
    if (varsDiv) varsDiv.innerHTML = '';

    // 1. Estados
    if (uniqueStates.size === 0) {
        if (statesDiv) statesDiv.innerHTML = '<span class="text-muted" style="font-size:10px;">Sem estados.</span>';
    } else {
        Array.from(uniqueStates).sort().forEach(function (st) {
            var btn = document.createElement('button');
            btn.className = 'btn btn-xs btn-primary';
            btn.style.margin = '2px';
            btn.innerText = st;
            btn.onclick = function () {
                var stateInput = document.getElementById(prefix + 'State');
                if (stateInput) {
                    stateInput.value = st;
                    stateInput.style.backgroundColor = "#d1e7dd";
                    setTimeout(() => stateInput.style.backgroundColor = "#fff", 300);
                } else {
                    insertSymbol(prefix, st);
                }
            };
            if (statesDiv) statesDiv.appendChild(btn);
        });
    }

    // 2. Variáveis
    var hasVars = Object.keys(allVars).length > 0;
    if (hasVars) {
        for (var vName in allVars) {
            var btn = document.createElement('button');
            btn.className = 'btn btn-xs btn-success';
            btn.style.margin = '2px';
            btn.innerText = vName;
            btn.onclick = (function (name) {
                return function () { insertSymbol(prefix, name); };
            })(vName);
            if (varsDiv) varsDiv.appendChild(btn);
        }
    } else {
        if (varsDiv) varsDiv.innerHTML = '<span class="text-muted" style="font-size:10px;">Sem variáveis.</span>';
    }

    // 3. Ações / Hyperedges
    var hasActions = false;
    if (actionMap.size > 0) {
        Array.from(actionMap.keys()).sort().forEach(function (display) {
            var techName = actionMap.get(display);
            var btn = document.createElement('button');
            btn.className = 'btn btn-xs btn-warning';
            btn.style.margin = '2px';
            btn.innerText = display;
            btn.onclick = function () { insertSymbol(prefix, techName); };
            if (actionsDiv) actionsDiv.appendChild(btn);
        });
        hasActions = true;
    }

    // 4. Injetar Estados também nos atalhos de Ações (para PDL)
    if (uniqueStates.size > 0 && actionsDiv) {
        if (hasActions) {
            var divider = document.createElement('span');
            divider.style.display = "inline-block";
            divider.style.width = "2px";
            divider.style.height = "15px";
            divider.style.background = "#ddd";
            divider.style.margin = "0 8px";
            divider.style.verticalAlign = "middle";
            actionsDiv.appendChild(divider);
        }
        Array.from(uniqueStates).sort().forEach(function (st) {
            var btn = document.createElement('button');
            btn.className = 'btn btn-xs btn-primary';
            btn.style.margin = '2px';
            btn.innerText = st;
            btn.onclick = function () { insertSymbol(prefix, st); };
            actionsDiv.appendChild(btn);
        });
        hasActions = true;
    }

    if (!hasActions && actionsDiv) {
        actionsDiv.innerHTML = '<span class="text-muted" style="font-size:10px;">Sem elementos.</span>';
    }
}

// Inserção direcionada para as caixas PDL ou LTL
function insertSymbol(prefix, text, suffix) {
    var input = document.getElementById(prefix + 'Formula');
    if (!input) return;
    var valToInsert = text + (suffix || "");

    if (input.selectionStart !== undefined) {
        var startPos = input.selectionStart;
        var endPos = input.selectionEnd;
        input.value = input.value.substring(0, startPos) + valToInsert + input.value.substring(endPos, input.value.length);
        
        var newPos = startPos + (suffix ? text.length : valToInsert.length);
        input.selectionStart = newPos;
        input.selectionEnd = newPos;
    } else {
        input.value += valToInsert;
    }
    input.focus();
}

function insertSymbolFocused(text, suffix) {
    if (!lastFocusedInput) return;
    var input = lastFocusedInput;
    var valToInsert = text + (suffix || "");
    
    if (input.selectionStart !== undefined) {
        var startPos = input.selectionStart;
        var endPos = input.selectionEnd;
        input.value = input.value.substring(0, startPos) + valToInsert + input.value.substring(endPos, input.value.length);
        
        var newPos = startPos + (suffix ? text.length : valToInsert.length);
        input.selectionStart = newPos;
        input.selectionEnd = newPos;
    } else {
        input.value += valToInsert;
    }
    input.focus();
}


// -------------------------------------------------------------
// AVALIAÇÃO PDL CLÁSSICA (Lógica Standard Modal)
// -------------------------------------------------------------
function runPdl() {
    var prefix = 'pdl';
    var stateInput = document.getElementById(prefix + "State");
    var s = stateInput ? stateInput.value : "";
    
    // Se não há estado, inferir do grafo atual (estado verde)
    if (!s && typeof currentCytoscapeInstance !== 'undefined' && currentCytoscapeInstance) {
        var currentNodes = currentCytoscapeInstance.nodes('.current-state');
        if (currentNodes.length > 0) s = currentNodes[0].data('label');
    }

    var visualFormula = document.getElementById(prefix + "Formula").value;
    if (!visualFormula || visualFormula.trim() === "") {
        alert("Escreva uma fórmula PDL para verificar.");
        return;
    }
    
    var protectedCode = visualFormula
        .replace(/⟨/g, '___DIAM_OPEN___')
        .replace(/⟩/g, '___DIAM_CLOSE___')
        .replace(/¬/g, '!')
        .replace(/∧/g, '&&')
        .replace(/∨/g, '||')
        .replace(/→/g, '->')
        .replace(/↔/g, '<->')
        .replace(/⊤/g, 'true')
        .replace(/⊥/g, 'false')
        .replace(/□/g, 'G')
        .replace(/♢/g, 'F');

    var regexComparison = /\b([a-zA-Z_][\w\.]*)\s*(=|==|≠|!=|≤|<=|≥|>=|<|>)\s*(-?\d+(?:\.\d+)?|[a-zA-Z_][\w\.]*)\b/g;

    var codeWithVars = protectedCode.replace(regexComparison, function (match, v1, op, v2) {
        if (['true', 'false', '___DIAM_OPEN___', '___DIAM_CLOSE___'].includes(v1)) return match;
        var backendOp = op;
        if (op === '=' || op === '==') backendOp = '==';
        if (op === '≠' || op === '!=') backendOp = '!=';
        if (op === '≤' || op === '<=') backendOp = '<=';
        if (op === '≥' || op === '>=') backendOp = '>=';
        return '[' + v1 + ' ' + backendOp + ' ' + v2 + ']';
    });

    var finalCode = codeWithVars
        .replace(/___DIAM_OPEN___/g, '<')
        .replace(/___DIAM_CLOSE___/g, '>')
        .replace(/\[\[/g, '[') 
        .replace(/\]\]/g, ']');

    var resStr = RTA.runPdl(s, finalCode, 0);
    var resDiv = document.getElementById(prefix + "Result");

    try {
        var resObj = JSON.parse(resStr);
        if (resObj.error) {
            resDiv.style.color = "darkorange";
            resDiv.innerText = resObj.error;
            return;
        }

        var text = resObj.text;
        var lines = text.split('\n');
        var extraInfo = lines.slice(1).join('<br/>');
        var isTrue = resObj.result;
        
        resDiv.style.color = isTrue ? "green" : "red";
        var icon = isTrue ? '<span class="glyphicon glyphicon-ok"></span> Verdadeiro' : '<span class="glyphicon glyphicon-remove"></span> Falso';
        
        resDiv.innerHTML = icon + '<div style="font-size:10px; font-weight:normal; color:#555; margin-top:6px; background:#f9f9f9; padding:6px; border:1px solid #eee; border-radius:3px;">' + extraInfo + '</div>';

    } catch (e) {
        resDiv.style.color = "darkorange";
        resDiv.innerText = "Internal error parsing result: " + resStr;
    }
}


// -------------------------------------------------------------
// LÓGICA DO SIMULADOR CONTÍNUO LTL (Estilo UPPAAL SMC com Delays)
// -------------------------------------------------------------
let ltlSimTimer = null;
let ltlTotalTraces = 0;
let isLtlSimulating = false;

function toggleLTLSimulation() {
    if (isLtlSimulating) stopLTLSimulation();
    else startLTLSimulation();
}

function startLTLSimulation() {
    var visualFormula = document.getElementById("ltlFormula").value;
    if (!visualFormula || visualFormula.trim() === "") {
        alert("Escreva uma fórmula LTL para verificar.");
        return;
    }

    var finalCode = visualFormula
        .replace(/¬/g, '! ').replace(/∧/g, ' && ').replace(/∨/g, ' || ')
        .replace(/→/g, ' -> ').replace(/↔/g, ' <-> ').replace(/⊤/g, 'true')
        .replace(/⊥/g, 'false').replace(/□/g, 'G ').replace(/♢/g, 'F ');

    var traceLength = parseInt(document.getElementById("ltlTraceLength").value) || 30;
    var batchSize = parseInt(document.getElementById("ltlBatchSize").value) || 10;

    isLtlSimulating = true;
    ltlTotalTraces = 0;
    
    var btn = document.getElementById("btnLtlSim");
    btn.innerHTML = '<span class="glyphicon glyphicon-stop"></span> Parar Simulador';
    btn.style.backgroundColor = '#DC2626';
    btn.style.color = '#fff';

    var consoleDiv = document.getElementById("ltlLiveConsole");
    consoleDiv.innerHTML = "🔧 Iniciando Simulador LTL Contínuo...\n";
    document.getElementById("ltlSimStats").innerText = "Avaliados: 0";
    document.getElementById("ltlResult").innerHTML = "";

    function simLoop() {
        if (!isLtlSimulating) return;

        var resStr = RTA.verifyLTLBatch(finalCode, traceLength, batchSize);
        try {
            var resObj = JSON.parse(resStr);
            if (resObj.error) {
                consoleDiv.innerHTML += `<span style="color:#FF6B6B">Erro: ${resObj.error}</span>\n`;
                stopLTLSimulation();
                return;
            }

            ltlTotalTraces += resObj.passedCount;
            document.getElementById("ltlSimStats").innerText = "Avaliados: " + ltlTotalTraces;

            if (resObj.sample && resObj.passedCount > 0) {
                var line = `[Pass ${ltlTotalTraces}] ➔ ${resObj.sample}\n`;
                consoleDiv.innerHTML += line;
                consoleDiv.scrollTop = consoleDiv.scrollHeight;

                if (resObj.samplePath && resObj.samplePath.length > 0) {
                    if (typeof playCounterexample === 'function') {
                        playCounterexample(true, resObj.samplePath, 40); 
                    }
                }
            }

            if (resObj.visitLog && resObj.visitLog.length > 0) {
                consoleDiv.innerHTML += `\n<span style="color:#94a3b8; font-size: 8px;">[DEBUG] Rota do Explorador DFS (${resObj.visitLog.length} saltos): ${resObj.visitLog.join(" ➔ ")}</span>\n`;
            }


            if (!resObj.success) {
                ltlTotalTraces += 1;
                document.getElementById("ltlSimStats").innerText = "Avaliados: " + ltlTotalTraces + " (FALHOU!)";
                document.getElementById("ltlSimStats").style.color = "#DC2626";

                consoleDiv.innerHTML += `\n<span style="color:#FF6B6B; font-weight:bold;">❌ CONTRA-EXEMPLO ENCONTRADO!</span>\n`;
                consoleDiv.innerHTML += `<span style="color:#FF6B6B">Traço: Start ➔ ${resObj.counterExample}</span>\n`;
                consoleDiv.scrollTop = consoleDiv.scrollHeight;
                
                document.getElementById("ltlResult").innerHTML = '<span style="color:red"><span class="glyphicon glyphicon-remove"></span> A fórmula falhou num dos percursos. A animar o erro...</span>';

                window.lastCounterexample = resObj.path;
                if (typeof playCounterexample === 'function') {
                    playCounterexample(false);
                }
                stopLTLSimulation();
                return;
            }

            if (isLtlSimulating) {
                ltlSimTimer = setTimeout(simLoop, 20); 
            }
        } catch (e) {
            consoleDiv.innerHTML += "<span style='color:orange'>Erro interno de parsing.</span>\n";
            stopLTLSimulation();
        }
    }

    simLoop(); // Arranca a máquina!
}

function stopLTLSimulation() {
    isLtlSimulating = false;
    clearTimeout(ltlSimTimer);
    var btn = document.getElementById("btnLtlSim");
    if (btn) {
        btn.innerHTML = '<span class="glyphicon glyphicon-play"></span> Sim. Contínua';
        btn.style.backgroundColor = '';
        btn.style.color = '';
    }
}

function runLtlMultiple() {
    var backupSize = document.getElementById("ltlBatchSize").value;
    document.getElementById("ltlBatchSize").value = 1;
    startLTLSimulation();
    
    setTimeout(() => {
        stopLTLSimulation();
        document.getElementById("ltlBatchSize").value = backupSize;
    }, 100);
}



function runLTLExhaustive() {
    var visualFormula = document.getElementById("ltlFormula").value;
    if (!visualFormula || visualFormula.trim() === "") {
        alert("Escreva uma fórmula LTL para verificar.");
        return;
    }

    var finalCode = visualFormula
        .replace(/¬/g, '!').replace(/∧/g, '&&').replace(/∨/g, '||')
        .replace(/→/g, '->').replace(/↔/g, '<->').replace(/⊤/g, 'true')
        .replace(/⊥/g, 'false').replace(/□/g, 'G ').replace(/♢/g, 'F ');


    var maxDepth = parseInt(document.getElementById("ltlTraceLength").value) || 30;
    var consoleDiv = document.getElementById("ltlLiveConsole");
    
    consoleDiv.innerHTML = "🧠 Iniciando Motor Simbólico (DBM)...\nExplorando todos os Universos Temporais...\n";
    document.getElementById("ltlSimStats").innerText = "A Iniciar Prova...";
    document.getElementById("ltlResult").innerHTML = "";

    // -- ANIMAÇÃO LIGADA --
    var cyWrapper = document.getElementById("cyWrapper");
    var scanner = document.getElementById("cy-scanner-overlay");
    if (cyWrapper) cyWrapper.classList.add("cy-processing");
    if (scanner) scanner.style.display = "block";
    if (typeof showCanvasTab === 'function') showCanvasTab('cyTab'); // Muda para o Grafo para ver o show!

    // Atraso de 100ms para a UI atualizar e mostrar o scanner antes da conta pesada bloquear o browser
    setTimeout(() => {
        var resStr = RTA.runLTLExhaustive(finalCode, 50000, maxDepth);
        
        // -- ANIMAÇÃO DESLIGADA --
        if (cyWrapper) cyWrapper.classList.remove("cy-processing");
        if (scanner) scanner.style.display = "none";

        try {
            var resObj = JSON.parse(resStr);
            if (resObj.error) {
                consoleDiv.innerHTML += `<span style="color:#FF6B6B">Erro: ${resObj.error}</span>\n`;
                return;
            }

            document.getElementById("ltlSimStats").innerText = "Zonas Exploradas: " + resObj.explored;

            if (resObj.success) {
                let msg = resObj.limitReached ? 
                    `\n✅ VERIFICADO (Limitado a ${resObj.explored} zonas)\nNenhuma violação encontrada neste limite de profundidade.` :
                    `\n🏆 PROVA MATEMÁTICA A 100%\nA fórmula é válida para *qualquer* valor de tempo possível!`;
                
                consoleDiv.innerHTML += `<span style="color:#10B981; font-weight:bold;">${msg}</span>\n`;
                document.getElementById("ltlResult").innerHTML = '<span style="color:green"><span class="glyphicon glyphicon-ok"></span> Prova Exaustiva Concluída com Sucesso!</span>';
                
                // Piscar a verde!
                if (cyWrapper) {
                    cyWrapper.classList.add("cy-success-flash");
                    setTimeout(() => cyWrapper.classList.remove("cy-success-flash"), 1000);
                }
            } else {
                consoleDiv.innerHTML += `\n<span style="color:#FF6B6B; font-weight:bold;">❌ CONTRA-EXEMPLO ENCONTRADO!</span>\n`;
                consoleDiv.innerHTML += `<span style="color:#FF6B6B">Traço (DBM): Start ➔ ${resObj.counterExample}</span>\n`;
                consoleDiv.scrollTop = consoleDiv.scrollHeight;
                
                document.getElementById("ltlResult").innerHTML = '<span style="color:red"><span class="glyphicon glyphicon-remove"></span> A fórmula falhou. A animar o contra-exemplo...</span>';

                window.lastCounterexample = resObj.path;
                if (typeof playCounterexample === 'function') {
                    playCounterexample(false, resObj.path, 800); // Anima o erro no grafo normal
                }
            }
        } catch (e) {
            consoleDiv.innerHTML += "<span style='color:orange'>Erro interno ao processar a prova.</span>\n";
        }
    }, 100);
}


let dbmCyInstance = null;

function showDBMGraphModal() {
    var jsonStr = RTA.getSymbolicStepsJSON();
    var elements = JSON.parse(jsonStr);
    
    if (elements.length === 0) {
        alert("Nenhum modelo carregado ou grafo vazio.");
        return;
    }
    
    // Abre a modal
    $('#dbmGraphModal').modal('show');
    
    // Pequeno atraso para a Modal ganhar as dimensões certas antes de injetar o Cytoscape
    setTimeout(() => {
        if (dbmCyInstance) dbmCyInstance.destroy();
        
        dbmCyInstance = cytoscape({
            container: document.getElementById('dbmCytoscapeContainer'),
            elements: elements,
            style: [
                {
                    selector: 'node',
                    style: {
                        'shape': 'round-rectangle',
                        'background-color': '#ffffff',
                        'border-width': 2,
                        'border-color': '#4f46e5', // Azul índigo
                        'label': 'data(label)',
                        'text-wrap': 'wrap',
                        'text-valign': 'center',
                        'text-halign': 'center',
                        'font-size': '12px',
                        'font-family': 'monospace',
                        'color': '#1e293b',
                        'width': 'label',
                        'height': 'label',
                        'padding': '12px'
                    }
                },
                {
                    selector: 'node[isStart = true]',
                    style: {
                        'border-width': 4,
                        'border-color': '#10b981', // Verde
                        'background-color': '#ecfdf5'
                    }
                },
                {
                    selector: 'edge',
                    style: {
                        'width': 2,
                        'line-color': '#94a3b8',
                        'target-arrow-color': '#94a3b8',
                        'target-arrow-shape': 'triangle',
                        'curve-style': 'bezier',
                        'label': 'data(label)',
                        'font-size': '11px',
                        'font-weight': 'bold',
                        'color': '#0f172a',
                        'text-background-color': '#e2e8f0',
                        'text-background-opacity': 1,
                        'text-background-padding': '3px'
                    }
                }
            ],
            layout: {
                name: 'dagre',      // Organiza hierarquicamente como uma árvore de estados
                rankDir: 'TB',      // De cima (Top) para baixo (Bottom)
                spacingFactor: 1.2
            },
            wheelSensitivity: 0.2
        });
    document.getElementById('dbmNodeDetails').style.display = 'none';

        // Evento ao clicar num Node
        dbmCyInstance.on('tap', 'node', function(evt){
            var node = evt.target;
            var actEdges = node.data('actEdges');
            var inactEdges = node.data('inactEdges');

            document.getElementById('dbmActEdges').innerText = actEdges || "Nenhuma";
            document.getElementById('dbmInactEdges').innerText = inactEdges || "Nenhuma";
            
            // Mostra o painel com animação rápida
            var panel = document.getElementById('dbmNodeDetails');
            panel.style.display = 'block';
        });

        // Evento para fechar o painel se clicar no fundo vazio do grafo
        dbmCyInstance.on('tap', function(evt){
            if(evt.target === dbmCyInstance){
                document.getElementById('dbmNodeDetails').style.display = 'none';
            }
        });

    }, 250);
}


window.animateDFS = function(visitLog) {
    if (!currentCytoscapeInstance || !visitLog || visitLog.length === 0) return;
    
    var cy = currentCytoscapeInstance;
    let step = 0;
    
    // Cálculo inteligente da velocidade:
    // Se ele explorar 19 zonas, demora uns 300ms em cada uma (dá para ver bem).
    // Se ele explorar 5000 zonas, acelera para 10ms para não demorar horas!
    let speed = Math.max(10, Math.min(300, 5000 / visitLog.length));

    function nextStep() {
        cy.elements().removeClass('dfs-node'); // Limpa a cor do anterior
        
        if (step >= visitLog.length) {
            return; // Fim do caminho
        }

        let stateName = visitLog[step];
        
        // Encontra o nó no Grafo Principal através do texto do nome do estado
        let node = cy.nodes().filter(n => {
            let lbl = n.data('label');
            return lbl === stateName || lbl.startsWith(stateName + '\n');
        });
        
        if (node.length > 0) {
            node.addClass('dfs-node');
        }

        step++;
        setTimeout(nextStep, speed);
    }
    
    nextStep();
}