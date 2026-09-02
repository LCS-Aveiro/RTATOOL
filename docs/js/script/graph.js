

var currentCytoscapeInstance = null;
var textTraceHistory = [];
var autoDelayTimer = null;
var storedDelayValue = 1.0;
var jsTextHistory = [];


const simpleHash = s => {
    let h = 0;
    for (let i = 0; i < s.length; i++) h = Math.imul(31, h) + s.charCodeAt(i) | 0;
    return String(h);
};

const getLayoutKey = s => {
    const match = s.match(/\bname\s+([a-zA-Z0-9_]+)/);
    return match && match[1] ? "name_" + match[1] : simpleHash(s.replace(/\s+/g, ''));
};


window.updateAllViews = function (json) {
    try {
        var data = JSON.parse(json);
        if (data.error) {
            console.error("Erro no modelo:", data.error);
            return;
        }

        if (typeof renderGlobalPanel === 'function') renderGlobalPanel(data, 'sidePanel');
        if (typeof renderGlobalPanel === 'function') renderGlobalPanel(data, 'sidePanel-bottom');
        if (typeof renderCytoscapeGraph === 'function') renderCytoscapeGraph("cytoscapeMainContainer", data, false);
        if (typeof renderTextView === 'function') renderTextView();
        if (typeof renderMermaidView === 'function') renderMermaidView();
        if (typeof renderPdlHelpers === 'function') renderPdlHelpers(data);
        if (typeof updatePctlDropdowns === 'function') updatePctlDropdowns(data);

        if (data && data.panelData) {
            var sbStates = document.getElementById('sb-states');
            var trans = data.panelData.enabled || [];
            if (sbStates) sbStates.textContent = trans.length + ' transition' + (trans.length !== 1 ? 's' : '') + ' enabled';

            var sbModel = document.getElementById('sb-model');
            if (sbModel) { sbModel.textContent = 'Model active'; sbModel.style.color = '#86EFAC'; }
        }

        lastModelData = data;

    } catch (e) {
        console.error("Erro ao processar updateAllViews:", e);
    }

    if (typeof checkBreakpointsAfterStep === 'function') {
        checkBreakpointsAfterStep();
    }
};


function injectNotesToElements(sourceCode, elements) {
    var regex = /\/\/\s*note\s*:\s*([^\s]+)\s+"([^"]+)"/g;
    var match;
    var addedNotes = new Set();
    while ((match = regex.exec(sourceCode)) !== null) {
        var targetId = match[1];
        var text = match[2];
        var noteId = 'note_' + simpleHash(targetId + text);
        if (addedNotes.has(noteId)) continue;
        addedNotes.add(noteId);
        
        if (targetId === 'global') {
            if (!elements.some(el => el.data && el.data.id === noteId)) {
                elements.push({ group: 'nodes', data: { id: noteId, label: text, targetId: targetId }, classes: 'note-node' });
            }
        } else {
            var targetExists = elements.some(el => el.data && el.data.id === targetId);
            if (targetExists) {
                if (!elements.some(el => el.data && el.data.id === noteId)) {
                    elements.push({ group: 'nodes', data: { id: noteId, label: text, targetId: targetId }, classes: 'note-node' });
                    elements.push({ group: 'edges', data: { id: 'edge_' + noteId, source: noteId, target: targetId }, classes: 'note-edge' });
                }
            }
        }
    }
}

function renderCytoscapeGraph(mainContainerId, dataOrJson, isFirstRender) {
    var mainContainer = document.getElementById(mainContainerId);
    if (!mainContainer) return;

    var data = (typeof dataOrJson === 'string') ? JSON.parse(dataOrJson) : dataOrJson;
    var sourceCode = (typeof editor !== 'undefined') ? editor.getValue() : "";
    
    if (data && data.graphElements) {
        injectNotesToElements(sourceCode, data.graphElements);
    }
    
    applySavedPositions(data.graphElements, sourceCode);

    if (isFirstRender || !currentCytoscapeInstance) {
        setupInitialCytoscape(mainContainerId, data);
        return;
    }

    try {
        var existingNodes = currentCytoscapeInstance.nodes().map(n => n.id());
        var newIds = new Set(data.graphElements.map(el => el.data.id));
        var toRemove = currentCytoscapeInstance.elements().filter(el => !newIds.has(el.id()));

        currentCytoscapeInstance.remove(toRemove);
        currentCytoscapeInstance.json({ elements: data.graphElements });

        currentCytoscapeInstance.nodes().filter(n => !existingNodes.includes(n.id())).forEach(node => {
            if (node.hasClass('event-node') || node.hasClass('deadlock-node')) {
                var parts = node.id().split('_');
                var sourceNode = parts.length >= 2 ? currentCytoscapeInstance.getElementById(parts[1]) : null;
                if (sourceNode && sourceNode.length > 0) {
                    var pos = sourceNode.position();
                    node.position({ x: pos.x, y: pos.y - 70 });
                }
            }
        });

        if (data.lastTransition) {
            var trans = data.lastTransition;
            var actionNodeId = `event_${trans.from}_${trans.to}_${trans.tId}_${trans.label}`;
            var edgeTo = `s_to_a_${trans.from}_${actionNodeId}`;
            var edgeFrom = `a_to_s_${actionNodeId}_${trans.to}`;
            var elementsToFlash = currentCytoscapeInstance.elements(`#${actionNodeId}, #${edgeTo}, #${edgeFrom}`);
            if (elementsToFlash.length > 0) {
                elementsToFlash.addClass('transition-flash');
                setTimeout(() => elementsToFlash.removeClass('transition-flash'), 1000);
            }
        }

        if (window.applyFilters) window.applyFilters();
    } catch (e) {
        console.error("Erro ao atualizar grafo, recriando...", e);
        setupInitialCytoscape(mainContainerId, data);
    }
}


async function setupInitialCytoscape(mainContainerId, data) {
    var mainContainer = document.getElementById(mainContainerId);
    if (currentCytoscapeInstance) {
        currentCytoscapeInstance.destroy();
        currentCytoscapeInstance = null;
    }

    mainContainer.innerHTML = '';
    mainContainer.style.display = 'block';
    mainContainer.style.width = '100%';
    mainContainer.style.height = '100%';
    mainContainer.style.backgroundColor = '#ffffff';

    var sourceCode = (typeof editor !== 'undefined') ? editor.getValue() : JSON.stringify(data.graphElements);
    const graphId = getLayoutKey(sourceCode);

    if (!hasExistingLayoutsInLocalStorage()) await loadDefaultLayoutsFromSeedFile();

    var hasSavedLayout = applySavedPositions(data.graphElements, sourceCode);

    var cy = cytoscape({
        container: mainContainer,
        elements: data.graphElements,
        style: getCytoscapeStyles(),
        layout: {
            name: hasSavedLayout ? 'preset' : 'dagre',
            rankDir: 'LR',
            fit: true,
            padding: 50,
            spacingFactor: 1.2,
            animate: false
        },
        wheelSensitivity: 0.2,
        textureOnViewport: true,
        pixelRatio: 1
    });

    if (cy.edgeEditing) {
        cy.edgeEditing({
            undoable: false,
            bendDistancesFunction: function (ele, val) {
                if (val) {
                    ele.data('cyedgecontroleditingDistances', val);
                    ele.data('edgeDistances', val);
                }
                return ele.data('cyedgecontroleditingDistances') || ele.data('edgeDistances');
            },
            bendWeightsFunction: function (ele, val) {
                if (val) {
                    ele.data('cyedgecontroleditingWeights', val);
                    ele.data('edgeWeights', val);
                }
                return ele.data('cyedgecontroleditingWeights') || ele.data('edgeWeights');
            },
            controlPointDistancesFunction: function (ele, val) {
                if (val) {
                    ele.data('cyedgecontroleditingDistances', val);
                    ele.data('edgeDistances', val);
                }
                return ele.data('cyedgecontroleditingDistances') || ele.data('edgeDistances');
            },
            controlPointWeightsFunction: function (ele, val) {
                if (val) {
                    ele.data('cyedgecontroleditingWeights', val);
                    ele.data('edgeWeights', val);
                }
                return ele.data('cyedgecontroleditingWeights') || ele.data('edgeWeights');
            },
            bendPositionsFunction: function (ele, val) {
                if (val) ele.data('bendPositions', val);
                return ele.data('bendPositions');
            },
            controlPointPositionsFunction: function (ele, val) {
                if (val) ele.data('controlPointPositions', val);
                return ele.data('controlPointPositions');
            },
            anchorSize: 10,
            anchorColor: '#ff9e64',
            enableDoubleTapToCreateBendPoint: true,
            initBendPointsAutomated: false
        });
    }

    let saveTimeout;
    const triggerAutoSave = () => {
        clearTimeout(saveTimeout);
        saveTimeout = setTimeout(() => {
            const stableId = getLayoutKey(editor.getValue());
            autoSaveLayoutToLocalStorage(cy, stableId);
            console.log("Layout salvo automaticamente!");
        }, 300);
    };
    cy.on('dragfree', 'node', triggerAutoSave);
    
    cy.on('cyedgecontroleditingfree cyedgebendeditingfree', triggerAutoSave);
    cy.on('mouseup touchend', triggerAutoSave);

    cy.on('tapstart', 'edge', function(evt) {
        cy.edges().unselect();
        evt.target.select();
    });

    cy.on('tap', function(evt) {
        if (evt.target === cy) {
            cy.elements().unselect();
        }
    });

    cy.on('tap', 'node.event-node.enabled', function (evt) {
        if (window.TraceRecorder && TraceRecorder.isReplaying()) return;
        var node = evt.target;
        var parts = node.id().split('_');
        if (parts.length >= 5) {
            var edgeJson = JSON.stringify({ from: parts[1], to: parts[2], tId: parts[3], label: parts[4] });
          
            if (window.TraceRecorder) {
                TraceRecorder.setLabel(parts[4]);
            }
            var responseJson = RTA.takeStep(edgeJson);
            var newStateText = RTA.getCurrentStateText();
            jsTextHistory.push({ label: parts[4] + " ->", text: newStateText });
            updateAllViews(responseJson);
        }
    });


    cy.on('mouseover', 'node.event-node, edge', function (evt) {
        var target = evt.target;
        target.addClass('hovered');

        if (target.isNode() && target.hasClass('event-node')) {
            if (target.data('hover_label')) {
                target.data('original_label', target.data('label'));
                target.data('label', target.data('hover_label'));
            }
            if (target.hasClass('enabled')) {
                target.cy().container().style.cursor = 'pointer';
            }
        }
    });
    cy.on('mouseout', 'node.event-node, edge', function (evt) {
        var target = evt.target;
        target.removeClass('hovered');

        if (target.isNode() && target.hasClass('event-node')) {
            if (target.data('original_label')) {
                target.data('label', target.data('original_label'));
            }
            target.cy().container().style.cursor = 'default';
        }
    });

    cy.on('dbltap', 'edge.has-details', function (evt) {
        var rawText = evt.target.data('full_label');
        var contentPre = document.getElementById('edgeDetailContent');
        contentPre.textContent = formatCode(rawText);
        if (typeof Prism !== 'undefined') {
            contentPre.className = "language-clike";
            Prism.highlightElement(contentPre);
        }
        $('#edgeDetailModal').modal('show');
    });

    currentCytoscapeInstance = cy;
    setupContextMenu(cy);
    if (window.applyFilters) window.applyFilters();
}


function formatCode(code) {
    if (!code) return "";
    return code
        .replace(/;/g, ";\n")
        .replace(/(\d)if/g, "$1\nif")
        .replace(/\sif\s/g, "\nif ")
        .replace(/\sif\(/g, "\nif (")
        .replace(/\{/g, " {\n    ")
        .replace(/\}/g, "\n}")
        .replace(/then/g, " then ")
        .replace(/AND/g, " AND\n    ")
        .replace(/  +/g, ' ')
        .replace(/\n\s*/g, "\n    ")
        .replace(/\n    \}/g, "\n}")
        .trim();
}

function changeEdgeStyle(styleName) {
    if (!currentCytoscapeInstance) return;
    currentEdgeStyle = styleName;
    var edges = currentCytoscapeInstance.edges();
    edges.removeClass('taxi bezier straight');
    if (styleName === 'taxi') edges.style({ 'curve-style': 'taxi', 'taxi-direction': 'vertical' });
    else if (styleName === 'bezier') edges.style({ 'curve-style': 'bezier', 'control-point-step-size': 40 });
    else edges.style({ 'curve-style': 'straight' });
}

function doUndo() {
  if (window.TraceRecorder && TraceRecorder.isReplaying()) return;

  if (typeof RTA !== 'undefined') {
    if (window.TraceRecorder) {
      TraceRecorder.setLabel("undo");
    }

    var json = RTA.undo();
    var data = JSON.parse(json);

    if (!data.error) {
      updateAllViews(json);
    }
  }
}

function stopAutoDelay() {
    if (autoDelayTimer) {
        clearInterval(autoDelayTimer);
        autoDelayTimer = null;
    }
}
window.stopAutoDelay = stopAutoDelay;

function toggleAutoDelay(isChecked) {
    if (isChecked) {
        if (autoDelayTimer) return;
        const runStep = () => {
            var inp = document.getElementById('delayInputVal');
            var delay = inp ? parseFloat(inp.value) : 1.0;
            updateAllViews(RTA.advanceTime(delay));
        };
        runStep();
        autoDelayTimer = setInterval(runStep, 1000);
    } else {
        stopAutoDelay();
    }
}



function showStats() { document.getElementById("analysisResult").innerText = RTA.getStats(); }
function checkProblems() { document.getElementById("analysisResult").innerText = RTA.checkProblems(); }


$(document).ready(function () {
    loadAppSettings();
    $('#simCollapse, #pdlBody').on('shown.bs.collapse hidden.bs.collapse', function () {
        $(this).css('height', '');
    });
});