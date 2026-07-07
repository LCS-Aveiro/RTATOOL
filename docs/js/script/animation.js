
window.playCounterexample = function (isSuccess) {
    if (!window.lastCounterexample || !currentCytoscapeInstance) return;

    var cy   = currentCytoscapeInstance;
    var path = window.lastCounterexample;

    var pathColor = isSuccess ? '#2563EB' : '#DC2626';
    var nodeColor = isSuccess ? '#BFDBFE' : '#FECACA';
    var borderColor = isSuccess ? '#1D4ED8' : '#B91C1C';

    cy.elements().removeClass('anim-visiting anim-visited anim-target cx-node cx-path');

    cy.style()
        .selector('.cx-path').style({ 'line-color': pathColor, 'target-arrow-color': pathColor, 'width': 5, 'line-style': 'solid', 'transition-property': 'line-color, target-arrow-color, width', 'transition-duration': '0.3s' })
        .selector('.cx-node').style({ 'background-color': nodeColor, 'border-color': borderColor, 'border-width': 4, 'transition-property': 'background-color, border-color', 'transition-duration': '0.3s' })
        .update();

    let step = 0;
    function nextStep() {
        if (step >= path.length) {
            // Remove highlighting after 4 seconds
            setTimeout(() => cy.elements().removeClass('cx-node cx-path'), 4000);
            return;
        }

        let eventNode = cy.getElementById(path[step]);
        if (eventNode.length > 0) {
            eventNode.addClass('cx-node');
            eventNode.incomers('edge').addClass('cx-path').sources().addClass('cx-node');
            eventNode.outgoers('edge').addClass('cx-path').targets().addClass('cx-node');
            cy.animate({ center: { eles: eventNode }, zoom: 1.2 }, { duration: 400 });
        }

        step++;
        setTimeout(nextStep, 900);
    }
    nextStep();
};