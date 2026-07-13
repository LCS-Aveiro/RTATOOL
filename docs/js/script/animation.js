window.playCounterexample = function (isSuccess, customPath, speedMs) {
    var path = customPath || window.lastCounterexample;
    if (!path || !currentCytoscapeInstance) return;

    var cy = currentCytoscapeInstance;
    var duration = speedMs || 900; 
    var pathColor = isSuccess ? '#10B981' : '#DC2626'; 
    var nodeColor = isSuccess ? '#D1FAE5' : '#FECACA';
    var borderColor = isSuccess ? '#059669' : '#B91C1C';

    cy.elements().removeClass('anim-visiting anim-visited anim-target cx-node cx-path');

    cy.style()
        .selector('.cx-path').style({ 'line-color': pathColor, 'target-arrow-color': pathColor, 'width': 5, 'transition-property': 'line-color, target-arrow-color, width', 'transition-duration': '0.1s' })
        .selector('.cx-node').style({ 'background-color': nodeColor, 'border-color': borderColor, 'border-width': 4, 'transition-property': 'background-color, border-color', 'transition-duration': '0.1s' })
        .update();

    let step = 0;
    function nextStep() {
        if (step >= path.length) {
            setTimeout(() => cy.elements().removeClass('cx-node cx-path'), 2000);
            return;
        }

        let eventNode = cy.getElementById(path[step]);
        if (eventNode.length > 0) {
            eventNode.addClass('cx-node');
            eventNode.incomers('edge').addClass('cx-path').sources().addClass('cx-node');
            eventNode.outgoers('edge').addClass('cx-path').targets().addClass('cx-node');
            
            if(duration > 100) cy.animate({ center: { eles: eventNode }, zoom: 1.2 }, { duration: duration/2 });
        }

        step++;
        setTimeout(nextStep, duration);
    }
    nextStep();
};