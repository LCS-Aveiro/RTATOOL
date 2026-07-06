function getCytoscapeStyles() {
    return [
        { selector: 'node', style: { 'label': 'data(label)', 'text-valign': 'center', 'color': '#000000', 'font-family': 'sans-serif', 'font-weight': 'bold', 'text-outline-width': 2, 'text-outline-color': '#FFFFFF' } },
        
        { selector: 'edge', style: { 'width': 2, 'curve-style': 'unbundled-bezier', 'line-color': '#9CA3AF','target-arrow-shape': 'none', 'label': 'data(label)','color': '#000000', 'text-outline-color': '#FFFFFF','text-outline-width': 2,'font-size': '14px'} }, 
        { selector: 'edge[edgeDistances]', style: {'curve-style': 'unbundled-bezier','control-point-distances': 'data(edgeDistances)','control-point-weights': 'data(edgeWeights)','edge-distances': 'node-position'}},
        { selector: 'edge.from-action-node', style: { 'target-arrow-shape': 'triangle' } },
        
        { selector: 'node.state-node', style: { 'background-color': '#BFDBFE', 'shape': 'ellipse', 'width': 50, 'height': 50, 'border-width': 3, 'border-color': '#3B82F6', 'text-wrap': 'wrap', 'text-valign': 'center' } },
        { selector: 'node.has-invariant', style: { 'label': (ele) => ele.data('label') + '\n[' + ele.data('invariant') + ']' } },
        
        { selector: '.current-state', style: { 'background-color': '#86EFAC', 'border-color': '#166534', 'border-width': 4 } },
        
        { selector: 'node.event-node', style: { 'background-color': '#E5E7EB', 'shape': 'rectangle', 'width': 50, 'height': 30, 'border-width': 2, 'border-color': '#9CA3AF' } },
        
        { selector: '.enable-rule', style: { 'line-color': '#2563EB', 'target-arrow-color': '#2563EB' } },
        
        { selector: '.disable-rule', style: { 'line-color': '#DC2626', 'target-arrow-color': '#DC2626' } },
        { selector: 'edge.enable-rule.to-target', style: { 'target-arrow-shape': 'triangle-tee' } },
        { selector: 'edge.disable-rule.to-target', style: { 'target-label': 'X', 'target-text-offset': 5, 'color': '#DC2626', 'font-size': '12px' } },
        
        { selector: '.disabled', style: { 'line-style': 'dashed', 'background-opacity': 0.6, 'border-style': 'dashed', 'opacity': 0.7 } },
        
        { selector: '.transition-flash', style: { 'background-color': '#F97316', 'line-color': '#F97316', 'target-arrow-color': '#F97316' } },
        
        { selector: '.compound-parent', style: { 'background-color': '#F3F4F6', 'background-opacity': 1, 'border-color': '#D1D5DB', 'border-width': 2, 'content': 'data(label)', 'text-valign': 'top', 'text-halign': 'center', 'color': '#374151', 'font-weight': 'bold', 'font-size': '16px' } }
    ];
}

