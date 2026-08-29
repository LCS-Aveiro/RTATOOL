function MermaidSVG() {
    const container = document.getElementById('mermaidContainer');
    const svgElement = container.querySelector('svg');

    if (!svgElement) {
        alert("Please open the Mermaid tab first.");
        return;
    }

    const clonedSvg = svgElement.cloneNode(true);
    const fontSize = "14px";
    const textColor = "#000000";

    const style = document.createElementNS("http://www.w3.org/2000/svg", "style");
    style.textContent = `

    foreignObject {
            overflow: visible !important;
        }


        .edgeLabel rect, .label rect {
            display: none !important;
        }


        .edgeLabel span {
            font-family: Arial, sans-serif !important;
            font-size: ${fontSize} !important;
            color: ${textColor} !important;
            font-weight: bold !important;
            background-color: white !important;
            padding: 1px 4px !important;
            border: 1px solid #999 !important; 
            border-radius: 3px !important;
            display: inline-block !important;
            
            transform: translate(-50%, -50%) !important;
            position: absolute !important;
            white-space: nowrap !important;
        }

        .edgeLabel span:empty {
            display: none !important;
        }

        .nodeLabel, .node span {
            font-family: Arial, sans-serif !important;
            font-size: ${fontSize} !important;
            color: ${textColor} !important;
            font-weight: bold !important;
            background: none !important;
            border: none !important;
        }

        .marker.cross path {
            stroke: red !important;
            stroke-width: 2px !important;
        }

        text {
            font-family: Arial, sans-serif !important;
            font-size: ${fontSize} !important;
            fill: ${textColor} !important;
        }
    `;
    clonedSvg.insertBefore(style, clonedSvg.firstChild);


    const serializer = new XMLSerializer();
    let svgData = serializer.serializeToString(clonedSvg);

    if (!svgData.match(/xmlns="http\:\/\/www\.w3\.org\/2000\/svg"/)) {
        svgData = svgData.replace(/^<svg/, '<svg xmlns="http://www.w3.org/2000/svg"');
    }

    

   return svgData;
}

function downloadMermaidSVG() {

    const svgBlob = new Blob(['<?xml version="1.0" encoding="UTF-8" standalone="no"?>\r\n' + MermaidSVG()], {
        type: "image/svg+xml;charset=utf-8"
    });
    
    const url = URL.createObjectURL(svgBlob);
    const downloadLink = document.createElement("a");
    downloadLink.href = url;
    downloadLink.download = "rta.svg";
    document.body.appendChild(downloadLink);
    downloadLink.click();
    document.body.removeChild(downloadLink);
}


 function svgToTikz(svgElement) {
    const viewBox = svgElement.viewBox.baseVal;
    const offsetX = viewBox ? viewBox.x : 0;
    const offsetY = viewBox ? viewBox.y : 0;
    const scale = 0.018; 
    
    let tikz = "% In LaTeX: \\usepackage{tikz} \\usetikzlibrary{arrows.meta}\n";
    tikz += "\\begin{tikzpicture}[yscale=-1, x=1cm, y=1cm, >=Stealth]\n";

    function parseColor(color) {
        if (!color || color === 'none' || color === 'transparent') return null;
        if (color.startsWith('#')) {
            const hex = color.replace('#', '');
            const r = (parseInt(hex.length === 3 ? hex[0]+hex[0] : hex.slice(0, 2), 16) / 255).toFixed(2);
            const g = (parseInt(hex.length === 3 ? hex[1]+hex[1] : hex.slice(2, 4), 16) / 255).toFixed(2);
            const b = (parseInt(hex.length === 3 ? hex[2]+hex[2] : hex.slice(4, 6), 16) / 255).toFixed(2);
            return `{rgb,1:red,${r};green,${g};blue,${b}}`;
        }
        return color; 
    }

    function getStyles(el) {
        const styleStr = el.getAttribute('style') || "";
        const inline = {};
        styleStr.split(';').forEach(s => {
            const [k, v] = s.split(':');
            if (k && v) inline[k.trim()] = v.trim();
        });

        const fill = parseColor(inline['fill'] || el.getAttribute('fill'));
        const stroke = parseColor(inline['stroke'] || el.getAttribute('stroke'));
        const sw = parseFloat(inline['stroke-width'] || el.getAttribute('stroke-width') || 1);
        const dash = inline['stroke-dasharray'] || el.getAttribute('stroke-dasharray');

        let res = [];
        if (fill && fill !== 'none') res.push(`fill=${fill}`);
        if (stroke && stroke !== 'none') res.push(`draw=${stroke}`);
        res.push(`line width=${(isNaN(sw) ? 0.5 : sw * 0.5).toFixed(1)}pt`);

        if (dash && dash !== '0' && dash !== 'none') {
            const dashVal = parseFloat(dash.split(/[\s,]+/)[0]);
            if (dashVal <= 3) res.push('dotted');
            else res.push('dashed');
        }

        const markerEnd = el.getAttribute('marker-end') || "";
        if (markerEnd.includes('pointEnd')) res.push(`->`);
      
        return res.length ? `[${res.join(', ')}] ` : "";
    }

    function process(el, ax, ay) {
        let x = ax, y = ay;
        const trans = el.getAttribute('transform');
        if (trans && trans.includes('translate')) {
            const m = trans.match(/translate\(([^,)]+)[, ]?([^)]+)?\)/);
            if (m) { x += parseFloat(m[1]) || 0; y += parseFloat(m[2]) || 0; }
        }

        const fX = (v) => ((parseFloat(v) + x - offsetX) * scale).toFixed(3);
        const fY = (v) => ((parseFloat(v) + y - offsetY) * scale).toFixed(3);

        if (el.tagName === 'rect') {
            const styleStr = el.getAttribute('style') || "";
            const wAttr = parseFloat(el.getAttribute('width') || 0);
            if (wAttr > 0 && !styleStr.includes('width: 0')) {
                const h = parseFloat(el.getAttribute('height') || 0);
                const rx = parseFloat(el.getAttribute('x') || 0);
                const ry = parseFloat(el.getAttribute('y') || 0);
                tikz += `  \\draw${getStyles(el)} (${fX(rx)},${fY(ry)}) rectangle (${fX(rx + wAttr)},${fY(ry + h)});\n`;
            }
        }

        if (el.tagName === 'path' && !el.classList.contains('arrowMarkerPath')) {
            const d = el.getAttribute('d');
            const markerEnd = el.getAttribute('marker-end') || "";
            if (d) {
                let p = d.replace(/([MLCQZ])([^MLCQZ]*)/gi, (m, c, a) => {
                    const pts = a.trim().split(/[\s,]+/).map(parseFloat);
                    if (pts.some(isNaN) && c.toUpperCase() !== 'Z') return "";
                    if (c.toUpperCase() === 'M') return `(${fX(pts[0])},${fY(pts[1])}) `;
                    if (c.toUpperCase() === 'L') return `-- (${fX(pts[0])},${fY(pts[1])}) `;
                    if (c.toUpperCase() === 'C') return `.. controls (${fX(pts[0])},${fY(pts[1])}) and (${fX(pts[2])},${fY(pts[3])}) .. (${fX(pts[4])},${fY(pts[5])}) `;
                    if (c.toUpperCase() === 'Z') return `-- cycle`;
                    return "";
                });
                if (p.trim()) {
                    let suffix = ";";
                    if (markerEnd.includes('crossEnd')) {
                        suffix = " node[at end, sloped, anchor=center, inner sep=0pt, text=red, font=\\bfseries\\small] {X};";
                    }
                    tikz += `  \\draw${getStyles(el)} ${p.trim()}${suffix}\n`;
                }
            }
        }

        if (el.tagName === 'span' || (el.tagName === 'text' && !el.closest('marker'))) {
            const txt = el.textContent.trim();
            if (txt && txt.length < 300) {
                const fo = el.closest('foreignObject');
                let tx, ty;
                if (fo) {
                    const w = parseFloat(fo.getAttribute('width') || 0);
                    const h = parseFloat(fo.getAttribute('height') || 0);
                    tx = parseFloat(fo.getAttribute('x') || 0) + w/2;
                    ty = parseFloat(fo.getAttribute('y') || 0) + h/2;
                } else {
                    tx = parseFloat(el.getAttribute('x') || 0);
                    ty = parseFloat(el.getAttribute('y') || 0);
                }
                const safeTxt = txt.replace(/([_#&$%])/g, '\\$1');
                tikz += `  \\node at (${fX(tx)},${fY(ty)}) {\\small\\textbf{${safeTxt}}};\n`;
            }
        }
        Array.from(el.children).forEach(c => process(c, x, y));
    }

    process(svgElement, 0, 0);
    tikz += "\\end{tikzpicture}";
    return tikz;
}


function downloadLatex() {
    const svgContent = MermaidSVG();
    let element;

    if (typeof svgContent === "string") {
        const parser = new DOMParser();
        const doc = parser.parseFromString(svgContent, "image/svg+xml");
        element = doc.querySelector("svg");
    } else {
        element = svgContent;
    }

    const tikzCode = svgToTikz(element);

    const blob = new Blob([tikzCode], { type: "text/plain;charset=utf-8" });
    
    const url = URL.createObjectURL(blob);
    const downloadLink = document.createElement("a");
    downloadLink.href = url;
    downloadLink.download = "rta.tex"; 
    document.body.appendChild(downloadLink);
    downloadLink.click();
    
    document.body.removeChild(downloadLink);
    URL.revokeObjectURL(url); 
}


function downloadPNG() {
    if (!currentCytoscapeInstance) {
        alert("Carregue o modelo primeiro.");
        return;
    }

    const pngData = currentCytoscapeInstance.png({
        full: true,
        bg: '#ffffff',
        scale: 2
    });

    const link = document.createElement("a");
    link.href = pngData;
    link.download = "rta-graph.png";
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
}



function translateToGLTS() {
    var newCode = RTA.translateToGLTS();
    if (newCode && !newCode.startsWith("Erro")) {
        editor.setValue(newCode);
        loadAndRender();
        alert(i18n[currentLang].alert_trans_ok);
    } else {
        alert(newCode);
    }
}

function downloadString(filename, content) {
    var blob = new Blob([content], { type: 'text/plain' });
    var a = document.createElement('a');
    a.href = window.URL.createObjectURL(blob);
    a.download = filename;
    a.click();
}




function downloadUppaal(type) {
    var sourceCode = editor.getValue();
    const graphId = getLayoutKey(sourceCode);
    var layoutJson = localStorage.getItem(`cyLayout_${graphId}`) || "{}";
    var content = ""; var name = "model.xml";

    if (type === 'tgrg') { 
        content = RTA.getUppaalTGRG(layoutJson); 
        name = "model_tgrg.xml"; 
    }
    if (type === 'teste') { 
        content = RTA.getUppaal(layoutJson); 
        name = "model_teste.xml"; 
    }
    if (content) downloadString(name, content);
    else alert("Modelo não carregado.");
}
