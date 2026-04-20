


const stripNameCommand = s => {
    return s.replace(/^\s*name\s+[a-zA-Z0-9_]+[;\s]*/gm, '');
};


function loadExample() {
    var select = document.getElementById("examplesSelect");
    var code = select.value;
    var descDiv = document.getElementById("exampleDesc");

    var selectedName = select.options[select.selectedIndex].text;

    if (code) {
        editor.setValue(code);

        const desc = exampleDescriptions[currentLang][selectedName];

        if (desc) {
            descDiv.innerText = desc;
            descDiv.style.display = "block";
        } else {
            descDiv.style.display = "none";
        }
    } else {
        descDiv.style.display = "none";
    }
}



function loadAndRender() {
    var fullCode = editor.getValue();
    var cleanCode = stripNameCommand(fullCode);
    console.log(fullCode);
    console.log(cleanCode);

    var jsonString = RTA.loadModel(cleanCode);
    var data = JSON.parse(jsonString);

    if (data.error) {
        alert(data.error);
    } else {
        textTraceHistory = [];
        jsTextHistory = [];
        var initialStateText = RTA.getCurrentStateText();
        jsTextHistory.push({ label: "Start ->", text: initialStateText });
        renderCytoscapeGraph("cytoscapeMainContainer", data, true);

        updateAllViews(jsonString);
        console.log(data);
        renderPdlHelpers(data);
    }
}



$(document).ready(function() {
    $('#simCollapse, #pdlBody').on('shown.bs.collapse hidden.bs.collapse', function () {
        $(this).css('height', '');
    });
});

document.addEventListener("DOMContentLoaded", function () {
    try {
        var examplesJson = RTA.getExamples();
        var examples = JSON.parse(examplesJson);
        var select = document.getElementById("examplesSelect");

        select.innerHTML = '<option value="">Carregar Exemplo...</option>';

        for (var key in examples) {
            var opt = document.createElement("option");
            opt.value = examples[key];
            opt.innerHTML = key;
            select.appendChild(opt);
        }
    } catch (e) { console.error("Erro ao carregar exemplos", e); }
});
