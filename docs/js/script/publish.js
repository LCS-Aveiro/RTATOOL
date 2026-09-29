
(function () {
    "use strict";

    async function initFromUrl() {
        var params = new URLSearchParams(window.location.search);
        var modelId = params.get("model");
        var exampleKey = params.get("example");

        if (modelId) {
            try {
                var res = await RTACommunity.getModel(modelId);
                var m = res.model;

                editor.setValue(m.code);

                try {
                    if (m.layout_json && m.layout_json !== "null") {
                        var key = getLayoutKey(m.code);
                        localStorage.setItem("cyLayout_" + key, m.layout_json);
                    }
                } catch (e) { }

                loadAndRender();
                showCanvasTab("cyTab");

                var sb = document.getElementById("sb-model");
                if (sb) sb.textContent = m.title + " (por " + m.username + ")";
                return;
            } catch (e) {
                console.error("Erro ao carregar modelo da comunidade:", e);
                alert("Não foi possível carregar este modelo da comunidade. A carregar exemplo padrão.");
            }
        }

        try {
            var examples = JSON.parse(RTA.getExamples());
            var key2 = (exampleKey && examples[exampleKey]) ? exampleKey : "TIMER";
            if (examples[key2]) {
                editor.setValue(examples[key2]);
                loadAndRender();
                showCanvasTab("cyTab");
            }
        } catch (e) {
            console.error("Erro na inicialização:", e);
        }
    }
    window.RTAInitFromURL = initFromUrl;

    function openPublishModal() {
        if (!window.RTA_CURRENT_USER) {
            if (confirm("Precisas de ter conta para publicar um modelo na galeria.\nQueres criar conta / entrar agora?")) {
                if (window.openAuthModal) window.openAuthModal("register");
            }
            return;
        }
        document.getElementById("pubTitle").value = "";
        document.getElementById("pubDesc").value = "";
        document.getElementById("pubPublic").checked = true;
        document.getElementById("pubError").textContent = "";
        document.getElementById("publishModal").style.display = "flex";
    }
    window.openPublishModal = openPublishModal;

    document.addEventListener("DOMContentLoaded", function () {
        var modal = document.getElementById("publishModal");
        if (!modal) return;

        document.getElementById("publishModalClose").onclick = function () { modal.style.display = "none"; };
        modal.addEventListener("click", function (e) { if (e.target === modal) modal.style.display = "none"; });

        document.getElementById("publishSubmitBtn").onclick = async function () {
            var title = document.getElementById("pubTitle").value.trim();
            var desc = document.getElementById("pubDesc").value.trim();
            var isPublic = document.getElementById("pubPublic").checked;
            var errEl = document.getElementById("pubError");
            errEl.textContent = "";

            if (!title) { errEl.textContent = "Escreve um título para o modelo."; return; }
            if (typeof editor === "undefined") { errEl.textContent = "Editor ainda não está pronto."; return; }

            var code = editor.getValue();
            if (!code || !code.trim()) { errEl.textContent = "O modelo está vazio."; return; }

            var layout = null;
            try { layout = captureLiveLayout(); } catch (e) {}
            if (!layout) {
                try {
                    var key = getLayoutKey(code);
                    layout = localStorage.getItem("cyLayout_" + key) || "{}";
                } catch (e) { layout = "{}"; }
            }

            try {
                var res = await RTACommunity.createModel({
                    title: title,
                    description: desc,
                    code: code,
                    layout: layout,
                    is_public: isPublic ? 1 : 0
                });
                modal.style.display = "none";
                if (confirm("Modelo publicado com sucesso! Queres ver a galeria agora?")) {
                    window.location.href = "index.html";
                }
            } catch (e) {
                errEl.textContent = e;
            }
        };
    });
})();
