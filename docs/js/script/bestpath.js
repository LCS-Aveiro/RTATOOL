

function updateBPField() {
    const type  = document.getElementById('bpType').value;
    const label = document.getElementById('bpValueLabel');
    const input = document.getElementById('bpValue');

    if (type === 'state') {
        label.innerText = "Target State";
        input.placeholder = "e.g. s1";
    } else {
        label.innerText = "Condition (Boolean Expression)";
        input.placeholder = "e.g. u1==0 && u2==0 && d1==1";
    }
}

function findPathByValue() {
    const type         = document.getElementById('bpType').value;
    const engine       = document.getElementById('bpEngine').value; 
    const targetValue  = document.getElementById('bpValue').value.trim();
    const summaryDiv   = document.getElementById('bpResultSummary');
    const pathDiv      = document.getElementById('bpResultPath');
    const resultBox    = document.getElementById('bpResultBox');

    if (!targetValue) {
        alert("Please enter a target (state name or condition).");
        return;
    }

    resultBox.style.display = 'block';
    summaryDiv.innerHTML = "Analyzing model...";
    summaryDiv.style.color = "var(--gray-700)";
    pathDiv.innerHTML = "";

    let response;
    
    if (type === 'state') {
        if (engine === 'symbolic') {
            response = RTA.findBestPathZone(targetValue);
        } else {
            response = RTA.findBestPath(targetValue);
        }
    } else {
        if (engine === 'symbolic') {
            response = RTA.findPathToValueZone(targetValue);
        } else {
            response = RTA.findPathToValue(targetValue);
        }
    }

    try {
        const data = (typeof response === 'string') ? JSON.parse(response) : response;

        if (data.error) {
            summaryDiv.innerHTML = "No path found";
            summaryDiv.style.color = "var(--red)";
            pathDiv.innerHTML = `<span style="font-size:11px; color:var(--gray-500)">${data.error}</span>`;
        } else if (Array.isArray(data)) {
            if (data.length === 0) {
                summaryDiv.innerHTML = "Target already reached!";
                summaryDiv.style.color = "#16a34a";
            } else {
                const pdlSequence = `⟨${data.join('; ')}⟩true`;
                summaryDiv.innerHTML = `Path found! (${data.length} steps)`;
                summaryDiv.style.color = "#2563eb";

                pathDiv.innerHTML = `
                    <div style="background:#f1f5f9; padding:10px; border:1px solid #e2e8f0; border-radius:4px; margin-top:5px;">
                        <code style="display:block; margin-bottom:8px; word-break: break-all; color:#334155;">${pdlSequence}</code>
                        <button class="u-btn primary" style="padding:2px 8px; font-size:10px;"
                                onclick="useInPdl('${pdlSequence}')">
                            <span class="glyphicon glyphicon-share-alt"></span> Send to PDL Verifier
                        </button>
                    </div>
                `;
            }
        }
    } catch (e) {
        console.error("Analysis error:", e);
        summaryDiv.innerHTML = "Error processing engine response.";
        summaryDiv.style.color = "var(--red)";
    }
}


function useInPdl(formula) {
    const pdlInput = document.getElementById('pdlFormula');
    if (pdlInput) {
        pdlInput.value = formula;

        pdlInput.style.backgroundColor = "#d9edf7";
        setTimeout(() => pdlInput.style.backgroundColor = "#fff", 500);

        pdlInput.focus();
    } else {
        alert("Campo PDL não encontrado!");
    }
}