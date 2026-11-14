# RTA Tool (Reconfigurable Timed Automata: Animated Analysis)

RTA is a formal verification and animation tool for Labelled Reactive Graphs. It was developed at the Department of Mathematics, University of Aveiro, with the support of FCT (Foundation for Science and Technology).


## Lineage & Acknowledgements


**ReTA** (Reconfigurable Timed Automata) builds upon and extends the foundational concepts of **Marge** (Labelled Reactive Graphs). 

For more details on the original Marge tool and the underlying theory of Reactive Graphs, please refer to the foundational paper: 📄 *Reactive Graphs in Action* - [https://link.springer.com/chapter/10.1007/978-3-031-71261-6_6](https://link.springer.com/chapter/10.1007/978-3-031-71261-6_6)

---

## Features

*   **Visual Editor:** Write Reactive Graph syntax with syntax highlighting.
*   **Interactive Simulation:** Click on nodes/transitions to step through the system.
*   **Time Travel:** Undo/Redo support for simulation steps.
*   **State Space Visualization:** View the current state or the full LTS (Labelled Transition System) using Mermaid or Cytoscape.
*   **PDL Verification:** Verify properties like `[a;b]s0` or `<act>true` directly in the tool.
*   **Exports:** Convert your models to **Uppaal** (RG, GLTS, TGRG).

---

## 🛠 Prerequisites

To build and run this project from source, you need:

1.  **JDK 17** (Required).
2.  **SBT** (Scala Build Tool).

---

## Building the Project

Start the SBT console (ensure you are using Java 17):

```bash
# Example for Windows Powershell
sbt -java-home "C:\Program Files\Java\jdk-17" -mem 4096
```

### 1. Building the Web Version (Frontend)

To generate the JavaScript code that powers the HTML interface:

```bash
# Inside sbt console:
rtaJS/fastLinkJS
```

*   **Output:** The compiled JavaScript will be placed in `docs/js/gen/main.js`.
*   **Run:** Open `docs/index.html` in your browser.


---

## Project Structure

*   `shared/` - Core logic (Parser, Semantics, Converters) shared between JS and JVM.
*   `js/` - Frontend logic (Cytoscape integration, DOM manipulation, rtaAPI).
*   `docs/` - Static files for the web interface (HTML, CSS, Libs).

---

## Example Model

```rta
init s0
s0 --> s1: a
s1 --> s0: b
a --! a: offA disabled
```
```

<!-- ORGANIZA-BRAIN-SECTION -->
---

## 🧠 Segundo Cérebro (Organiza) & RAG Local — Instruções para IA

> **Ficha Técnica deste Projeto no Organiza:**
> - **Nota ID:** `62` — **`RTATOOL (Verificação LTL, CTL & DBM)`** (Espaço: `Projetos`, Pai: `Ferramentas Formais LCS (React-Tool, RePaTool, ReTOOL, RTATOOL)`)
> - **API Base URL:** `https://sites.joshuadourado.pt/organiza/public/api.php`
> - **Chave API Escondida (NUNCA fazer commit):** Ler de `C:\Users\Utilizador\.organiza_key` (`$HOME\.organiza_key`)
> - **Servidor MCP & Motor RAG Local:** `C:\Users\Utilizador\.organiza-rag\server.mjs` (Índice SQLite FTS5 BM25 em `C:\Users\Utilizador\.organiza-rag\index.db`)
> - **Backup do RAG (se mudares de PC/Servidor):** Nota `id: 58` tem em anexo `organiza-brain-rag-kit.zip` (`api.php?r=files/91ae2f6c8177cbd2e6f88d72af67b9b5`)
> - **Python no Windows:** Usar sempre o launcher `py` (`Python 3.14` por omissão) em vez de `python` (`3.9`).

### Fluxo Rápido para Agentes (Claude, Antigravity, Cursor)
1. **Consultar a ficha técnica, diagnósticos e cenários de teste deste projeto:**
   - Via MCP (`organiza-brain-rag`): chamar `brain_read_note(id=62)` e `rag_search(query="...", workspace="ProjetosUA")`.
   - Via Terminal/HTTP: `GET https://sites.joshuadourado.pt/organiza/public/api.php?r=notes/62` com header `X-API-Key: (Get-Content "$HOME\.organiza_key" -Raw).Trim()`.
2. **Pesquisar rotas, SQL e funções localmente sem gastar tokens:**
   - `node "C:\Users\Utilizador\.organiza-rag\server.mjs" --search "TERMO" --workspace ProjetosUA`
3. **Ao concluir alterações neste projeto:**
   - Registar no Organiza via `POST ?r=brain/learn` (`mode: "append"`, `space: "Projetos"`, `title: "RTATOOL (Verificação LTL, CTL & DBM)"`) e atualizar o índice local com `node "C:\Users\Utilizador\.organiza-rag\server.mjs" --reindex`.
