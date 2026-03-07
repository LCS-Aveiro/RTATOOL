# RTA Tool (Reconfigurable Timed Automata: Animated Analysis)

RTA is a formal verification and animation tool for Labelled Reactive Graphs. It was developed at the Department of Mathematics, University of Aveiro, with the support of FCT (Foundation for Science and Technology).

🔗 **Live Demo:** [https://joshua137k.github.io/RTATOOL/](https://joshua137k.github.io/RTATOOL/)

---

## 📋 Features

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

## 🚀 Building the Project

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

## 📂 Project Structure

*   `shared/` - Core logic (Parser, Semantics, Converters) shared between JS and JVM.
*   `js/` - Frontend logic (Cytoscape integration, DOM manipulation, rtaAPI).
*   `docs/` - Static files for the web interface (HTML, CSS, Libs).

---

## 📝 Example Model

```rta
init s0
s0 --> s1: a
s1 --> s0: b
a --! a: offA disabled
```
```