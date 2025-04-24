# How to Compile a LaTeX File (Twice!)

When working with LaTeX, elements such as **table of contents**, **cross-references**, **citations**, and **indexes** often require multiple compilation passes to be correctly generated.

This guide walks you through how to compile a LaTeX document (e.g., `main.tex`) such that the output PDF is named `Stats&Stumps_CricketPrediction.pdf`.

---

## 📄 Goal

Compile `main.tex` into:

```
Stats&Stumps_CricketPrediction.pdf
```

---

## 🛠️ Step-by-step Instructions

### ✅ 1. Using `pdflatex` (Command Line)

Run the following commands:

```bash
pdflatex -jobname="Stats&Stumps_CricketPrediction" main.tex
pdflatex -jobname="Stats&Stumps_CricketPrediction" main.tex
```

- `-jobname` specifies the name of the output PDF.
- The second compilation ensures that all cross-references and tables are correctly resolved.

### 📚 2. If You're Using BibTeX (with Citations)

If your LaTeX document contains citations:

```bash
pdflatex -jobname="Stats&Stumps_CricketPrediction" main.tex
bibtex Stats&Stumps_CricketPrediction
pdflatex -jobname="Stats&Stumps_CricketPrediction" main.tex
pdflatex -jobname="Stats&Stumps_CricketPrediction" main.tex
```

This will generate the bibliography and resolve all references correctly.

### 🤖 3. Preferred: Use `latexmk` for Automation

If installed, `latexmk` automates the entire process:

```bash
latexmk -pdf -jobname="Stats&Stumps_CricketPrediction" main.tex
```

- It detects how many runs are needed and takes care of bibliographies and cross-references.

To clean up auxiliary files:

```bash
latexmk -c
```

---

## 💻 4. Using Overleaf

If you’re using Overleaf (online LaTeX editor), just rename the output in the project settings or download the compiled PDF and rename it manually to `Stats&Stumps_CricketPrediction.pdf`.

---

## 📝 Summary

| Tool      | Description                                  |
|-----------|----------------------------------------------|
| `pdflatex` | Run **twice** to resolve references          |
| `bibtex`   | Required if using citations                  |
| `latexmk`  | Automates multiple compilations              |
| Overleaf   | Handles everything in the cloud automatically|

---

✅ **Always compile at least twice** to ensure all references and contents are rendered correctly.  
🚀 **Use `latexmk`** for a hassle-free experience.
