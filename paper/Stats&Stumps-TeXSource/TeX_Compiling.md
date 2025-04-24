# Compiling the TeX files

## 🛠️ Step-by-step Instructions


### 🤖 1. Use `latexmk` for Automation

If installed, `[latexmk]([url](https://mgeier.github.io/latexmk.html))` automates the entire process of compiling the tex files:

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
