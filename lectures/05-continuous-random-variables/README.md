# Seminar 5 Continuous random variables

Approved course materials for September 30, 2026.

- `continuous-rv-slides.html` and `.pdf`: browser slides and printable copy.
- `continuous-rv-slides.qmd`: editable slide source.
- `continuous-rv-live-demo.qmd`: nine independently runnable base-R demonstrations.
- `continuous-rv-live-demo.html`: reference copy with evaluated output.
- `poli210-seminar5.Rproj`: optional RStudio project for the demonstration.
- `R/figures.R`: reproducible plots used by the slides.

Save the demonstration QMD and optional project file in the same folder.
Run a numbered chunk in RStudio using its green arrow. Each chunk supplies
its own inputs, so no data download or separate helper file is needed for
the live demonstration.

To rebuild from this directory:

```sh
quarto render continuous-rv-slides.qmd --to revealjs
quarto render continuous-rv-slides.qmd --to typst
quarto render continuous-rv-live-demo.qmd --to html
```

Rendering uses `knitr` and `rmarkdown`. Typst supplies PDF output without
LaTeX. This directory is a separate Quarto project. After rebuilding its
outputs, render the website from the course repository root to update `docs/`.

The slides credit readings and adapted examples. Instructor notes, assignment
solutions, student information, and grading rubrics are not included here.
