# Seminar 6 Sampling distributions

Approved course materials for October 7, 2026.

- `sampling-distributions-slides.html` and `.pdf`: browser slides and printable copy.
- `sampling-distributions-slides.qmd`: editable slide source.
- `sampling-distributions-live-demo.qmd`: 14 independently runnable base-R chunks, including Demo 8b.
- `sampling-distributions-live-demo.html`: reference copy with evaluated output.
- `poli210-seminar6.Rproj`: optional RStudio project for the demonstration.
- `R/figures.R`: reproducible plots used by the slides.

Save the demonstration QMD and optional project file in the same folder.
Each numbered chunk supplies its own inputs and runs independently.
Demo 8b covers functions, lists, cumulative-probability comparisons, and
seed sensitivity. Demo 11 covers the same-sample t statistic and two-sided tails.

To rebuild from this directory:

```sh
quarto render sampling-distributions-slides.qmd --to revealjs
quarto render sampling-distributions-slides.qmd --to typst
quarto render sampling-distributions-live-demo.qmd --to html
```

Rendering uses `knitr` and `rmarkdown`. Typst supplies PDF output without
LaTeX. This directory is a separate Quarto project. After rebuilding its
outputs, render the website from the course repository root to update `docs/`.

Instructor notes, assignment solutions, student information, and grading
rubrics are not included here.
