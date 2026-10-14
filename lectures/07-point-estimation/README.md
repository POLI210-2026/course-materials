# Seminar 7 Point estimation

Approved course materials for October 14, 2026.

- `point-estimation-slides.html` and `.pdf`: browser slides and printable copy.
- `point-estimation-slides.qmd`: editable slide source.
- `point-estimation-live-demo.qmd`: seven independently runnable base-R chunks.
- `point-estimation-live-demo.html`: reference copy with evaluated output.
- `poli210-seminar7.Rproj`: optional RStudio project for the demonstration.
- `R/figures.R`: reproducible plots used by the slides.

Demos 1-4 cover repeated studies, estimator properties, potential outcomes,
random assignment, and comparing simulated distributions. Demos 5-7 are an
optional extension on variance estimation and the t distribution.

Save the demonstration QMD and optional project file in the same folder.
Each numbered chunk supplies its own inputs and runs independently, without
external data or helper files.

To rebuild from this directory:

```sh
quarto render point-estimation-slides.qmd --to revealjs
quarto render point-estimation-slides.qmd --to typst
quarto render point-estimation-live-demo.qmd --to html
```

Rendering uses `knitr` and `rmarkdown`. Typst supplies PDF output without
LaTeX. This directory is a separate Quarto project. After rebuilding its
outputs, render the website from the course repository root to update `docs/`.

Instructor notes, assignment solutions, student information, and grading
rubrics are not included here.
