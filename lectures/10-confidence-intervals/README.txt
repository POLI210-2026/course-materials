POLI 210 — Seminar 10: Confidence intervals
November 4, 2026

Student materials:
https://poli210-2026.github.io/course-materials/meetings/10-confidence-intervals/

Download seminar10-demo.zip and extract it. Open poli210-seminar10.Rproj
in RStudio, then confidence-intervals-demo.qmd. Follow its package setup
instructions and run sections in order. The demo QMD is self-contained:
no data downloads or external helper scripts are needed. The HTML shows
saved code and results; the browser does not execute R.

The lecture folder also contains editable slide sources. To rebuild from
this directory, with ggplot2, dplyr, knitr, and rmarkdown installed:
  quarto render confidence-intervals-slides.qmd --to revealjs
  quarto render confidence-intervals-slides.qmd --to typst
  quarto render confidence-intervals-demo.qmd --to html

The PDF is the printable slide export. Slide sources use R/figures.R,
slides.scss, typst-show.typ, and math-fonts.html. The demo does not depend
on those files. References are included in the slides and demo.

A6 is distributed separately through private student repositories.
