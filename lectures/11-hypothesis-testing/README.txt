POLI 210 — Seminar 11: Hypothesis testing
November 18, 2026

Student materials:
https://poli210-2026.github.io/course-materials/meetings/11-hypothesis-testing/

Download seminar11-demo.zip and extract it. Open poli210-seminar11.Rproj
in RStudio, then hypothesis-testing-demo.qmd. If needed, install packages
once in the R Console:
  install.packages(c("ggplot2", "dplyr", "knitr", "rmarkdown"))
Run the setup and subsequent sections in order. The demo is self-contained:
all data are simulated and no external data or helper scripts are required.
The HTML shows saved code and results; the browser does not execute R.

The lecture folder on GitHub also contains editable slide sources. To rebuild
from that folder with the packages above installed:
  quarto render hypothesis-testing-slides.qmd --to revealjs
  quarto render hypothesis-testing-slides.qmd --to typst
  quarto render hypothesis-testing-demo.qmd --to html

The PDF is the printable slide export. Slide sources use R/figures.R,
slides.scss, typst-show.typ, and math-fonts.html. The demo does not depend
on those files. References are included in the slides and demo.

A7 is distributed separately through private student repositories.
