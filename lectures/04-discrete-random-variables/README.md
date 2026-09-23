# Seminar 4: Discrete random variables

Approved course materials for September 23, 2026.

- `discrete-rv-slides.html` and `.pdf`: browser slides and printable slide copy.
- `discrete-rv-slides.qmd`: editable slide source.
- `discrete-rv-live-demo.qmd`: nine independent R demonstrations for RStudio.
- `discrete-rv-live-demo.html`: reference copy with evaluated output.
- `poli210-seminar4.Rproj`: optional clean-workspace RStudio project.
- `discrete-rv-demo.R`: shared code used when rendering the slides.
- `diagrams/`: editable Typst/CeTZ diagrams and supplied SVG exports.

Open the companion QMD in RStudio and run one numbered chunk at a time. Each
chunk defines its own inputs and seed. The code uses base R. The supplied SVGs
allow rendering without regenerating the diagrams or fetching CeTZ.

From this directory, rebuild with:

```sh
quarto render discrete-rv-slides.qmd --to all
quarto render discrete-rv-live-demo.qmd --to html
```

Rendering requires `knitr` and `rmarkdown`. PDF output uses Typst, without LaTeX.
This directory is a separate Quarto project and does not inherit website navigation.
Render the website from the `course-materials` root afterward to refresh `docs/`.

Sources and permissions are credited in the slides. Instructor notes and assignment
solutions are not included in this public repository.
