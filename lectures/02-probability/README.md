# Seminar 2: Probability and simulation

The same Quarto source builds browser slides and a landscape Typst PDF.
From this directory:

```sh
quarto render probability-slides.qmd --to revealjs
quarto render probability-slides.qmd --to typst
```

Then render the website from the `course-materials` repository root. The deck
is a separate Quarto project so it does not inherit the website navigation.
`probability-demo.R` contains the live demonstration code; no extra R packages
are needed for the examples. Rendering requires `knitr` and `rmarkdown`.
