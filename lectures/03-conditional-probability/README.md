# Seminar 3: Conditional probability and Bayes' rule

The Quarto source builds browser slides and a landscape Typst PDF. From this
directory:

```sh
quarto render conditional-probability-slides.qmd --to revealjs
quarto render conditional-probability-slides.qmd --to typst
```

Then render the website from the `course-materials` repository root. This deck
is a separate Quarto project so it does not inherit the website navigation.

`conditional-probability-demo.R` contains the shared demonstration code,
including Monty Hall and the coalition example. The simulations use base R.
Rendering requires `knitr` and `rmarkdown`.

`arrest-tree.dot` is the editable Graphviz diagram source. The supplied
`arrest-tree.svg` lets the slides render without installing Graphviz. If you
edit the DOT file, regenerate the SVG before rendering the slides, for example
with `dot -Tsvg arrest-tree.dot -o arrest-tree.svg` when Graphviz is installed.
