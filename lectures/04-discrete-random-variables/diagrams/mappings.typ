// Editable teaching diagrams inspired by Matthew Blackwell's slide 11.
// CeTZ version pinned to the locally available version for reproducibility.
#import "@preview/cetz:0.3.4": canvas, draw
#let ink = rgb("263d4a")
#let blue = rgb("176b8e")
#let pale = rgb("eaf2f5")
#let orange = rgb("985326")

#let coin-mapping() = canvas(length: 1cm, {
  import draw: *
  set-style(stroke: .8pt + ink)
  rect((-0.6, -0.3), (5.0, 1.9), fill: pale)
  content((2.2, 1.45), [Sample space $S$])
  content((9.0, 2.2), [Probability accumulates at each value])
  line((5.8, 0), (12.5, 0), mark: (end: ">"))
  content((12.5, -.5), $RR$)
  let outcomes = ("TT", "HT", "TH", "HH")
  let target = (6.6, 9.0, 9.0, 11.4)
  for i in range(4) {
    let start = (i * 1.4, .5)
    circle(start, radius: .43, fill: white)
    content(start, text(font: "Libertinus Sans", outcomes.at(i)))
    // Controls route each arrow beneath the sample space and number line.
    bezier((start.at(0), .07), (target.at(i), -.04),
      (start.at(0) + 1, -1.1 - .28*i),
      (target.at(i) - 1.3, -1.3 - .28*i),
      stroke: 1pt + blue, mark: (end: ">"))
  }
  for (j, x) in (6.6, 9.0, 11.4).enumerate() {
    line((x, -.1), (x, .1))
    content((x, -.45), $ #j $)
    circle((x, .52), radius: .48, fill: white)
    content((x, .52), $1/4$)
  }
  circle((9, 1.5), radius: .48, fill: white)
  content((9, 1.5), $1/4$)
  content((2.2, -2.0), [$X(s)$ counts heads])
  content((9, -2.0), [$p(1)=P(X=1)=1/4+1/4=1/2$])
})

#let squared-deviation-mapping() = canvas(length: 1cm, {
  import draw: *
  set-style(stroke: .8pt + ink)
  content((1.9, 2.9), [$X$: number treated])
  content((9.2, 2.9), [$Y=(X-1.5)^2$])
  content((5.4, 2.05), [Map values; add their probabilities])
  let ys = (1.3, .1, -1.1, -2.3)
  let probs = ($1/8$, $3/8$, $3/8$, $1/8$)
  let target = (-1.75, .65, .65, -1.75)
  for i in range(4) {
    circle((1, ys.at(i)), radius: .42, fill: pale)
    content((1, ys.at(i)), $ #i $)
    content((2.3, ys.at(i)), probs.at(i))
    bezier((2.9, ys.at(i)), (8.35, target.at(i)),
      (4.5, ys.at(i)), (6.6, target.at(i)),
      stroke: 1pt + if i == 1 or i == 2 {blue} else {orange},
      mark: (end: ">"))
  }
  circle((9.15, .65), radius: .62, fill: pale)
  content((9.15, .65), $0.25$)
  content((11.0, .65), [$3/8+3/8=3/4$])
  circle((9.15, -1.75), radius: .62, fill: white)
  content((9.15, -1.75), $2.25$)
  content((11.0, -1.75), [$1/8+1/8=1/4$])
  content((6.2, -3.25), [$E[Y]=0.25(3/4)+2.25(1/4)=0.75$])
})
