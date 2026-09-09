#set document(title: "Probability and simulation", author: "Brad LeVeck")
#set page(width: 13.333in, height: 7.5in, margin: (x: .65in, top: .5in, bottom: .48in), numbering: "1", number-align: right)
#set text(font: "Arial", size: 21pt, fill: rgb("#172d42"))
#set par(justify: false, leading: .62em)
#set heading(numbering: none)
#show heading.where(level: 1): it => {
  pagebreak(weak: true)
  block(below: .65em)[#text(size: 31pt, weight: "bold", fill: rgb("#005a8b"), it.body)]
}
#show raw: set text(size: 16pt)
#set table(inset: 9pt)
#set list(spacing: .65em)
#set enum(spacing: .65em)
#align(center + horizon)[
  #text(size: 42pt, weight: "bold", fill: rgb("#005a8b"))[Probability and simulation]
  #v(20pt)
  #text(size: 27pt)[POLI 210 · Seminar 2]
  #v(30pt)
  Brad LeVeck \
  September 9, 2026
]
#pagebreak(weak: true)
