// Simple numbering for non-book documents
#let equation-numbering = "(1)"
#let callout-numbering = "1"
#let subfloat-numbering(n-super, subfloat-idx) = {
  numbering("1a", n-super, subfloat-idx)
}

// Theorem configuration for theorion
// Simple numbering for non-book documents (no heading inheritance)
#let theorem-inherited-levels = 0

// Theorem numbering format (can be overridden by extensions for appendix support)
// This function returns the numbering pattern to use
#let theorem-numbering(loc) = "1.1"

// Default theorem render function
#let theorem-render(prefix: none, title: "", full-title: auto, body) = {
  if full-title != "" and full-title != auto and full-title != none {
    strong[#full-title.]
    h(0.5em)
  }
  body
}
// Some definitions presupposed by pandoc's typst output.
#let content-to-string(content) = {
  if content.has("text") {
    content.text
  } else if content.has("children") {
    content.children.map(content-to-string).join("")
  } else if content.has("body") {
    content-to-string(content.body)
  } else if content == [ ] {
    " "
  }
}

#let horizontalrule = line(start: (25%,0%), end: (75%,0%))

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms.item: it => block(breakable: false)[
  #text(weight: "bold")[#it.term]
  #block(inset: (left: 1.5em, top: -0.4em))[#it.description]
]

// Some quarto-specific definitions.

#show raw.where(block: true): set block(
    fill: luma(230),
    width: 100%,
    inset: 8pt,
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let fields = old_block.fields()
  let _ = fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.abs
  }
  block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == str {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == content {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

// Subfloats
// This is a technique that we adapted from https://github.com/tingerrr/subpar/
#let quartosubfloatcounter = counter("quartosubfloatcounter")

#let quarto_super(
  kind: str,
  caption: none,
  label: none,
  supplement: str,
  position: none,
  subcapnumbering: "(a)",
  body,
) = {
  context {
    let figcounter = counter(figure.where(kind: kind))
    let n-super = figcounter.get().first() + 1
    set figure.caption(position: position)
    [#figure(
      kind: kind,
      supplement: supplement,
      caption: caption,
      {
        show figure.where(kind: kind): set figure(numbering: _ => {
          let subfloat-idx = quartosubfloatcounter.get().first() + 1
          subfloat-numbering(n-super, subfloat-idx)
        })
        show figure.where(kind: kind): set figure.caption(position: position)

        show figure: it => {
          let num = numbering(subcapnumbering, n-super, quartosubfloatcounter.get().first() + 1)
          show figure.caption: it => block({
            num.slice(2) // I don't understand why the numbering contains output that it really shouldn't, but this fixes it shrug?
            [ ]
            it.body
          })

          quartosubfloatcounter.step()
          it
          counter(figure.where(kind: it.kind)).update(n => n - 1)
        }

        quartosubfloatcounter.update(0)
        body
      }
    )#label]
  }
}

// callout rendering
// this is a figure show rule because callouts are crossreferenceable
#show figure: it => {
  if type(it.kind) != str {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let children = old_title_block.body.body.children
  let old_title = if children.len() == 1 {
    children.at(0)  // no icon: title at index 0
  } else {
    children.at(1)  // with icon: title at index 1
  }

  // TODO use custom separator if available
  // Use the figure's counter display which handles chapter-based numbering
  // (when numbering is a function that includes the heading counter)
  let callout_num = it.counter.display(it.numbering)
  let new_title = if empty(old_title) {
    [#kind #callout_num]
  } else {
    [#kind #callout_num: #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block,
    block_with_new_content(
      old_title_block.body,
      if children.len() == 1 {
        new_title  // no icon: just the title
      } else {
        children.at(0) + new_title  // with icon: preserve icon block + new title
      }))

  align(left, block_with_new_content(old_callout,
    block(below: 0pt, new_title_block) +
    old_callout.body.children.at(1)))
}

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black, body_background_color: white) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color,
        width: 100%,
        inset: 8pt)[#if icon != none [#text(icon_color, weight: 900)[#icon] ]#title]) +
      if(body != []){
        block(
          inset: 1pt, 
          width: 100%, 
          block(fill: body_background_color, width: 100%, inset: 8pt, body))
      }
    )
}


// syntax highlighting functions from skylighting:
/* Function definitions for syntax highlighting generated by skylighting: */
#let EndLine() = raw("\n")
#let Skylighting(fill: none, number: false, start: 1, sourcelines) = {
   let blocks = []
   let lnum = start - 1
   let bgcolor = rgb("#f1f3f5")
   for ln in sourcelines {
     if number {
       lnum = lnum + 1
       blocks = blocks + box(width: if start + sourcelines.len() > 999 { 30pt } else { 24pt }, text(fill: rgb("#aaaaaa"), [ #lnum ]))
     }
     blocks = blocks + ln + EndLine()
   }
   block(fill: bgcolor, width: 100%, inset: 8pt, radius: 2pt, blocks)
}
#let AlertTok(s) = text(fill: rgb("#ad0000"),raw(s))
#let AnnotationTok(s) = text(fill: rgb("#5e5e5e"),raw(s))
#let AttributeTok(s) = text(fill: rgb("#657422"),raw(s))
#let BaseNTok(s) = text(fill: rgb("#ad0000"),raw(s))
#let BuiltInTok(s) = text(fill: rgb("#003b4f"),raw(s))
#let CharTok(s) = text(fill: rgb("#20794d"),raw(s))
#let CommentTok(s) = text(fill: rgb("#5e5e5e"),raw(s))
#let CommentVarTok(s) = text(style: "italic",fill: rgb("#5e5e5e"),raw(s))
#let ConstantTok(s) = text(fill: rgb("#8f5902"),raw(s))
#let ControlFlowTok(s) = text(weight: "bold",fill: rgb("#003b4f"),raw(s))
#let DataTypeTok(s) = text(fill: rgb("#ad0000"),raw(s))
#let DecValTok(s) = text(fill: rgb("#ad0000"),raw(s))
#let DocumentationTok(s) = text(style: "italic",fill: rgb("#5e5e5e"),raw(s))
#let ErrorTok(s) = text(fill: rgb("#ad0000"),raw(s))
#let ExtensionTok(s) = text(fill: rgb("#003b4f"),raw(s))
#let FloatTok(s) = text(fill: rgb("#ad0000"),raw(s))
#let FunctionTok(s) = text(fill: rgb("#4758ab"),raw(s))
#let ImportTok(s) = text(fill: rgb("#00769e"),raw(s))
#let InformationTok(s) = text(fill: rgb("#5e5e5e"),raw(s))
#let KeywordTok(s) = text(weight: "bold",fill: rgb("#003b4f"),raw(s))
#let NormalTok(s) = text(fill: rgb("#003b4f"),raw(s))
#let OperatorTok(s) = text(fill: rgb("#5e5e5e"),raw(s))
#let OtherTok(s) = text(fill: rgb("#003b4f"),raw(s))
#let PreprocessorTok(s) = text(fill: rgb("#ad0000"),raw(s))
#let RegionMarkerTok(s) = text(fill: rgb("#003b4f"),raw(s))
#let SpecialCharTok(s) = text(fill: rgb("#5e5e5e"),raw(s))
#let SpecialStringTok(s) = text(fill: rgb("#20794d"),raw(s))
#let StringTok(s) = text(fill: rgb("#20794d"),raw(s))
#let VariableTok(s) = text(fill: rgb("#111111"),raw(s))
#let VerbatimStringTok(s) = text(fill: rgb("#20794d"),raw(s))
#let WarningTok(s) = text(style: "italic",fill: rgb("#5e5e5e"),raw(s))



#let article(
  title: none,
  subtitle: none,
  authors: none,
  keywords: (),
  date: none,
  abstract-title: none,
  abstract: none,
  thanks: none,
  cols: 1,
  lang: "en",
  region: "US",
  font: none,
  fontsize: 11pt,
  title-size: 1.5em,
  subtitle-size: 1.25em,
  heading-family: none,
  heading-weight: "bold",
  heading-style: "normal",
  heading-color: black,
  heading-line-height: 0.65em,
  mathfont: none,
  codefont: none,
  linestretch: 1,
  sectionnumbering: none,
  linkcolor: none,
  citecolor: none,
  filecolor: none,
  toc: false,
  toc_title: none,
  toc_depth: none,
  toc_indent: 1.5em,
  doc,
) = {
  // Set document metadata for PDF accessibility
  set document(title: title, keywords: keywords)
  set document(
    author: authors.map(author => content-to-string(author.name)).join(", ", last: " & "),
  ) if authors != none and authors != ()
  set par(
    justify: true,
    leading: linestretch * 0.65em
  )
  set text(lang: lang,
           region: region,
           size: fontsize)
  set text(font: font) if font != none
  show math.equation: set text(font: mathfont) if mathfont != none
  show raw: set text(font: codefont) if codefont != none

  set heading(numbering: sectionnumbering)

  show link: set text(fill: rgb(content-to-string(linkcolor))) if linkcolor != none
  show ref: set text(fill: rgb(content-to-string(citecolor))) if citecolor != none
  show link: this => {
    if filecolor != none and type(this.dest) == label {
      text(this, fill: rgb(content-to-string(filecolor)))
    } else {
      text(this)
    }
   }

  let has-title-block = title != none or (authors != none and authors != ()) or date != none or abstract != none
  if has-title-block {
    place(
      top,
      float: true,
      scope: "parent",
      clearance: 4mm,
      block(below: 1em, width: 100%)[

        #if title != none {
          align(center, block(inset: 2em)[
            #set par(leading: heading-line-height) if heading-line-height != none
            #set text(font: heading-family) if heading-family != none
            #set text(weight: heading-weight)
            #set text(style: heading-style) if heading-style != "normal"
            #set text(fill: heading-color) if heading-color != black

            #text(size: title-size)[#title #if thanks != none {
              footnote(thanks, numbering: "*")
              counter(footnote).update(n => n - 1)
            }]
            #(if subtitle != none {
              parbreak()
              text(size: subtitle-size)[#subtitle]
            })
          ])
        }

        #if authors != none and authors != () {
          let count = authors.len()
          let ncols = calc.min(count, 3)
          grid(
            columns: (1fr,) * ncols,
            row-gutter: 1.5em,
            ..authors.map(author =>
                align(center)[
                  #author.name \
                  #author.affiliation \
                  #author.email
                ]
            )
          )
        }

        #if date != none {
          align(center)[#block(inset: 1em)[
            #date
          ]]
        }

        #if abstract != none {
          block(inset: 2em)[
          #text(weight: "semibold")[#abstract-title] #h(1em) #abstract
          ]
        }
      ]
    )
  }

  if toc {
    let title = if toc_title == none {
      auto
    } else {
      toc_title
    }
    block(above: 0em, below: 2em)[
    #outline(
      title: toc_title,
      depth: toc_depth,
      indent: toc_indent
    );
    ]
  }

  doc
}

#set table(
  inset: 6pt,
  stroke: none
)
#let brand-color = (:)
#let brand-color-background = (:)
#let brand-logo = (:)

#set page(
  paper: "us-letter",
  margin: (x: 1.25in, y: 1.25in),
  numbering: "1",
  columns: 1,
)

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

= Today's questions
<todays-questions>
- What are the possible outcomes of a random process?
- Which outcomes count as the event we care about?
- When can we calculate a probability by counting?
- How can R simulate the same process?

#strong[Reading:] PSD §§2.1, 2.2, and 2.4. Conditional probability and Bayes' rule are for next week.

= Why political scientists use probability
<why-political-scientists-use-probability>
#strong[Polling:] Even with a fixed population, different samples give different results.

#strong[Experiments:] A random assignment procedure determines who receives treatment.

#strong[Our task today:] Describe the procedure, then ask how often an event occurs.

The political examples use hypothetical numbers, not current polling estimates.

= The language of a probability model
<the-language-of-a-probability-model>
#table(
  columns: 3,
  align: (auto,auto,auto,),
  table.header([Term], [Meaning], [Two coin tosses],),
  table.hline(),
  [Random experiment], [Procedure with an uncertain outcome], [Toss twice],
  [Outcome], [One complete result], [$H T$],
  [Sample space $S$], [Set of all possible outcomes], [${ H H \, H T \, T H \, T T }$],
  [Event $E$], [A subset of $S$], [Exactly one head: ${ H T \, T H }$],
)
A #strong[trial] is one repetition of the entire experiment. Random does #strong[not] automatically mean equally likely.

= Describe outcomes before assigning probabilities
<describe-outcomes-before-assigning-probabilities>
Two independent fair tosses give four equally likely ordered outcomes:

$ S = { H H \, H T \, T H \, T T } . $

But the possible #strong[counts] of heads are not equally likely:

#table(
  columns: 3,
  align: (right,auto,right,),
  table.header([Heads], [Outcomes], [Probability],),
  table.hline(),
  [0], [$T T$], [$1 \/ 4$],
  [1], [$H T \, T H$], [$1 \/ 2$],
  [2], [$H H$], [$1 \/ 4$],
)
The representation of an outcome matters.

= Events are sets
<events-are-sets>
#table(
  columns: 3,
  align: (auto,auto,auto,),
  table.header([Notation], [Read it as], [In R, for logical vectors #NormalTok("E");, #NormalTok("F");],),
  table.hline(),
  [$E sect F$], [Both $E$ and $F$], [#NormalTok("E & F");],
  [$E union F$], [$E$ or $F$, including both], [#NormalTok("E"); | #NormalTok("F");],
  [$overline(E)$], [Not $E$], [#NormalTok("!E");],
  [$nothing$], [Impossible event], [No outcomes],
  [$E subset.eq F$], [Every outcome in $E$ is in $F$], [$E$ implies $F$],
)
#strong[Disjoint events:] $E sect F = nothing$. They cannot both occur in one trial.

= What makes a probability assignment valid?
<what-makes-a-probability-assignment-valid>
+ $P \( E \) gt.eq 0$ for every event $E$.
+ $P \( S \) = 1$.
+ Probabilities add for disjoint events: $P \( E union F \) = P \( E \) + P \( F \)$ when $E sect F = nothing$.

The third rule extends to any countable collection of pairwise disjoint events.

We can distribute probability unevenly across outcomes. The total must still be 1.

= Three useful consequences
<three-useful-consequences>
#strong[Complement] $ P \( overline(E) \) = 1 - P \( E \) . $

#strong[Union] $ P \( E union F \) = P \( E \) + P \( F \) - P \( E sect F \) . $

#strong[Containment:] If $E subset.eq F$, then $P \( E \) lt.eq P \( F \)$.

Why do we subtract the intersection in the union rule?

= A voting example: organize the overlap
<a-voting-example-organize-the-overlap>
In a hypothetical population, let:

- $E$: plans to vote in the presidential election, $P \( E \) = 0.55$\;
- $F$: plans to vote in the congressional election, $P \( F \) = 0.36$\;
- $P \( E sect F \) = 0.35$.

#strong[With a neighbor:] Find the probability of planning to vote in at least one election, in neither, and in exactly one.

Start by splitting $S$ into four disjoint pieces.

= Four disjoint pieces make the calculation clear
<four-disjoint-pieces-make-the-calculation-clear>
#table(
  columns: 2,
  align: (auto,right,),
  table.header([Piece of the sample space], [Probability],),
  table.hline(),
  [Both elections], [$0.35$],
  [Presidential only], [$0.55 - 0.35 = 0.20$],
  [Congressional only], [$0.36 - 0.35 = 0.01$],
  [Neither], [$1 - \( 0.35 + 0.20 + 0.01 \) = 0.44$],
)
At least one: $0.56$. Exactly one: $0.21$.

Check: the four disjoint pieces sum to 1.

= When counting gives a probability
<when-counting-gives-a-probability>
If $S$ is finite and its outcomes are #strong[equally likely],

$ P \( E \) = frac(\| E \|, \| S \|) . $

Two independently rolled fair dice have $6 times 6 = 36$ equally likely ordered pairs.

Their sums run from 2 through 12, but those 11 sums are #strong[not] equally likely.

#strong[Checkpoint:] Would #NormalTok("sample(2:12, 1)"); simulate the sum correctly?

= Enumerate a sample space in R
<enumerate-a-sample-space-in-r>
#block[
#Skylighting(([#NormalTok("S ");#OtherTok("<-");#NormalTok(" ");#FunctionTok("expand.grid");#NormalTok("(");#AttributeTok("first =");#NormalTok(" ");#DecValTok("1");#SpecialCharTok(":");#DecValTok("6");#NormalTok(", ");#AttributeTok("second =");#NormalTok(" ");#DecValTok("1");#SpecialCharTok(":");#DecValTok("6");#NormalTok(")");],
[#FunctionTok("head");#NormalTok("(S, ");#DecValTok("4");#NormalTok(")");],));
#block[
#Skylighting(([#NormalTok("  first second");],
[#NormalTok("1     1      1");],
[#NormalTok("2     2      1");],
[#NormalTok("3     3      1");],
[#NormalTok("4     4      1");],));
]
#Skylighting(([#FunctionTok("nrow");#NormalTok("(S)");],));
#block[
#Skylighting(([#NormalTok("[1] 36");],));
]
]
Each row is one complete outcome. This table is the full sample space, not a random sample from it.

= Select an event, then count it
<select-an-event-then-count-it>
#block[
#Skylighting(([#NormalTok("E ");#OtherTok("<-");#NormalTok(" (S");#SpecialCharTok("$");#NormalTok("first ");#SpecialCharTok("+");#NormalTok(" S");#SpecialCharTok("$");#NormalTok("second) ");#SpecialCharTok("==");#NormalTok(" ");#DecValTok("7");],
[#FunctionTok("sum");#NormalTok("(E)                 ");#CommentTok("# number of favorable outcomes");],));
#block[
#Skylighting(([#NormalTok("[1] 6");],));
]
#Skylighting(([#FunctionTok("mean");#NormalTok("(E)                ");#CommentTok("# favorable / total");],));
#block[
#Skylighting(([#NormalTok("[1] 0.1666667");],));
]
]
In R, #NormalTok("TRUE"); acts like 1 and #NormalTok("FALSE"); like 0 in these calculations.

This mean is an #strong[exact probability] because the table contains every equally likely outcome once.

= sample() generates random draws
<sample-generates-random-draws>
#block[
#Skylighting(([#FunctionTok("set.seed");#NormalTok("(");#DecValTok("210");#NormalTok(")");],
[#FunctionTok("sample");#NormalTok("(");#DecValTok("1");#SpecialCharTok(":");#DecValTok("6");#NormalTok(", ");#AttributeTok("size =");#NormalTok(" ");#DecValTok("12");#NormalTok(", ");#AttributeTok("replace =");#NormalTok(" ");#ConstantTok("TRUE");#NormalTok(")");],));
#block[
#Skylighting(([#NormalTok(" [1] 4 3 1 1 6 3 1 4 1 2 6 5");],));
]
]
- #NormalTok("x");: the values to draw from (here #NormalTok("1:6");).
- #NormalTok("size");: how many draws to return.
- #NormalTok("replace");: can the same entry be drawn again?
- #NormalTok("prob");: optional relative probabilities; the default is uniform.

#NormalTok("set.seed()"); makes a simulation reproducible; it does not make a model correct.

= Replacement is a modeling choice
<replacement-is-a-modeling-choice>
#block[
#Skylighting(([#FunctionTok("sample");#NormalTok("(");#DecValTok("1");#SpecialCharTok(":");#DecValTok("6");#NormalTok(", ");#AttributeTok("size =");#NormalTok(" ");#DecValTok("2");#NormalTok(", ");#AttributeTok("replace =");#NormalTok(" ");#ConstantTok("TRUE");#NormalTok(")   ");#CommentTok("# two die rolls");],
[#FunctionTok("sample");#NormalTok("(");#DecValTok("1");#SpecialCharTok(":");#DecValTok("6");#NormalTok(", ");#AttributeTok("size =");#NormalTok(" ");#DecValTok("2");#NormalTok(", ");#AttributeTok("replace =");#NormalTok(" ");#ConstantTok("FALSE");#NormalTok(")  ");#CommentTok("# two distinct slips");],));
]
Dice may show the same number twice. Two distinct slips cannot be the same slip.

#strong[Important:] One trial can contain multiple draws. To simulate two dice, one trial must return two rolls---not one roll and not all 36 possible pairs.

= A general simulation recipe
<a-general-simulation-recipe>
+ Describe one trial precisely.
+ Generate its complete outcome.
+ Record whether the event occurred: 1 or 0.
+ Repeat the trial many times.
+ Average those indicators.

$ hat(p)_N = 1 / N sum_(i = 1)^N I_i \, #h(2em) I_i = cases(delim: "{", 1 & upright("event occurs in trial ") i \,, 0 & upright("otherwise.")) $

The result estimates the probability #strong[under the model we simulated].

= Simulate the two-dice event
<simulate-the-two-dice-event>
#block[
#Skylighting(([#FunctionTok("set.seed");#NormalTok("(");#DecValTok("211");#NormalTok(")");],
[#NormalTok("N ");#OtherTok("<-");#NormalTok(" ");#DecValTok("20000");],
[#NormalTok("first ");#OtherTok("<-");#NormalTok(" ");#FunctionTok("sample");#NormalTok("(");#DecValTok("1");#SpecialCharTok(":");#DecValTok("6");#NormalTok(", N, ");#AttributeTok("replace =");#NormalTok(" ");#ConstantTok("TRUE");#NormalTok(")");],
[#NormalTok("second ");#OtherTok("<-");#NormalTok(" ");#FunctionTok("sample");#NormalTok("(");#DecValTok("1");#SpecialCharTok(":");#DecValTok("6");#NormalTok(", N, ");#AttributeTok("replace =");#NormalTok(" ");#ConstantTok("TRUE");#NormalTok(")");],
[#NormalTok("hit ");#OtherTok("<-");#NormalTok(" (first ");#SpecialCharTok("+");#NormalTok(" second) ");#SpecialCharTok("==");#NormalTok(" ");#DecValTok("7");],
[#FunctionTok("c");#NormalTok("(");#AttributeTok("estimate =");#NormalTok(" ");#FunctionTok("mean");#NormalTok("(hit), ");#AttributeTok("exact =");#NormalTok(" ");#DecValTok("6");#NormalTok(" ");#SpecialCharTok("/");#NormalTok(" ");#DecValTok("36");#NormalTok(")");],));
#block[
#Skylighting(([#NormalTok(" estimate     exact ");],
[#NormalTok("0.1646000 0.1666667 ");],));
]
]
Entry #NormalTok("i"); in each vector belongs to trial #NormalTok("i");.

The Monte Carlo estimate usually differs slightly from the exact probability.

= More trials stabilize the estimate
<more-trials-stabilize-the-estimate>
#box(image("probability-slides_files/figure-typst/unnamed-chunk-6-1.png", alt: "A cumulative simulated event proportion stabilizes near one sixth as the number of trials increases; the dashed line marks the exact probability.", width: 86%))

The dashed line is the exact probability, $1 \/ 6$. The law of large numbers supports stabilization for independent repeated trials from the same model. Accuracy need not improve at every step.

= A randomized experiment: define one assignment
<a-randomized-experiment-define-one-assignment>
Three participants are each assigned treatment by an independent fair coin.

#block[
#Skylighting(([#FunctionTok("set.seed");#NormalTok("(");#DecValTok("212");#NormalTok(")");],
[#NormalTok("assignment ");#OtherTok("<-");#NormalTok(" ");#FunctionTok("sample");#NormalTok("(");#FunctionTok("c");#NormalTok("(");#StringTok("\"C\"");#NormalTok(", ");#StringTok("\"T\"");#NormalTok("), ");#DecValTok("3");#NormalTok(", ");#AttributeTok("replace =");#NormalTok(" ");#ConstantTok("TRUE");#NormalTok(")");],
[#NormalTok("assignment");],));
#block[
#Skylighting(([#NormalTok("[1] \"T\" \"T\" \"C\"");],));
]
#Skylighting(([#FunctionTok("any");#NormalTok("(assignment ");#SpecialCharTok("==");#NormalTok(" ");#StringTok("\"T\"");#NormalTok(") ");#SpecialCharTok("&");#NormalTok(" ");#FunctionTok("any");#NormalTok("(assignment ");#SpecialCharTok("==");#NormalTok(" ");#StringTok("\"C\"");#NormalTok(")");],));
#block[
#Skylighting(([#NormalTok("[1] TRUE");],));
]
]
Our event: #strong[both treatment and control are represented].

This is a question about the assignment procedure, before observing any outcomes.

= The experiment's sample space
<the-experiments-sample-space>
$ S = { C C C \, C C T \, C T C \, C T T \, T C C \, T C T \, T T C \, T T T } . $

With independent fair assignments, each pattern has probability $1 \/ 8$.

Six patterns include both groups:

$ P \( upright("both groups represented") \) = 6 / 8 = 3 / 4 . $

#strong[Design question:] Does this procedure guarantee a usable comparison group?

= replicate() repeats the whole trial
<replicate-repeats-the-whole-trial>
#block[
#Skylighting(([#FunctionTok("set.seed");#NormalTok("(");#DecValTok("213");#NormalTok(")");],
[#NormalTok("mixed ");#OtherTok("<-");#NormalTok(" ");#FunctionTok("replicate");#NormalTok("(");#DecValTok("20000");#NormalTok(", {");],
[#NormalTok("  a ");#OtherTok("<-");#NormalTok(" ");#FunctionTok("sample");#NormalTok("(");#FunctionTok("c");#NormalTok("(");#StringTok("\"C\"");#NormalTok(", ");#StringTok("\"T\"");#NormalTok("), ");#DecValTok("3");#NormalTok(", ");#AttributeTok("replace =");#NormalTok(" ");#ConstantTok("TRUE");#NormalTok(")");],
[#NormalTok("  ");#FunctionTok("any");#NormalTok("(a ");#SpecialCharTok("==");#NormalTok(" ");#StringTok("\"T\"");#NormalTok(") ");#SpecialCharTok("&");#NormalTok(" ");#FunctionTok("any");#NormalTok("(a ");#SpecialCharTok("==");#NormalTok(" ");#StringTok("\"C\"");#NormalTok(")");],
[#NormalTok("})");],
[#FunctionTok("mean");#NormalTok("(mixed)");],));
#block[
#Skylighting(([#NormalTok("[1] 0.74855");],));
]
]
The last expression inside #NormalTok("{ ... }"); is the result saved from each trial.

Put #NormalTok("set.seed()"); #strong[before] #NormalTok("replicate()");, not inside it.

= A different design has a different sample space
<a-different-design-has-a-different-sample-space>
Suppose we require exactly one treated participant:

#block[
#Skylighting(([#FunctionTok("sample");#NormalTok("(");#FunctionTok("c");#NormalTok("(");#StringTok("\"T\"");#NormalTok(", ");#StringTok("\"C\"");#NormalTok(", ");#StringTok("\"C\"");#NormalTok("), ");#AttributeTok("size =");#NormalTok(" ");#DecValTok("3");#NormalTok(", ");#AttributeTok("replace =");#NormalTok(" ");#ConstantTok("FALSE");#NormalTok(")");],));
#block[
#Skylighting(([#NormalTok("[1] \"C\" \"C\" \"T\"");],));
]
]
Now $S = { T C C \, C T C \, C C T }$. Each pattern has probability $1 \/ 3$.

Both groups are represented with probability 1.

We changed the assignment procedure---not just the R syntax.

= A hypothetical polling model
<a-hypothetical-polling-model>
Assume each sampled response is an independent approval indicator with $P \( upright("approve") \) = 0.60$.

#block[
#Skylighting(([#FunctionTok("set.seed");#NormalTok("(");#DecValTok("214");#NormalTok(")");],
[#NormalTok("responses ");#OtherTok("<-");#NormalTok(" ");#FunctionTok("sample");#NormalTok("(");#FunctionTok("c");#NormalTok("(");#DecValTok("0");#NormalTok(", ");#DecValTok("1");#NormalTok("), ");#DecValTok("20");#NormalTok(", ");#AttributeTok("replace =");#NormalTok(" ");#ConstantTok("TRUE");#NormalTok(",");],
[#NormalTok("                    ");#AttributeTok("prob =");#NormalTok(" ");#FunctionTok("c");#NormalTok("(");#FloatTok("0.40");#NormalTok(", ");#FloatTok("0.60");#NormalTok("))");],
[#FunctionTok("mean");#NormalTok("(responses)");],));
#block[
#Skylighting(([#NormalTok("[1] 0.5");],));
]
]
#NormalTok("1"); means approve. The sample mean is the approval share.

For a large population, this is a simplified model of repeated sampling.

= Weighted outcomes: calculate and simulate
<weighted-outcomes-calculate-and-simulate>
For two independent responses with approval probability 0.60, the ordered outcomes are #strong[not] equally likely:

#table(
  columns: 2,
  align: (auto,right,),
  table.header([Outcome], [Probability under independent sampling],),
  table.hline(),
  [Neither approves: $00$], [$0.40 times 0.40 = 0.16$],
  [First only: $10$], [$0.60 times 0.40 = 0.24$],
  [Second only: $01$], [$0.40 times 0.60 = 0.24$],
  [Both approve: $11$], [$0.60 times 0.60 = 0.36$],
)
Agreement is ${ 00 \, 11 }$, so $P \( upright("agree") \) = 0.16 + 0.36 = 0.52$. Multiply within an independent pair; add across disjoint matching outcomes.

= Repeat the poll, not just one response
<repeat-the-poll-not-just-one-response>
What is the chance that a poll of 100 reports #strong[less than 50% approval]?

#block[
#Skylighting(([#FunctionTok("set.seed");#NormalTok("(");#DecValTok("215");#NormalTok(")");],
[#NormalTok("poll_share ");#OtherTok("<-");#NormalTok(" ");#FunctionTok("replicate");#NormalTok("(");#DecValTok("10000");#NormalTok(", {");],
[#NormalTok("  x ");#OtherTok("<-");#NormalTok(" ");#FunctionTok("sample");#NormalTok("(");#FunctionTok("c");#NormalTok("(");#DecValTok("0");#NormalTok(", ");#DecValTok("1");#NormalTok("), ");#DecValTok("100");#NormalTok(", ");#AttributeTok("replace =");#NormalTok(" ");#ConstantTok("TRUE");#NormalTok(",");],
[#NormalTok("              ");#AttributeTok("prob =");#NormalTok(" ");#FunctionTok("c");#NormalTok("(");#FloatTok("0.40");#NormalTok(", ");#FloatTok("0.60");#NormalTok("))");],
[#NormalTok("  ");#FunctionTok("mean");#NormalTok("(x)");],
[#NormalTok("})");],
[#FunctionTok("mean");#NormalTok("(poll_share ");#SpecialCharTok("<");#NormalTok(" ");#FloatTok("0.50");#NormalTok(")");],));
#block[
#Skylighting(([#NormalTok("[1] 0.0168");],));
]
]
There are 100 responses per trial and 10,000 trials.

= Different polls, one assumed population
<different-polls-one-assumed-population>
#box(image("probability-slides_files/figure-typst/unnamed-chunk-12-1.png", alt: "Simulated approval shares cluster around 0.60, with a small lower tail below 0.50. The dashed line is 0.50; the solid line is the assumed 0.60 approval rate.", width: 86%))

Dashed line: 0.50; solid line: the assumed 0.60 approval rate. This captures sampling variability under our assumptions. It does not fix nonresponse, question wording, or an unrepresentative sampling process.

= Counting without listing every outcome
<counting-without-listing-every-outcome>
#strong[Product rule:] If one choice has $m$ options and the next has $n$ options for each first choice, there are $m n$ ordered possibilities.

- Two dice: $6 times 6 = 36$ ordered outcomes.
- Twenty yes/no responses: $2^20 = 1 \, 048 \, 576$ response sequences.
- Three distinct names chosen in order from six: $6 times 5 times 4 = 120$.

Counting outcomes does not, by itself, tell us they are equally likely. The polling sequences are not equally likely when approval probability is 0.60.

= Order and replacement determine the count
<order-and-replacement-determine-the-count>
#table(
  columns: 2,
  align: (auto,right,),
  table.header([Procedure], [Number of possibilities],),
  table.hline(),
  [Ordered, $k$ draws from $n$, with replacement], [$n^k$],
  [Ordered, $k$ distinct choices from $n$], [$n ! \/ \( n - k \) !$],
  [Unordered, $k$ distinct choices from $n$], [$binom(n, k) = n ! \/ \[ k ! \( n - k \) ! \]$],
)
For choices without replacement, $0 lt.eq k lt.eq n$\; also $0 ! = 1$.

Use #NormalTok("factorial(n)"); and #NormalTok("choose(n, k)"); in R.

#strong[Check:] Are a committee and a ranked list the same kind of outcome?

= A committee example
<a-committee-example>
Choose a two-person committee uniformly from six legislators: four incumbents and two newcomers.

$ P \( upright("both incumbents") \) = binom(4, 2) / binom(6, 2) = 6 / 15 = 0.40 . $

#block[
#Skylighting(([#FunctionTok("choose");#NormalTok("(");#DecValTok("4");#NormalTok(", ");#DecValTok("2");#NormalTok(") ");#SpecialCharTok("/");#NormalTok(" ");#FunctionTok("choose");#NormalTok("(");#DecValTok("6");#NormalTok(", ");#DecValTok("2");#NormalTok(")");],));
#block[
#Skylighting(([#NormalTok("[1] 0.4");],));
]
]
The committee is unordered. Every pair of distinct legislators is equally likely.

= Simulate the same committee procedure
<simulate-the-same-committee-procedure>
#block[
#Skylighting(([#FunctionTok("set.seed");#NormalTok("(");#DecValTok("216");#NormalTok(")");],
[#NormalTok("both_incumbents ");#OtherTok("<-");#NormalTok(" ");#FunctionTok("replicate");#NormalTok("(");#DecValTok("20000");#NormalTok(", {");],
[#NormalTok("  committee ");#OtherTok("<-");#NormalTok(" ");#FunctionTok("sample");#NormalTok("(");#DecValTok("1");#SpecialCharTok(":");#DecValTok("6");#NormalTok(", ");#DecValTok("2");#NormalTok(", ");#AttributeTok("replace =");#NormalTok(" ");#ConstantTok("FALSE");#NormalTok(")");],
[#NormalTok("  ");#FunctionTok("all");#NormalTok("(committee ");#SpecialCharTok("<=");#NormalTok(" ");#DecValTok("4");#NormalTok(")  ");#CommentTok("# legislators 1–4 are incumbents");],
[#NormalTok("})");],
[#FunctionTok("mean");#NormalTok("(both_incumbents)");],));
#block[
#Skylighting(([#NormalTok("[1] 0.3978");],));
]
]
#strong[Your turn:] Find the probability of exactly one newcomer by counting. Then change the final expression to simulate that event.

= Before trusting a simulation
<before-trusting-a-simulation>
- Does one trial represent the intended process?
- Did we choose the right replacement rule and weights?
- Is the event coded correctly, including equality and boundary cases?
- Are all repetitions generated afresh?
- Is the answer plausible, and does it agree with an exact calculation when available?

Running an incorrect model more times produces a more stable wrong answer.

= A2: calculation and simulation
<a2-calculation-and-simulation>
#strong[Due:] Wednesday, September 16, at 1:30 p.m.

PSD exercises #strong[2.1, 2.3, 2.4, 2.7, 2.9, 2.33, and 2.36].

- Read each linked exercise; write explanations in the Quarto report.
- Save code in #NormalTok("R/a2_answers.R");\; run the visible checks locally.
- Render, commit, push, and check GitHub Actions.

Next week: conditional probability, independence, and Bayes' rule; Blitzstein and Hwang, Chapter 2.

= Sources and scope
<sources-and-scope>
Speegle and Clair, #link("https://probstatsdata.com/probchapter.html")[#emph[Probability, Statistics, and Data], Chapter 2], especially §§2.1, 2.2, and 2.4.

The voting example adapts prior POLI 210 probability materials. Polling and three-person random-assignment examples build on themes in Matthew Blackwell's #emph[Gov 2000: Random Variables] (Fall 2016), retained in the earlier course archive.

The simulations and numerical polling assumptions here are instructional examples. Formal random-variable distributions and inference come later in the course.
