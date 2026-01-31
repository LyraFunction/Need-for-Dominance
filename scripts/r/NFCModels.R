library(DiagrammeR)
library(DiagrammeRsvg)
library(htmltools)

svg <- export_svg(grViz("
digraph LGM {

  graph [rankdir = LR, splines=line]

  # Global style
  node [fontsize = 12, labelfontname = 'Times New Roman', width = 1, height = 1, margin = .01]

  # Voting indicator
  {rank = min;
    x1 [label = 'NFC', shape = circle, style = filled, fillcolor = white];
  }

  {rank = same;
    eta1 [label = 'SDO', shape = circle, style = filled, fillcolor = white];
    eta2 [label = 'Political\nIdentity', shape = circle, style = filled, fillcolor = white];
  }

  # indicators (rank = max)
  {rank = max;
    x2  [label = 'NFC',  shape = circle, style = filled, fillcolor = white];
    x3  [label = 'Stability',  shape = circle, style = filled, fillcolor = white];
    x4  [label = 'Evaluation',  shape = circle, style = filled, fillcolor = white];
  }

  # Edges: Left indicator -> latent variables
  x1:e -> eta1:w [splines = line]
  x1:e -> eta2:w [splines = line]

  # Edges: Right indicators -> latent variables
  x1:e  -> x2:w  [splines = line]
  x1:e  -> x3:w  [splines = line]
  x1:e  -> x4:w [splines = line]
  eta1:e -> x2:w [splines = line]
  eta1:e -> x3:w [splines = line]
  eta1:e -> x4:w [splines = line]
  eta2:e -> x2:w [splines = line]
  eta2:e -> x3:w [splines = line]
  eta2:e -> x4:w [splines = line]
}
"))

html_print(HTML(svg))
