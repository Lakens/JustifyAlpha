Justify
================
Daniel Lakens and Maximilian Maier
2021-06-08

# R Package accompanying “Justify Your Alpha: A Primer on Two Practical Approaches”

The goal of JustifyAlpha is to provide ways for researchers to justify
their alpha level when designing studies. Two approaches are currently
implemented. The first function *optimal\_alpha* allows users to
computed balanced or minimized Type 1 and Type 2 error rates. The second
approach uses the function *ttestEvidence* or *ftestEvidence* to lower
the alpha level as a function of the sample size to prevent Lindley’s
paradox.

## Installation

You can install the released version of JustifyAlpha from
[GitHub](https://github.com/Lakens/JustifyAlpha) with:

``` r
devtools::install_github("Lakens/JustifyAlpha")
```

# Preprint

A preprint explaining how to use this package and the Shiny app is
available from here:

# Vignette

A vignette explaining how to use this package and the Shiny app is
available from here:
<https://lakens.github.io/JustifyAlpha/articles/intro_to_justifieR.html>

# Shiny App

You can run the shiny app locally, but an online version is available
from <https://shiny.ieis.tue.nl/JustifyAlpha/> and
<https://maxma1er.shinyapps.io/JustifyAlpha/> and
