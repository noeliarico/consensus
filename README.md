# consensus <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/noeliarico/consensus.svg?branch=master)](https://travis-ci.org/noeliarico/consensus)
<!-- badges: end -->

Welcome to the `consensus` package! 

The goal of consensus is to serve as a tool for managing rankings and profile of rankings and applying ranking rules.

Check the full doc in https://noeliarico.github.io/consensus/

## Installation

You can install the released version of consensus from [CRAN](https://CRAN.R-project.org) with:

``` r
devtools::install_github("noeliarico/consensus")
```

## Example

This is a basic example which shows you how to create rankings:

``` r
library(consensus)
# To create a ranking
r <- ranking(c(4, 1, 2))
C2 ≻ C3 ≻ C1 
# To give names to the ranking
r <- ranking(c(4, 1, 2), cnames = letters[1:3])
b ≻ c ≻ a 
```

