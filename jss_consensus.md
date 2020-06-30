---
documentclass: jss
author:
  - name: Noelia Rico
    affiliation: Department of Computer Science, University of Oviedo
    address: >
      First line
      Second line
    email: \email{noeliarico@uniovi.es}
    url: http://rstudio.com
  
  - name: Raúl Pérez-Fernández
    affiliation: Department of Computer Science, University of Oviedo
    email: \email{perezfernandez@uniovi.es}
    
  - name: Irene Díaz
    affiliation: Department of Computer Science, University of Oviedo
    email: \email{sirene@uniovi.es}
title:
  formatted: "Aggregating Ranking Rules in R: The Package \\pkg{consensus}"
  # If you use tex in the formatted title, also supply version without
  plain:     "Aggregating Ranking Rules in R: The Package consensus"
  # For running headers, if needed
  short:     "\\pkg{consensus}: Ranking Rules Aggregation"
abstract: >
  The abstract of the article.
keywords:
  # at least one keyword must be supplied
  formatted: [aggregation, social choice theory, "\\proglang{R}"]
  plain:     [aggregation, social choice theory, R]
preamble: >
  \usepackage{amsmath}
output: 
  rticles::jss_article:
    latex_engine: xelatex
---




# Introduction

# Social choice theory

## Plurality

## Borda Count


# State of the art

Not a tool in R that covers everything

<!--
## \pkg{RankAggreg}: Weighted Rank Aggregation

## \pkg{votesys}: Voting Systems, Instant-Runoff Voting, Borda Method, Various, Condorcet Methods

## \pkg{ConsRank}: Compute the Median Ranking(s) According to the Kemeny's Axiomatic Approach

## \pkg{vote}: Election Vote Counting

## \pkg{SciencesPo}: A Tool Set for Analyzing Political Behavior Data

https://www.r-bloggers.com/condorcet-ranking-and-rcpp/

-->

# consensus

## Creating a ranking

The first step to use the package is to know how rankings and profile of rankings can be created. The function `ranking` create an object of the class `ranking` that is printed in the console in a user-friendly format.


```r
ranking(c(30, 18, 2, 4, 6))
```

C3 ≻ C4 ≻ C5 ≻ C2 ≻ C1 


## Code formatting

Don't use markdown, instead use the more precise latex commands:

* \proglang{Java}
* \pkg{plyr}
* \code{print("abc")}

# R code

Can be inserted in regular R markdown blocks.


```r
x <- 1:10
x
```

```
##  [1]  1  2  3  4  5  6  7  8  9 10
```
