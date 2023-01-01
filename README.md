[![R-CMD-check](https://github.com/hsonne/wordcards/workflows/R-CMD-check/badge.svg)](https://github.com/hsonne/wordcards/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/hsonne/wordcards/workflows/pkgdown/badge.svg)](https://github.com/hsonne/wordcards/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/hsonne/wordcards/branch/main/graphs/badge.svg)](https://codecov.io/github/hsonne/wordcards)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/wordcards)]()
[![R-Universe_Status_Badge](https://hsonne.r-universe.dev/badges/wordcards)](https://hsonne.r-universe.dev/)

# wordcards

This package provides functions to analyse the frequency of
words or syllables in a given text. It provides a function to create a
PDF file with all words or syllables printed on cards, together with
their frequencies. The idea is to use these cards to help children
learn to read.

## Installation

For details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

# Install KWB package 'wordcards' from GitHub
remotes::install_github("hsonne/wordcards")
```

## Documentation

Release: [https://hsonne.github.io/wordcards](https://hsonne.github.io/wordcards)

Development: [https://hsonne.github.io/wordcards/dev](https://hsonne.github.io/wordcards/dev)
