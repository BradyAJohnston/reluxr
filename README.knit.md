---
output: github_document
bibliography: references.bib
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# reluxr

<!-- badges: start -->

[![R-CMD-check](https://github.com/BradyAJohnston/reluxr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/BradyAJohnston/reluxr/actions/workflows/R-CMD-check.yaml) [![CRAN status](https://www.r-pkg.org/badges/version/reluxr)](https://CRAN.R-project.org/package=reluxr) [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

The goal of reluxr is to enable deconvolution of luminescencent plate-based experiments. The implementation is based on the MatLab implementation from the paper titled '**Deconvolution of Luminescence Cross-Talk in High-Throughput Gene Expression Profiling'** [@mauri2019]

## Installation

You can install the development version of reluxr from [GitHub](https://github.com/) with:

``` r
if (!require(devtools)) install.packages("devtools")


devtools::install_github("BradyAJohnston/wellr")
devtools::install_github("BradyAJohnston/reluxr")
```

## Example

This is a basic example which shows you how to solve a common problem:




