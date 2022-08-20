
<!-- README.md is generated from README.Rmd. Please edit that file -->

# reluxr

<!-- badges: start -->

[![R-CMD-check](https://github.com/BradyAJohnston/reluxr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/BradyAJohnston/reluxr/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/reluxr)](https://CRAN.R-project.org/package=reluxr)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

The goal of reluxr is to enable deconvolution of luminescencent
plate-based experiments. The implementation is based on the MatLab
implementation from the paper titled ’**Deconvolution of Luminescence
Cross-Talk in High-Throughput Gene Expression Profiling’** (Mauri,
Vecchione, and Fritz 2019)

## Installation

You can install the development version of reluxr from
[GitHub](https://github.com/) with:

``` r
if (!require(devtools)) install.packages("devtools")


devtools::install_github("BradyAJohnston/wellr")
devtools::install_github("BradyAJohnston/reluxr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(reluxr)
library(wellr)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

fl <- system.file(
   "extdata",
   "calibrate_tecan",
   "calTecan1.xlsx",
   package = "reluxr"
 )


dat <- readxl::read_excel(fl, skip = 43, col_names = FALSE)
#> New names:
#> • `` -> `...1`
#> • `` -> `...2`
#> • `` -> `...3`
#> • `` -> `...4`
#> • `` -> `...5`
#> • `` -> `...6`
#> • `` -> `...7`
#> • `` -> `...8`
#> • `` -> `...9`
#> • `` -> `...10`
#> • `` -> `...11`
#> • `` -> `...12`
#> • `` -> `...13`
#> • `` -> `...14`
#> • `` -> `...15`
#> • `` -> `...16`
#> • `` -> `...17`
#> • `` -> `...18`
#> • `` -> `...19`
#> • `` -> `...20`
#> • `` -> `...21`
#> • `` -> `...22`
#> • `` -> `...23`
#> • `` -> `...24`
#> • `` -> `...25`
#> • `` -> `...26`
#> • `` -> `...27`
#> • `` -> `...28`
#> • `` -> `...29`
#> • `` -> `...30`
#> • `` -> `...31`
#> • `` -> `...32`
#> • `` -> `...33`
#> • `` -> `...34`
#> • `` -> `...35`
#> • `` -> `...36`
#> • `` -> `...37`
#> • `` -> `...38`
#> • `` -> `...39`
#> • `` -> `...40`
#> • `` -> `...41`
#> • `` -> `...42`
#> • `` -> `...43`
#> • `` -> `...44`
#> • `` -> `...45`
#> • `` -> `...46`
#> • `` -> `...47`
#> • `` -> `...48`
#> • `` -> `...49`
#> • `` -> `...50`
#> • `` -> `...51`
#> • `` -> `...52`
#> • `` -> `...53`
#> • `` -> `...54`
#> • `` -> `...55`
#> • `` -> `...56`
#> • `` -> `...57`
#> • `` -> `...58`
#> • `` -> `...59`
#> • `` -> `...60`
#> • `` -> `...61`
#> • `` -> `...62`
#> • `` -> `...63`
#> • `` -> `...64`
#> • `` -> `...65`
#> • `` -> `...66`
#> • `` -> `...67`
#> • `` -> `...68`
#> • `` -> `...69`
#> • `` -> `...70`
#> • `` -> `...71`
#> • `` -> `...72`
#> • `` -> `...73`
#> • `` -> `...74`
#> • `` -> `...75`
#> • `` -> `...76`
#> • `` -> `...77`
#> • `` -> `...78`
#> • `` -> `...79`
#> • `` -> `...80`
#> • `` -> `...81`
#> • `` -> `...82`
#> • `` -> `...83`
#> • `` -> `...84`
#> • `` -> `...85`
#> • `` -> `...86`
#> • `` -> `...87`
#> • `` -> `...88`
#> • `` -> `...89`
#> • `` -> `...90`
#> • `` -> `...91`
#> • `` -> `...92`
#> • `` -> `...93`
#> • `` -> `...94`
#> • `` -> `...95`
#> • `` -> `...96`
#> • `` -> `...97`
#> • `` -> `...98`
#> • `` -> `...99`

dat |> 
  dplyr::mutate(
    group = cumsum(is.na(...2))
  ) |> 
  dplyr::group_by(group) |> 
  dplyr::mutate(
    ...1 = ...1[1]
  ) |> 
  dplyr::group_by(...1) |> 
  tidyr::nest() |> 
  dplyr::filter(!is.na(...1)) |> 
  dplyr::mutate(
    data = purrr::map(data, ~.x[-1,]), 
    data = purrr::map(data, janitor::row_to_names, row_number = 1), 
    data = purrr::map(data, dplyr::mutate, dplyr::across(-c(1:2), as.numeric)),
    data = purrr::map(data, tidyr::pivot_longer, 
                      cols = -c(1:2), 
                      names_to = "well", 
                      names_transform = wellr::well_format
                      )
  ) |> 
  tidyr::unnest(data) |> 
  janitor::clean_names()
#> # A tibble: 23,280 × 5
#> # Groups:   x1 [2]
#>    x1        time_s temp_c well   value
#>    <chr>     <chr>  <chr>  <chr>  <dbl>
#>  1 OD600:600 0      37.1   A01   0.0450
#>  2 OD600:600 0      37.1   A02   0.0452
#>  3 OD600:600 0      37.1   A03   0.0453
#>  4 OD600:600 0      37.1   A04   0.0453
#>  5 OD600:600 0      37.1   A05   0.0453
#>  6 OD600:600 0      37.1   A06   0.0452
#>  7 OD600:600 0      37.1   A07   0.0458
#>  8 OD600:600 0      37.1   A08   0.0456
#>  9 OD600:600 0      37.1   A09   0.0455
#> 10 OD600:600 0      37.1   A10   0.0451
#> # … with 23,270 more rows
#> # ℹ Use `print(n = ...)` to see more rows
```

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-mauri2019" class="csl-entry">

Mauri, Marco, Stefano Vecchione, and Georg Fritz. 2019. “Deconvolution
of Luminescence Cross-Talk in High-Throughput Gene Expression
Profiling.” *ACS Synthetic Biology* 8 (6): 1361–70.
<https://doi.org/10.1021/acssynbio.9b00032>.

</div>

</div>
