
<!-- README.md is generated from README.Rmd. Please edit that file -->

# reluxr

<!-- badges: start -->

[![R-CMD-check](https://github.com/BradyAJohnston/reluxr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/BradyAJohnston/reluxr/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/reluxr)](https://CRAN.R-project.org/package=reluxr)
[![reluxr status
badge](https://bradyajohnston.r-universe.dev/badges/reluxr)](https://bradyajohnston.r-universe.dev)
[![:name status
badge](https://bradyajohnston.r-universe.dev/badges/:name)](https://bradyajohnston.r-universe.dev)

<!-- badges: end -->

Implements the deconvolution algorithm developed in Mauri, Vecchione,
and Fritz (2019) which enables deconvolution of luminscence readings for
experimental culture plates. {reluxr} provides functions for calculating
the ‘best’ deconvolution matrix from a calibration plate, and enables
usage of this calibration matrix (or one calculated previously) to
adjust luminescent experimental values from a plate reader. This is an
R-based implementation of the MatLab workflow from the original paper
paper titled *1Deconvolution of Luminescence Cross-Talk in
High-Throughput Gene Expression Profiling’* (Mauri, Vecchione, and Fritz
2019)

## Installation

The package is not currently available on CRAN. Install the released
version from `r-universe` with the following code:

``` r
install.package("reluxr", repos = "https://bradyajohnston.r-universe.dev")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(reluxr)

fl <- system.file(
   "extdata",
   "calibrate_tecan",
   "calTecan1.xlsx",
   package = "reluxr"
 )
```

## Create a Deconvolution Matrix

The deconvolution matrix is created from a calibration plate, which
contains a single well with luminescent bacteria, with all other wells
being empty. The cross-talk when the plate-reader measures the wells can
then be calculated and used to create a deconvolution matrix which will
remove the crosstalk from the measured values.

``` r
dat <- plate_read_tecan(fl)

dat
#> # A tibble: 23,040 × 5
#>    cycle_nr time_s signal well   value
#>       <dbl>  <dbl> <chr>  <chr>  <dbl>
#>  1        1      0 OD600  A01   0.0450
#>  2        1      0 OD600  A02   0.0452
#>  3        1      0 OD600  A03   0.0453
#>  4        1      0 OD600  A04   0.0453
#>  5        1      0 OD600  A05   0.0453
#>  6        1      0 OD600  A06   0.0452
#>  7        1      0 OD600  A07   0.0458
#>  8        1      0 OD600  A08   0.0456
#>  9        1      0 OD600  A09   0.0455
#> 10        1      0 OD600  A10   0.0451
#> # … with 23,030 more rows
```

Regardless of how you read in the required data, it needs to be (and
should be regardless) in a *tidy* format, with each row being an
observation and each column a variable. In the case above we have
columns for the `cycle_nr`, `time_s`, `signal`, `well` and `value`.
While it would be better to have a column for the OD600 and for the LUMI
data, the time points do not match and aren’t currently pivotable.

To have a look at the final data, we can plot the plate based on
log-transformed luminescence values. We can see the very bright single
well than contains the bacteria, and the bleed-through signal that is
around it.

``` r
dat_fil <- dat |> 
  dplyr::filter(signal == "LUMI", time_s > 500)


dat_fil |> 
  dplyr::group_by(well) |> 
  dplyr::summarise(value = mean(value)) |> 
  rl_plot_plate(value)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

We can use the `rl_calc_decon_matrix()` function which will calculate a
deconvolution matrix, reducing the background bleed-through from the
plate to below a noise threshold. The noise threshold should be the
instrument’s background noise, which is defined as three times the
standard deviation of a blank well. The lower the noise threshold, the
harder it will be to calculate a deconvolution matrix which works with
the data.

We can also quickly look at the resulting deconvolution matrix
(`mat_d_best`) itself.

``` r
mat_d_best <- rl_calc_decon_matrix(
  data = dat_fil,
  value = value,
  time = time_s,
  ref_well = "E05",
  b_noise = 20
)

image(log10(mat_d_best))
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

## Deconvoluting the Data

Now that we have the matrix, we can use it to adjust the data.

We do so using the `rl_adjust_plate()` funciton, which takes a
dataframe, the name of the column you which to adjust, the deconvolution
matrix (in this case `mat_d_best`, and the name ofthe column which
stores the time data).

The returned dataframe will have the value column adjusted and
deconvoluted using the deconvolution matrix supplied.

In the examples below we first plot all of the values without
deconvolution, then apply the deconvolution matrix and plot the values
again.

``` r
rl_plot_time <- function(data, time, value, group = "well") {
  
  data <- dplyr::mutate(
    data, 
    time = {{ time }}, 
    value = {{ value }}, 
    group = {{ group }}
  )
  
  plt <- ggplot2::ggplot(
    data, 
    mapping = ggplot2::aes(
      x = time,
      y = value, 
      group = group
    )
  ) + 
    ggplot2::geom_line() + 
    ggplot2::scale_y_log10() + 
    ggplot2::theme_bw()
  
  plt
}
```

#### Raw Values

``` r
dat |>
  dplyr::filter(signal == "LUMI") |> 
  rl_plot_time(time_s, value, well) + 
  ggplot2::labs(
    x = "Time (s)", 
    y = "LUM"
  )
#> Warning in self$trans$transform(x): NaNs produced
#> Warning: Transformation introduced infinite values in continuous y-axis
#> Warning: Removed 25 row(s) containing missing values (geom_path).
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

#### Deconvoluted Values

``` r
dat |>
  dplyr::filter(signal == "LUMI") |> 
  
  rl_adjust_plate(value, mat_d_best, time = time_s) |> # deconvolute the values
  
  rl_plot_time(time_s, value, well) + 
  ggplot2::labs(
    x = "Time (s)", 
    y = "LUM"
  )
#> Warning in self$trans$transform(x): NaNs produced
#> Warning: Transformation introduced infinite values in continuous y-axis
#> Warning: Removed 657 row(s) containing missing values (geom_path).
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

We can also replot the plate from earlier, with the newly deconvoluted
values.

``` r

dat_fil |> 
  rl_adjust_plate(value, mat_d_best, time = time_s) |> # deconvolute the values
  dplyr::group_by(well) |> 
  dplyr::summarise(value = mean(value)) |> 
  rl_plot_plate(value) + 
  ggplot2::scale_fill_viridis_c(
    "log10(LUMI)",
    breaks = 1:5, 
    limits = c(1, NA)
  )
#> Scale for 'fill' is already present. Adding another scale for 'fill', which
#> will replace the existing scale.
#> Warning in FUN(X[[i]], ...): NaNs produced
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-mauri2019" class="csl-entry">

Mauri, Marco, Stefano Vecchione, and Georg Fritz. 2019. “Deconvolution
of Luminescence Cross-Talk in High-Throughput Gene Expression
Profiling.” *ACS Synthetic Biology* 8 (6): 1361–70.
<https://doi.org/10.1021/acssynbio.9b00032>.

</div>

</div>
