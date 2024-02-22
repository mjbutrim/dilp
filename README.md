
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dilp

<!-- badges: start -->

[![R-CMD-check](https://github.com/mjbutrim/dilp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mjbutrim/dilp/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of dilp is to help with analysis of quantitative fossil leaf
traits. Key functions included are:

- Digital Leaf Physiognomy - `dilp()`

  - Estimate mean annual temperature and mean annual precipitation using
    multiple linear regressions.

- Fossil Leaf Mass per Area - `lma()`

  - Reconstruct leaf mass per area using leaf area and petiole width

- Leaf Margin Analysis - `temp_slr()`

  - Estimate mean annual temperature using leaf margin analysis

- Leaf Area Analysis - `precip_slr()`

  - Estimate mean annual precipitation using leaf area analysis

## Installation

You can install the development version of dilp from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mjbutrim/dilp")
```

## Example

Find a basic example of running a DiLP and LMA analysis [in this
vignette](https://mjbutrim.github.io/dilp/articles/dilp.html)
