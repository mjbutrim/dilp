# Plot climate reconstructions on a Whittaker Biome plot

`dilp_whittaker()` plots
[`dilp()`](https://mjbutrim.github.io/dilp/reference/dilp.md) outputs
onto a Whittaker Biome plot. Base Whittaker Plot from the
[plotbiomes](https://github.com/valentinitnelav/plotbiomes) package by
Ștefan Valentin and Sam Levin.

## Usage

``` r
dilp_whittaker(climate_data)
```

## Arguments

- climate_data:

  A data frame containing either the direct output of a
  [`dilp()`](https://mjbutrim.github.io/dilp/reference/dilp.md) call, or
  the \$results tab from that output. Can also be a data frame with the
  following required columns:

  - site

  - MAT.MLR

  - MAT.MLR.error

  - MAP.MLR

  - MAP.MLR.error.minus

  - MAP.MLR.error.plus

## Value

A modifiable ggplot with dilp climate-reconstructed sites plotted onto a
Whittaker diagram.

## References

Valentin Ștefan, & Sam Levin. (2018). plotbiomes: R package for plotting
Whittaker biomes with ggplot2 (v1.0.0). Zenodo.
https://doi.org/10.5281/zenodo.7145245

## Examples

``` r
results <- dilp(McAbeeExample)
#> Warning: Outliers found. Please evaluate $outliers for possible wrong measurements
dilp_whittaker(results)

```
