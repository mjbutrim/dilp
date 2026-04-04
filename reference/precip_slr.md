# Estimate precipitation with simple linear regression

`precip_slr()` will produce estimates of mean annual precipitation and
standard error using leaf area analysis.

## Usage

``` r
precip_slr(
  data,
  regression = "Peppe2018",
  slope = NULL,
  constant = NULL,
  error = NULL
)
```

## Arguments

- data:

  A data frame that must include the columns "morphotype", "leaf_area",
  and "specimen_number". Must be species level data.

- regression:

  A string representing one of the following pre-loaded regressions:

  - "Peppe2018" - for global precipitation estimates

  - "Peppe2011" - The Americas, Japan, and Oceania

  - "Jacobs2002" - Africa

  - "Wilf1998" - The Americas and Africa

- slope:

  Slope, if using a custom regression

- constant:

  Constant, if using a custom regression

- error:

  Standard error, if using a custom regression

## Value

A table with MAP estimates for each site

## References

- Peppe, D. J., Baumgartner, A., Flynn, A., & Blonder, B. (2018).
  Reconstructing paleoclimate and paleoecology using fossil leaves.
  Methods in paleoecology: Reconstructing Cenozoic terrestrial
  environments and ecological communities, 289-317.

- Peppe, D.J., Royer, D.L., Cariglino, B., Oliver, S.Y., Newman, S.,
  Leight, E., Enikolopov, G., Fernandez-Burgos, M., Herrera, F., Adams,
  J.M., Correa, E., Currano, E.D., Erickson, J.M., Hinojosa, L.F.,
  Hoganson, J.W., Iglesias, A., Jaramillo, C.A., Johnson, K.R., Jordan,
  G.J., Kraft, N.J.B., Lovelock, E.C., Lusk, C.H., Niinemets, Ü.,
  Peñuelas, J., Rapson, G., Wing, S.L. and Wright, I.J. (2011),
  Sensitivity of leaf size and shape to climate: global patterns and
  paleoclimatic applications. New Phytologist, 190: 724-739.
  https://doi.org/10.1111/j.1469-8137.2010.03615.x

- Jacobs, B. F. (2002). Estimation of low-latitude paleoclimates using
  fossil angiosperm leaves: examples from the Miocene Tugen Hills,
  Kenya. Paleobiology, 28, 399–421.

- Wilf, P. (2008). Fossil angiosperm leaves: paleobotany’s difficult
  children prove themselves. Paleontological Society Papers, 14,
  319–333.

## Examples

``` r
precip_slr(McAbeeExample, regression = "Peppe2011")
#>        site  n    lower      MAP    upper
#> 1 McAbee H1 17 57.88926 126.7697 360.0805
#> 2 McAbee H2 13 62.04646 135.8734 385.9390
```
