# Estimate temperature with simple linear regression

`temp_slr()` will produce estimates of mean annual temperature and
standard error using leaf margin analysis.

## Usage

``` r
temp_slr(
  data,
  regression = "Peppe2018",
  slope = NULL,
  constant = NULL,
  error = NULL
)
```

## Arguments

- data:

  A data frame that must include the columns "morphotype" and "margin".
  Can be species or site level data.

- regression:

  A string representing one of the following pre-loaded regressions:

  - "Peppe2018" - for global temperature estimates

  - "Peppe2011" - The Americas, Japan, and Oceania

  - "Peppe2011NH" - Peppe 2011 (Northern Hemisphere only)

  - "Miller2006" - North and Central America

  - "WingGreenwood" - East Asia - original leaf margin analysis
    regression

  - "Wilf1997" - The Americas

- slope:

  Slope, if using a custom regression

- constant:

  Constant, if using a custom regression

- error:

  Standard error, if using a custom regression

## Value

A table with MAT estimates for each site

## References

- Miller, I. M., Brandon, M. T., & Hickey, L. J. (2006). Using leaf
  margin analysis to estimate mid-Cretaceous (Albian) paleolatitude of
  the Baja BC block. Earth and Planetary Science Letters, 245, 95–114.

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

- Wing, S., & Greenwood, D. R. (1993). Fossils and fossil climate: the
  case for equable continental interiors in the Eocene. Philosophical
  Transactions of the Royal Society of London Series B, 341, 243–252.

- Wilf, P. (1997). When are leaves good thermometers? A new case for
  leaf margin analysis. Paleobiology, 23, 373–390.

## Examples

``` r
temp_slr(McAbeeExample, regression = "Peppe2011")
#>        site  n    lower      MAT    upper
#> 1 McAbee H1 31 6.180645 11.18065 16.18065
#> 2 McAbee H2 30 4.360000  9.36000 14.36000
```
