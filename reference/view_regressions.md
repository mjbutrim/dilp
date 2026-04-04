# View preloaded regressions

View preloaded regressions

## Usage

``` r
view_regressions(type)
```

## Arguments

- type:

  Must be either "dilp", "lma", temp", or "precip".

## Value

A data frame containing the parameters for each available regression of
the selected type.

## Examples

``` r
view_regressions("dilp")
#>          Name MAT.MLR.M MAT.MLR.FDR MAT.MLR.TC.IP MAT.MLR.constant
#> 1 PeppeGlobal     0.210      42.296        -2.609          -16.004
#> 2     PeppeNH     0.233       0.000        -1.547            8.161
#>   MAT.MLR.error MAT.SLR.M MAT.SLR.constant MAT.SLR.error MAP.MLR.LA
#> 1             4     0.204            4.600           4.8      0.298
#> 2             4     0.262            3.167           2.0         NA
#>   MAP.MLR.TC.IP MAP.MLR.PR MAP.MLR.constant MAP.MLR.SE MAP.SLR.LA
#> 1         0.279     -2.717            3.033        0.6      0.283
#> 2            NA         NA               NA         NA         NA
#>   MAP.SLR.constant MAP.SLR.SE
#> 1             2.92       0.61
#> 2               NA         NA
```
