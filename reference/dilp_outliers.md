# Identify outlier specimens

`dilp_outliers()` will typically only be called internally by
[`dilp()`](https://mjbutrim.github.io/dilp/reference/dilp.md). However,
it can be used on its own to locate specimens that may have been
misreported or measured incorrectly. `dilp_outliers()` returns a data
frame listing specimens that have unusually high or low values for the
four key parameters used in DiLP analyses. If flagged, it may be worth
taking a look at the raw measurements and evaluating if the specimen
should be used.

## Usage

``` r
dilp_outliers(specimen_data)
```

## Arguments

- specimen_data:

  Processed specimen level leaf physiognomic data. The structure should
  match the structure of the output from
  [`dilp_processing()`](https://mjbutrim.github.io/dilp/reference/dilp_processing.md)

## Value

A 4 by X data frame. Each row represents one of the DiLP parameters, and
the specimens that are outliers for that parameter.

## Examples

``` r
# Check for outliers in the provided McAbeeExample dataset. Each
# of these outliers has been manually re-examined and was found acceptable.
dilp_dataset <- dilp_processing(McAbeeExample)
dilp_outliers <- dilp_outliers(dilp_dataset)
dilp_outliers
#>          Variable     Outlier1     Outlier2     Outlier3    Outlier4
#> 1             fdr         <NA>         <NA>         <NA>        <NA>
#> 2           tc_ip  BU-712-1117 BU-712-1169A BU-712-1176A        <NA>
#> 3       leaf_area BU-712-2173A BU-712-2105A  BU-712-2124        <NA>
#> 4 perimeter_ratio   M-2015-1-1 BU-712-1073A  BU-712-1165 M-2015-1-62
```
