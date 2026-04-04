# Identify outlier specimens

`dilp_outliers()` is called internally by
[`dilp()`](https://mjbutrim.github.io/dilp/reference/dilp.md). However,
it can be used on its own to flag specimens that may have been reported,
measured, or prepared incorrectly. `dilp_outliers()` returns a data
frame listing specimens that have unusually high or low values for the
four key parameters used in DiLP analyses. This includes whether a
specimen is an outlier in the entire dataset, or among other specimens
in the same morphotype. If flagged, we suggest looking at the raw
measurements and prepped specimen and evaluating if the data is in error
or is correct. If in error, the specimen will need to be reprepared
and/or remeasured, and the updated datasheet re-read back into R.

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
#> Warning: Outliers found. Please evaluate $outliers for possible wrong measurements
dilp_outliers
#>         site specimen_number morphotype outlier.entire.dataset
#> 1  McAbee H1    BU-712-1073A        M28        perimeter_ratio
#> 2  McAbee H1     BU-712-1117         M8                  tc_ip
#> 3  McAbee H1     BU-712-1165        M28        perimeter_ratio
#> 4  McAbee H1    BU-712-1169A         M8                  tc_ip
#> 5  McAbee H1    BU-712-1176A         M8                  tc_ip
#> 6  McAbee H1    BU-712-1182A         M5            No outliers
#> 7  McAbee H1    BU-712-1182A         M5            No outliers
#> 8  McAbee H1      M-2015-1-1        M24        perimeter_ratio
#> 9  McAbee H1    M-2015-1-122         M5            No outliers
#> 10 McAbee H1     M-2015-1-17        M28            No outliers
#> 11 McAbee H1      M-2015-1-3         M5            No outliers
#> 12 McAbee H1      M-2015-1-3         M5            No outliers
#> 13 McAbee H1     M-2015-1-40         M5            No outliers
#> 14 McAbee H1     M-2015-1-62        M28        perimeter_ratio
#> 15 McAbee H1     M-2015-1-69         M8            No outliers
#> 16 McAbee H1      M-2015-1-7        M19            No outliers
#> 17 McAbee H2    BU-712-2105A        M47              leaf_area
#> 18 McAbee H2     BU-712-2124        M94              leaf_area
#> 19 McAbee H2    BU-712-2173A        M18              leaf_area
#> 20 McAbee H2     BU-712-2197        M19            No outliers
#> 21 McAbee H2     M-2015-2-15        M19            No outliers
#> 22 McAbee H2     M-2015-2-84        M19            No outliers
#>    outlier.morphotype
#> 1         No outliers
#> 2               tc_ip
#> 3         No outliers
#> 4               tc_ip
#> 5               tc_ip
#> 6               tc_ip
#> 7     perimeter_ratio
#> 8     perimeter_ratio
#> 9     perimeter_ratio
#> 10    perimeter_ratio
#> 11              tc_ip
#> 12    perimeter_ratio
#> 13    perimeter_ratio
#> 14    perimeter_ratio
#> 15    perimeter_ratio
#> 16              tc_ip
#> 17        No outliers
#> 18        No outliers
#> 19        No outliers
#> 20    perimeter_ratio
#> 21              tc_ip
#> 22              tc_ip
```
