# Check for common errors in DiLP measurements

`dilp_errors()` will typically only be called internally by
[`dilp()`](https://mjbutrim.github.io/dilp/reference/dilp.md). However,
it can be used on its own to evaluate errors that commonly occur during
the data collection and processing steps. A `dilp_errors()` call will
nearly always follow a
[`dilp_processing()`](https://mjbutrim.github.io/dilp/reference/dilp_processing.md)
call. Returns a data frame.

## Usage

``` r
dilp_errors(specimen_data)
```

## Arguments

- specimen_data:

  Processed specimen level leaf physiognomic data. The structure should
  match the structure of the output from
  [`dilp_processing()`](https://mjbutrim.github.io/dilp/reference/dilp_processing.md)

## Value

A 7 by X data frame. Each row shows a common error, and which specimens
from the input dataset are tripping it.

## Examples

``` r
# Check for errors in the provided McAbeeExample dataset.
dilp_dataset <- dilp_processing(McAbeeExample)
dilp_errors <- dilp_errors(dilp_dataset)
dilp_errors
#>                                                   Check Specimen1
#> 1                             Entire tooth count not NA      none
#> 2                        Entire tooth count : IP not NA      none
#> 3                         Entire perimeter ratio not NA      none
#> 4                                   FDR not between 0-1      none
#> 5 External perimeter not larger than internal perimeter      none
#> 6                Feret is not larger than minimum Feret      none
#> 7                    Perimeter ratio not greater than 1      none
```
