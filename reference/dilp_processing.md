# Process raw leaf physiognomic data

`dilp_processing()` will typically only be called internally by
[`dilp()`](https://mjbutrim.github.io/dilp/reference/dilp.md). However,
it can be used on its own to generate and view a processed DiLP dataset
that includes raw and derived physiognomic values useful for DiLP and
other physiognomic analyses. Returns a data frame.

## Usage

``` r
dilp_processing(specimen_data)
```

## Arguments

- specimen_data:

  A data frame containing specimen level leaf physiognomic data. A good
  reference for how to put together the data:
  [`McAbeeExample`](https://mjbutrim.github.io/dilp/reference/McAbeeExample.md)

## Value

A data frame containing cleaned and processed specimen level leaf
physiognomic data. New variables calculated are:

- Leaf area

- Feret diameter

- Feret diameter ratio (FDR)

- Raw blade perimeter corrected (Raw blade perimeter - length of cut
  perimeter)

- Internal raw blade perimeter corrected (Internal raw blade perimeter -
  length of cut perimeter)

- Total tooth count

- Total tooth count : internal perimeter (TC:IP)

- Perimeter ratio

- Petiole metric

- Aspect ratio

- Shape factor

- Compactness

- Tooth area

- Tooth area : perimeter (TA:P)

- Tooth area: internal perimeter (TA:IP)

- Tooth area : blade area (TA:BA)

- Average primary tooth area (Avg TA)

- Tooth count : blade area (TC:BA)

- Tooth count : perimeter (TC:P)

## Examples

``` r
dilp_dataset <- dilp_processing(McAbeeExample)
dilp_dataset
#> # A tibble: 192 × 40
#>    site      specimen_number morphotype measurer_comments margin petiole_width
#>    <chr>     <chr>           <chr>      <chr>              <dbl>         <dbl>
#>  1 McAbee H1 BU-712-1006     M1         NA                     0         0.165
#>  2 McAbee H1 BU-712-1022A    M1         NA                     0         0.047
#>  3 McAbee H1 BU-712-1065B    M1         NA                     0        NA    
#>  4 McAbee H1 BU-712-1083     M1         NA                     0         0.075
#>  5 McAbee H1 BU-712-1122     M1         NA                     0         0.074
#>  6 McAbee H1 BU-712-1133     M1         NA                     0         0.051
#>  7 McAbee H1 BU-712-1210     M1         NA                     0         0.138
#>  8 McAbee H1 BU-712-1214A    M1         NA                     0         0.142
#>  9 McAbee H1 BU-712-1216     M1         NA                     0         0.05 
#> 10 McAbee H1 M-2015-1-104    M1         NA                     0         0.121
#> # ℹ 182 more rows
#> # ℹ 34 more variables: petiole_area <dbl>, blade_area <dbl>,
#> #   blade_perimeter <lgl>, feret <dbl>, minimum_feret <lgl>,
#> #   raw_blade_area <dbl>, raw_blade_perimeter <dbl>,
#> #   internal_raw_blade_area <dbl>, internal_raw_blade_perimeter <dbl>,
#> #   length_of_cut_perimeter <dbl>, no_of_primary_teeth <dbl>,
#> #   no_of_subsidiary_teeth <dbl>, leaf_area <dbl>, feret_diameter <dbl>, …
```
