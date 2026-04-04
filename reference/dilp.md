# Generate DiLP results

`dilp()` processes raw leaf physiognomic data, checks for common
errors/outliers, and returns the processed data, keys to finding
potential errors or outliers, and paleoclimate reconstructions.

## Usage

``` r
dilp(specimen_data, params = "PeppeGlobal", subsite_cols = NULL)
```

## Arguments

- specimen_data:

  A data frame containing specimen level leaf physiognomic data. See
  Lowe et al. 2024 for more information on how to collect this data. A
  good reference for how to put together the data:
  [`McAbeeExample`](https://mjbutrim.github.io/dilp/reference/McAbeeExample.md)

  Required columns:

  - site

  - specimen_number

  - morphotype

  - margin

  - feret

  - blade_area

  - raw_blade_perimeter

  - internal_raw_blade_perimeter

  - length_of_cut_perimeter

  - no_of_primary_teeth

  - no_of_subsidiary_teeth

  Recommended columns:

  - petiole_width

  - petiole_area

  - blade_perimeter

  - minimum_feret

  - raw_blade_area

  - internal_raw_blade_area

- params:

  Either a string referring to one of two preloaded parameter sets of a
  list of custom parameters (same format as the list below).

  Preloaded parameter sets are "PeppeGlobal" and "PeppeNH" which are
  calibrated based on global and northern hemisphere data respectively.
  Allen et al. (2020) illustrates a situation in which the northern
  hemisphere parameters may be preferable. The "PeppeNH" parameters only
  estimate MAT. Use "PeppeGlobal" for all MAP estimates. Defaults to
  "PeppeGlobal" as follows (Peppe et al. 2011):

  - MAT.MLR.M = 0.21,

  - MAT.MLR.FDR = 42.296,

  - MAT.MLR.TC.IP = -2.609,

  - MAT.MLR.constant = -16.004,

  - MAT.MLR.error = 4,

  - MAT.SLR.M = 0.204,

  - MAT.SLR.constant = 4.6,

  - MAT.SLR.error = 4.9,

  - MAP.MLR.LA = 0.298,

  - MAP.MLR.TC.IP = 0.279,

  - MAP.MLR.PR = -2.717,

  - MAP.MLR.constant = 3.033,

  - MAP.MLR.SE = 0.6,

  - MAP.SLR.LA = 0.283,

  - MAP.SLR.constant = 2.92,

  - MAP.SLR.SE = 0.61

- subsite_cols:

  A vector or list of columns present in `specimen_data` to calculate
  paleoclimate estimates for. A completely optional parameter - allows
  different groupings of specimens to be tested, or comparisons of
  paleoclimate estimates at different levels of grouping. Adds
  additional estimates to \$results.

## Value

A list of tables that includes all pertinent DiLP information:

- processed_leaf_data: the full set of cleaned and newly calculated leaf
  physiognomic data that is necessary for DiLP analysis. See
  [`dilp_processing()`](https://mjbutrim.github.io/dilp/reference/dilp_processing.md)
  for more information.

- processed_morphotype_data: morphospecies-site pair means for all leaf
  physiognomic data.

- processed_site_data: site means for all leaf physiognomic data.

- errors: lists any specimens that may be causing common errors in DiLP
  calculations. See
  [`dilp_errors()`](https://mjbutrim.github.io/dilp/reference/dilp_errors.md)
  for more information.

- outliers: flags outliers in variables used for DiLP analysis that may
  represent incorrect data. See
  [`dilp_outliers()`](https://mjbutrim.github.io/dilp/reference/dilp_outliers.md)
  for more information.

- results: climate reconstructions of MAT and MAP using single and
  multi-linear regressions.

## References

- Allen, S. E., Lowe, A. J., Peppe, D. J., & Meyer, H. W. (2020).
  Paleoclimate and paleoecology of the latest Eocene Florissant flora of
  central Colorado, USA. Palaeogeography, Palaeoclimatology,
  Palaeoecology, 551, 109678.

- Peppe, D.J., Royer, D.L., Cariglino, B., Oliver, S.Y., Newman, S.,
  Leight, E., Enikolopov, G., Fernandez-Burgos, M., Herrera, F., Adams,
  J.M., Correa, E., Currano, E.D., Erickson, J.M., Hinojosa, L.F.,
  Hoganson, J.W., Iglesias, A., Jaramillo, C.A., Johnson, K.R., Jordan,
  G.J., Kraft, N.J.B., Lovelock, E.C., Lusk, C.H., Niinemets, Ü.,
  Peñuelas, J., Rapson, G., Wing, S.L. and Wright, I.J. (2011),
  Sensitivity of leaf size and shape to climate: global patterns and
  paleoclimatic applications. New Phytologist, 190: 724-739.
  https://doi.org/10.1111/j.1469-8137.2010.03615.x

- Lowe. A.J., Flynn, A.G., Butrim, M.J., Baumgartner, A., Peppe, D.J.,
  and Royer, D.L. (2024), Reconstructing terrestrial paleoclimate and
  paleoecology with fossil leaves using Digital Leaf Physiognomy and
  leaf mass per area. J. Vis. Exp. (212), e66838, doi:10.3791/66838
  (2024).

## Examples

``` r
dilp_results <- dilp(McAbeeExample)
#> Warning: Outliers found. Please evaluate $outliers for possible wrong measurements
dilp_results$processed_leaf_data
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
dilp_results$processed_morphotype_data
#> # A tibble: 61 × 38
#>    site  morphotype margin petiole_width petiole_area blade_area blade_perimeter
#>    <chr> <chr>       <dbl>         <dbl>        <dbl>      <dbl>           <dbl>
#>  1 McAb… M1              0        0.0962       0.0223       6.14             NaN
#>  2 McAb… M3              0      NaN          NaN          NaN                NaN
#>  3 McAb… M5              0        0.114        0.029        9.83             NaN
#>  4 McAb… M8              0        0.0765       0.0407       8.79             NaN
#>  5 McAb… M11             1      NaN          NaN          NaN                NaN
#>  6 McAb… M12             1        0.038        0.01         1.33             NaN
#>  7 McAb… M13             0        0.069        0.047        7.80             NaN
#>  8 McAb… M15             0      NaN          NaN          NaN                NaN
#>  9 McAb… M16             0      NaN          NaN          NaN                NaN
#> 10 McAb… M18             0        0.11         0.101       14.8              NaN
#> # ℹ 51 more rows
#> # ℹ 31 more variables: feret <dbl>, minimum_feret <dbl>, raw_blade_area <dbl>,
#> #   raw_blade_perimeter <dbl>, internal_raw_blade_area <dbl>,
#> #   internal_raw_blade_perimeter <dbl>, length_of_cut_perimeter <dbl>,
#> #   no_of_primary_teeth <dbl>, no_of_subsidiary_teeth <dbl>, leaf_area <dbl>,
#> #   feret_diameter <dbl>, fdr <dbl>, raw_blade_perimeter_corrected <dbl>,
#> #   internal_raw_blade_perimeter_corrected <dbl>, total_tooth_count <dbl>, …
dilp_results$processed_site_data
#> # A tibble: 2 × 37
#>   site      margin petiole_width petiole_area blade_area blade_perimeter feret
#>   <chr>      <dbl>         <dbl>        <dbl>      <dbl>           <dbl> <dbl>
#> 1 McAbee H1   32.3        0.0710       0.0709       11.6             NaN  5.19
#> 2 McAbee H2   23.3        0.0766       0.0894       16.4             NaN  5.92
#> # ℹ 30 more variables: minimum_feret <dbl>, raw_blade_area <dbl>,
#> #   raw_blade_perimeter <dbl>, internal_raw_blade_area <dbl>,
#> #   internal_raw_blade_perimeter <dbl>, length_of_cut_perimeter <dbl>,
#> #   no_of_primary_teeth <dbl>, no_of_subsidiary_teeth <dbl>, leaf_area <dbl>,
#> #   feret_diameter <dbl>, fdr <dbl>, raw_blade_perimeter_corrected <dbl>,
#> #   internal_raw_blade_perimeter_corrected <dbl>, total_tooth_count <dbl>,
#> #   tc_ip <dbl>, perimeter_ratio <dbl>, ln_leaf_area <dbl>, ln_pr <dbl>, …
dilp_results$errors
#>                                                   Check specimen_number
#> 1                             Entire tooth count not NA No errors found
#> 2                        Entire tooth count : IP not NA No errors found
#> 3                         Entire perimeter ratio not NA No errors found
#> 4                                   FDR not between 0-1 No errors found
#> 5 External perimeter not larger than internal perimeter No errors found
#> 6                Feret is not larger than minimum Feret No errors found
#> 7                           Perimeter ratio less than 1 No errors found
dilp_results$outliers
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
dilp_results$results
#>        site   margin       fdr    tc_ip ln_leaf_area  ln_tc_ip     ln_pr
#> 1 McAbee H1 32.25806 0.6965086 2.562487     6.792833 0.6070757 0.2047427
#> 2 McAbee H2 23.33333 0.7012671 2.651242     7.037892 0.6218561 0.1504205
#>    MAT.MLR MAT.MLR.error  MAT.SLR MAT.SLR.error  MAP.MLR MAP.MLR.error.plus
#> 1 13.54419             4 11.18065           4.8 106.7353           87.74914
#> 2 11.63970             4  9.36000           4.8 133.6330          109.86219
#>   MAP.MLR.error.minus  MAP.SLR MAP.SLR.error.plus MAP.SLR.error.minus
#> 1            48.15775 126.7697           106.5412            57.88926
#> 2            60.29365 135.8734           114.1923            62.04646
```
