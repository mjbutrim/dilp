# Generate leaf mass per area results

`calc_lma()` will typically only be called internally by
[`lma()`](https://mjbutrim.github.io/dilp/reference/lma.md). It provides
the flexibility to use custom regression parameters to calculate leaf
mass per area (LMA).

## Usage

``` r
calc_lma(data, params, resolution = "species")
```

## Arguments

- data:

  Must include "petiole metric" or some combination of columns to
  calculate petiole metric such as "Blade Area", "Petiole Area", and
  "Petiole Width", or "Leaf Area" and "Petiole Width". If calculating
  morphospecies-mean LMA, must include "Site" and "Morphotype" columns.
  If calculating species-mean LMA, only needs to include a "Site'
  column.

- params:

  A list of regression parameters. Must contain "stat" (= "mean" or =
  "variance"), "regression_slope", "y_intercept",
  "unexplained_mean_square", "sample_size_calibration"
  "mean_log_petiole_metric_calibration", "sum_of_squares_calibration",
  and "critical_value".

  Pre-loaded sets of parameters:

  "royer_species_mean_ma":

  :   - stat = "mean",

      - regression_slope = 0.382,

      - y_intercept = 3.070,

      - unexplained_mean_square = 0.032237,

      - sample_size_calibration = 667,

      - mean_log_petiole_metric_calibration = -3.011,

      - sum_of_squares_calibration = 182.1,

      - critical_value = 1.964

  "royer_site_mean_ma":

  :   - stat = "mean",

      - regression_slope = 0.429,

      - y_intercept = 3.214,

      - unexplained_mean_square = 0.005285,

      - sample_size_calibration = 25,

      - mean_log_petiole_metric_calibration = -2.857,

      - sum_of_squares_calibration = 5.331,

      - critical_value = 2.069

  "lowe_site_mean_ma":

  :   - stat = "mean",

      - regression_slope = 0.345,

      - y_intercept = 2.954,

      - unexplained_mean_square = 0.01212861,

      - sample_size_calibration = 70,

      - mean_log_petiole_metric_calibration = -2.902972,

      - sum_of_squares_calibration = 1.154691,

      - critical_value = 1.995469

  "lowe_site_variance_ma":

  :   - stat = "variance",

      - regression_slope = 0.302,

      - y_intercept = 5.028,

      - unexplained_mean_square = 0.1713672,

      - sample_size_calibration = 70,

      - mean_log_petiole_metric_calibration = -5.97104,

      - sum_of_squares_calibration = 5.085184,

      - critical_value = 1.995469

- resolution:

  Either "species" or "site". Informs whether the function should
  calculate morphospecies-mean LMA values ("species") or site-mean/site-
  variance LMA values ("site"). If resolution = "site", data must
  already be in the form of species-mean LMA.

## Value

A table with LMA results

## References

- Royer, D. L., L. Sack, P. Wilf, C. H. Lusk, G. J. Jordan, Ulo
  Niinemets, I. J. Wright, et al. 2007. Fossil Leaf Economics
  Quantified: Calibration, Eocene Case Study, and Implications.
  Paleobiology 33: 574–589

- Lowe, A. J., D. L. Royer, D. J. Wieczynski, M. J. Butrim, T.
  Reichgelt, L. Azevedo-Schmidt, D. J. Peppe, et al. 2024. Global
  patterns in community-scale leaf mass per area distributions of woody
  non-monocot angiosperms and their utility in the fossil record. In
  review.

## Examples

``` r
# Calculate morphospecies-mean LMA values with the parameters from Royer et al. (2007)
results <- calc_lma(McAbeeExample,
  params = list(
    stat = "mean",
    regression_slope = 0.382,
    y_intercept = 3.070,
    unexplained_mean_square = 0.032237,
    sample_size_calibration = 667,
    mean_log_petiole_metric_calibration = -3.011,
    sum_of_squares_calibration = 182.1,
    critical_value = 1.964
  ),
  resolution = "species"
)
results
#>         site morphotype  n petiole_metric    lower     value     upper
#> 1  McAbee H1         M1 10   0.0018166221 81.37167 105.44818 136.64852
#> 2  McAbee H1         M5  3   0.0011067149 54.54670  87.26130 139.59661
#> 3  McAbee H1         M8  8   0.0011510539 66.35987  88.58058 118.24193
#> 4  McAbee H1        M12  1   0.0010808383 38.37080  86.47620 194.89128
#> 5  McAbee H1        M13  1   0.0006070381 30.77894  69.37268 156.35916
#> 6  McAbee H1        M18  1   0.0011637972 39.46986  88.95393 200.47708
#> 7  McAbee H1        M19  1   0.0008073257 34.32356  77.35576 174.33838
#> 8  McAbee H1        M24  5   0.0002309535 33.24559  47.95859  69.18289
#> 9  McAbee H1        M28  1   0.0002175761 20.78085  46.87783 105.74787
#> 10 McAbee H1        M41  1   0.0002994371 23.48554  52.96009 119.42543
#> 11 McAbee H1        M44  1   0.0005108600 28.81385  64.94883 146.40011
#> 12 McAbee H1        M73  1   0.0003778943 25.67425  57.88359 130.50081
#> 13 McAbee H1        M79  2   0.0003960090 33.14315  58.92822 104.77386
#> 14 McAbee H1        M91  1   0.0001524066 18.12949  40.91735  92.34838
#> 15 McAbee H2         M1  3   0.0008877491 50.14134  80.21339 128.32102
#> 16 McAbee H2         M8  2   0.0010796906 48.64042  86.44111 153.61845
#> 17 McAbee H2        M18  1   0.0002132426 20.62123  46.51895 104.94097
#> 18 McAbee H2        M19  4   0.0006803596 48.21879  72.46130 108.89200
#> 19 McAbee H2        M22  3   0.0008971039 50.34260  80.53524 128.83571
#> 20 McAbee H2        M24  2   0.0007082087 41.40139  73.58031 130.77005
#> 21 McAbee H2        M28  1   0.0004553366 27.57296  62.15600 140.11437
#> 22 McAbee H2        M29  1   0.0001254609 16.82558  37.98658  85.76111
#> 23 McAbee H2        M31  2   0.0003283234 30.84620  54.85640  97.55577
#> 24 McAbee H2        M47  2   0.0009868057 46.99765  83.52115 148.42836
#> 25 McAbee H2        M76  1   0.0001534697 18.17789  41.02614  92.59292
#> 26 McAbee H2        M94  1   0.0001232820 16.71284  37.73320  85.19165
#> 27 McAbee H2        M97  1   0.0005314955 29.25363  65.93877 148.62843

# Calculate site-mean LMA values with the parameters from Lowe et al. (2024) entered from scratch
site_results <- calc_lma(results,
  params = list(
    stat = "mean",
    regression_slope = 0.345,
    y_intercept = 2.954,
    unexplained_mean_square = 0.01212861,
    sample_size_calibration = 70,
    mean_log_petiole_metric_calibration = -2.902972,
    sum_of_squares_calibration = 1.154691,
    critical_value = 1.995469
  ),
  resolution = "site"
)
site_results
#>        site  n    lower    value    upper
#> 1 McAbee H1 14 61.03750 73.68178 88.94539
#> 2 McAbee H2 13 53.87969 67.58568 84.77822
```
