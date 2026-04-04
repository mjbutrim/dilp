# Changelog

## dilp 1.3.0

- Refixed outliers merge to improve readability
- Added some extra visiblity parameters in
  [`dilp_cca()`](https://mjbutrim.github.io/dilp/reference/dilp_cca.md)
  that still don’t completely work

## dilp 1.2.1

- Fixed Outliers merge so there aren’t thousands of outliers.
- Adjusted Errors so that it pulls by column header rather than index

## dilp 1.2.0

- Added Miller(2006) error calculations to
  [`temp_slr()`](https://mjbutrim.github.io/dilp/reference/temp_slr.md)
- Added KowalskiDilcher (2003) regression to
  [`temp_slr()`](https://mjbutrim.github.io/dilp/reference/temp_slr.md)
- Added
  [`dilp_whittaker()`](https://mjbutrim.github.io/dilp/reference/dilp_whittaker.md)
  function for plotting localities to biome.
- Makeover for error and outlier reporting from
  [`dilp()`](https://mjbutrim.github.io/dilp/reference/dilp.md)

## dilp 1.1.0

CRAN release: 2024-04-05

- Added
  [`view_regressions()`](https://mjbutrim.github.io/dilp/reference/view_regressions.md)
  to allow users to easily see their options for preloaded regressions
  in dilp, lma, temp, and precip.
- Fixed [`dilp()`](https://mjbutrim.github.io/dilp/reference/dilp.md) so
  that the mixed_margins aren’t screwing everything up.
- Fixed [`dilp()`](https://mjbutrim.github.io/dilp/reference/dilp.md)
  documentation to properly reference the correct parameter options.
- Entry field “no_primary_teeth” changed to “no_of_primary_teeth” for
  consistency
- Updated provided data entry template so that column names match listed
  expected column names
- Added conditionals to
  [`dilp()`](https://mjbutrim.github.io/dilp/reference/dilp.md) so that
  if you enter blanks for raw blade area, and raw blade perimeter, but
  length of cut perimeter is 0, blade perimeter/blade area are present,
  and the leaf is toothed, the raw values are automatically filled in
  with the blade perimeter/blade area values.
- Added conditionals to
  [`dilp()`](https://mjbutrim.github.io/dilp/reference/dilp.md)
  site_mean values so that a locality with only untoothed leaves is
  assigned a site tc_ip of 0 and a site perimeter_ratio of 1, allowing
  MAT estimates to be formed.

## dilp 1.0.0

CRAN release: 2024-03-11

- Initial CRAN submission
- [`dilp()`](https://mjbutrim.github.io/dilp/reference/dilp.md) given
  new argument `subsite_cols` to allow multiple tiers of
  specimen/morphotype organization.
- [`dilp()`](https://mjbutrim.github.io/dilp/reference/dilp.md) given
  new options for regression parameters in `params`. Choose between
  “global” and “northern_hemisphere”.

## dilp (development version 0.0.0.9001)

- Functions implemented for simple linear regression (temp_slr() and
  precip_slr())

## dilp (development version 0.0.0.9000)

- Functions implemented for digital leaf physiognomy (dilp())
- Functions implemented for leaf mass per area reconstruction (lma())
