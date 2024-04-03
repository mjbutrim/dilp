# dilp (development version)
* Added `view_regressions()` to allow users to easily see their options for preloaded regressions in dilp, lma, temp, and precip.
* Fixed `dilp()` so that the mixed_margins aren't screwing everything up.
* Fixed `dilp()` documentation to properly reference the correct parameter options.
* Entry field "no_primary_teeth" changed to "no_of_primary_teeth" for consistency
* Updated provided data entry template so that column names match listed expected column names
* Added conditionals to `dilp()` so that if you enter blanks for raw blade area, and raw blade perimeter, but length of cut perimeter is 0, blade perimeter/blade area are present, and the leaf is toothed, the raw values are automatically filled in with the blade perimeter/blade area values.
* Added conditionals to `dilp()` site_mean values so that a locality with only untoothed leaves is assigned a site tc_ip of 0 and a site perimeter_ratio of 1, allowing MAT estimates to be formed.

# dilp 1.0.0
* Initial CRAN submission
* `dilp()` given new argument `subsite_cols` to allow multiple tiers of specimen/morphotype organization.
* `dilp()` given new options for regression parameters in `params`.  Choose between "global" and "northern_hemisphere".

# dilp (development version 0.0.0.9001)
* Functions implemented for simple linear regression (temp_slr() and precip_slr())

# dilp (development version 0.0.0.9000)

* Functions implemented for digital leaf physiognomy (dilp())
* Functions implemented for leaf mass per area reconstruction (lma())
