#' McAbee Example Data
#'
#' Leaf physiognomic data taken from specimens collected at the McAbee
#' locality in Canada somewhere by Alex at some point
#'
#' @format ## `McAbeeExample`
#' A data frame with 192 rows and 18 columns:
#' \describe{
#'   \item{Site}{Stratigraphic layer}
#'   \item{Specimen Number}{Repository number for individual fossils}
#'   \item{Morphotype}{Morphotype the specimen belongs to}
#'   \item{Measurer comments}{Any comments about the specimen}
#'   \item{Margin}{Whether the margin is toothed}
#'   \item{Petiole Width}{The width of the petiole at the basalmost point of insertion (cm)}
#' }
#' @source Lowe et al. 2018
"McAbeeExample"

#' Climate Calibration Data
#'
#' Temperature and precipitation data for modern localities used to calibrate the DiLP model
#'
#' @format ## `climateCalibration`
#' A data frame with 92 rows and 3 columns:
#' \describe{
#'   \item{Site}{Locality name}
#'   \item{MAT}{Mean Annual Temperature (celsius)}
#'   \item{MAP}{Mean Annual Precipitation (mm)}
#' }
#' @source Peppe et al. 2011
"climateCalibration"

#' Physiognomy Calibration Data
#'
#' Leaf physiognomic data taken from modern localities used to calibrate the DiLP model
#'
#' @format ## `physiognomyCalibration`
#' A data frame with 92 rows and 12 columns:
#' \describe{
#'   \item{Site}{Locality name}
#'   \item{Leaf.area}{Average leaf area at site}
#'   \item{FDR}{Average Feret Diameter Ratio}
#'   \item{Perimeter.ratio}{Average ratio between toothed and untoothed perimeter}
#' }
#' @source Peppe et al. 2011
"physiognomyCalibration"

# Different sets of parameters for leaf mass per area reconstruction , used in lma_functions
royer_species_mean_ma <- list(
  stat = "mean",
  regression_slope = 0.382,
  y_intercept = 3.070,
  unexplained_mean_square = 0.032237,
  sample_size_calibration = 667,
  mean_log_petiole_metric_calibration = -3.011,
  sum_of_squares_calibration = 182.1,
  critical_value = 1.964
)

royer_site_mean_ma <- list(
  stat = "mean",
  regression_slope = 0.429,
  y_intercept = 3.214,
  unexplained_mean_square = 0.005285,
  sample_size_calibration = 25,
  mean_log_petiole_metric_calibration = -2.857,
  sum_of_squares_calibration = 5.331,
  critical_value = 2.069
)

lowe_site_mean_ma <- list(
  stat = "mean",
  regression_slope = 0.345,
  y_intercept = 2.954,
  unexplained_mean_square = 0.01212861,
  sample_size_calibration = 70,
  mean_log_petiole_metric_calibration = -2.902972,
  sum_of_squares_calibration = 1.154691,
  critical_value = 1.995469
)

lowe_site_variance_ma <- list(
  stat = "variance",
  regression_slope = 0.302,
  y_intercept = 5.028,
  unexplained_mean_square = 0.1713672,
  sample_size_calibration = 70,
  mean_log_petiole_metric_calibration = -5.97104,
  sum_of_squares_calibration = 5.085184,
  critical_value = 1.995469
)

dilp_parameters <- list(
  MAT.MLR.M = 0.21,
  MAT.MLR.FDR = 42.296,
  MAT.MLR.TC.IP = -2.609,
  MAT.MLR.constant = -16.004,
  MAT.MLR.error = 4,
  MAT.SLR.M = 0.204,
  MAT.SLR.constant = 4.6,
  MAT.SLR.error = 4.9,
  MAP.MLR.LA = 0.298,
  MAP.MLR.TC.IP = 0.279,
  MAP.MLR.PR = -2.717,
  MAP.MLR.constant = 3.033,
  MAP.MLR.SE = 0.6,
  MAP.SLR.LA = 0.283,
  MAP.SLR.constant = 2.92,
  MAP.SLR.SE = 0.61
)
