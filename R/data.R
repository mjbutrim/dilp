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
