#' Generate leaf mass per area results
#'
#' @description
#' `calc_lma()` will typically only be called internally by [lma()]. It provides
#' the flexibility to use custom regression parameters to calculate leaf mass
#' per area (LMA).
#'
#' @param data Must include "petiole metric" or some combination of columns to
#' calculate petiole metric such as "Blade Area", "Petiole Area", and "Petiole Width",
#' or "Leaf Area" and "Petiole Width".  If calculating morphospecies-mean LMA,
#' must include "Site" and "Morphotype" columns. If calculating species-mean LMA,
#' only needs to include a "Site' column.
#' @param params A list of regression parameters. Must contain "stat" (= "mean" or = "variance"),
#' "regression_slope", "y_intercept", "unexplained_mean_square", "sample_size_calibration"
#' "mean_log_petiole_metric_calibration", "sum_of_squares_calibration", and "critical_value".
#'
#' Pre-loaded sets of parameters:
#'
#' \describe{
#'  \item{"royer_species_mean_ma":}{
#'    * stat = "mean",
#'    * regression_slope = 0.382,
#'    * y_intercept = 3.070,
#'    * unexplained_mean_square = 0.032237,
#'    * sample_size_calibration = 667,
#'    * mean_log_petiole_metric_calibration = -3.011,
#'    * sum_of_squares_calibration = 182.1,
#'    * critical_value = 1.964}
#'
#'  \item{"royer_site_mean_ma":}{
#'    * stat = "mean",
#'    * regression_slope = 0.429,
#'    * y_intercept = 3.214,
#'    * unexplained_mean_square = 0.005285,
#'    * sample_size_calibration = 25,
#'    * mean_log_petiole_metric_calibration = -2.857,
#'    * sum_of_squares_calibration = 5.331,
#'    * critical_value = 2.069}
#'
#'  \item{"lowe_site_mean_ma":}{
#'    * stat = "mean",
#'    * regression_slope = 0.345,
#'    * y_intercept = 2.954,
#'    * unexplained_mean_square = 0.01212861,
#'    * sample_size_calibration = 70,
#'    * mean_log_petiole_metric_calibration = -2.902972,
#'    * sum_of_squares_calibration = 1.154691,
#'    * critical_value = 1.995469}
#'
#'  \item{"lowe_site_variance_ma":}{
#'    * stat = "variance",
#'    * regression_slope = 0.302,
#'    * y_intercept = 5.028,
#'    * unexplained_mean_square = 0.1713672,
#'    * sample_size_calibration = 70,
#'    * mean_log_petiole_metric_calibration = -5.97104,
#'    * sum_of_squares_calibration = 5.085184,
#'    * critical_value = 1.995469}
#' }
#'
#' @param resolution Either "species" or "site".  Informs whether the function
#' should calculate morphospecies-mean LMA values ("species") or site-mean/site-
#' variance LMA values ("site").  If resolution = "site", data must already be
#' in the form of species-mean LMA.
#'
#' @return A table with LMA results
#'
#' @references
#' * Royer, D. L., L. Sack, P. Wilf, C. H. Lusk, G. J. Jordan, Ulo Niinemets, I. J. Wright, et al. 2007. Fossil Leaf Economics Quantified: Calibration, Eocene Case Study, and Implications. Paleobiology 33: 574–589
#' * Lowe, A. J., D. L. Royer, D. J. Wieczynski, M. J. Butrim, T. Reichgelt, L. Azevedo-Schmidt, D. J. Peppe, et al. 2024. Global patterns in community-scale leaf mass per area distributions of woody non-monocot angiosperms and their utility in the fossil record. In review.
#'
#' @export
#'
#' @examples
#' # Calculate morphospecies-mean LMA values with the parameters from Royer et al. (2007)
#' results <- calc_lma(McAbeeExample,
#'   params = "royer_species_mean_ma",
#'   resolution = "species"
#' )
#' results
#'
#' # Calculate site-mean LMA values with the parameters from Lowe et al. (2024) entered from scratch
#' site_results <- calc_lma(results,
#'   params = list(
#'     stat = "mean",
#'     regression_slope = 0.345,
#'     y_intercept = 2.954,
#'     unexplained_mean_square = 0.01212861,
#'     sample_size_calibration = 70,
#'     mean_log_petiole_metric_calibration = -2.902972,
#'     sum_of_squares_calibration = 1.154691,
#'     critical_value = 1.995469
#'   ),
#'   resolution = "site"
#' )
#' site_results
#'
calc_lma <- function(data, params, resolution = "species") {
  colnames(data) <- colnameClean(data)

  if ("petiole_metric" %in% colnames(data)) {
    data <- dplyr::filter(data, data$petiole_metric > 0)
    data$site <- data$site
    data$morphotype <- data$morphotype
  } else if ("leaf_area" %in% colnames(data) && "petiole_width" %in% colnames(data)) {
    data$petiole_metric <- (data$petiole_width^2) / data$leaf_area
    data <- dplyr::filter(data, data$petiole_metric > 0)
  } else if ("blade_area" %in% colnames(data) && "petiole_width" %in% colnames(data)) {
    if("petiole_area" %in% colnames(data)){

    } else {
      data[["petiole_area"]] <- NA
      warning("data is missing petiole_area and has filled it with NAs", call. = FALSE)
    }
    data$petiole_area <- ifelse(data$blade_area > 0 & is.na(data$petiole_area), 0, data$petiole_area)
    data$leaf_area <- data$blade_area + data$petiole_area
    data$petiole_metric <- (data$petiole_width^2) / data$leaf_area
    data <- dplyr::filter(data, data$petiole_metric > 0)
  } else {
    stop('Parameters for calculating LMA not present: Requires either "petiole_metric", or "leaf_area" and "petiole_width", or "blade_area", "petiole_area" and "petiole_width".')
  }

  if (params$stat == "mean") {
    stat_function <- function(x) {
      return(mean(x))
    }
  } else if (params$stat == "variance") {
    stat_function <- function(x) {
      return(stats::var(x))
    }
  } else {
    stop('Params$stat is invalid. Must be either "variance" or "mean"')
  }

  if (resolution == "species") {
    species_site_combos <- data %>%
      dplyr::group_by(.data$site) %>%
      dplyr::distinct(.data$morphotype) %>%
      dplyr::ungroup() %>%
      cbind("n" = NA, "petiole_metric" = NA, "lower" = NA, "value" = NA, "upper" = NA)

    for (i in 1:nrow(species_site_combos)) {
      ssp_subset <- dplyr::filter(data, data$site ==
        as.character(species_site_combos$site[i]) &
        data$morphotype ==
          as.character(species_site_combos$morphotype[i]))
      value <- 10^(params$regression_slope * log10(stat_function(ssp_subset$petiole_metric))
        + params$y_intercept)
      species_site_combos$petiole_metric[i] <- mean(ssp_subset$petiole_metric)
      species_site_combos$n[i] <- nrow(ssp_subset)
      species_site_combos$value[i] <- value
      species_site_combos$upper[i] <-
        10^(log10(value) + sqrt(params$unexplained_mean_square * ((1 / nrow(ssp_subset))
        + (1 / params$sample_size_calibration) + (((log10(stat_function(ssp_subset$petiole_metric)) -
            params$mean_log_petiole_metric_calibration)^2) / params$sum_of_squares_calibration))) * params$critical_value)
      species_site_combos$lower[i] <-
        10^(log10(value) - sqrt(params$unexplained_mean_square * ((1 / nrow(ssp_subset))
        + (1 / params$sample_size_calibration) + (((log10(stat_function(ssp_subset$petiole_metric)) -
            params$mean_log_petiole_metric_calibration)^2) / params$sum_of_squares_calibration))) * params$critical_value)
    }

    return(species_site_combos)
  } else if (resolution == "site") {
    sites <- data %>%
      dplyr::distinct(.data$site) %>%
      cbind("n" = NA, "lower" = NA, "value" = NA, "upper" = NA)

    for (i in 1:nrow(sites)) {
      site_subset <- dplyr::filter(data, data$site
      == as.character(sites$site[i]))
      value <- 10^(params$regression_slope * log10(stat_function(site_subset$petiole_metric))
        + params$y_intercept)
      sites$n[i] <- nrow(site_subset)
      sites$value[i] <- value
      sites$lower[i] <-
        10^(log10(value) - sqrt(params$unexplained_mean_square * ((1 / nrow(site_subset))
        + (1 / params$sample_size_calibration) + (((log10(stat_function(site_subset$petiole_metric)) -
            params$mean_log_petiole_metric_calibration)^2) / params$sum_of_squares_calibration))) * params$critical_value)
      sites$upper[i] <-
        10^(log10(value) + sqrt(params$unexplained_mean_square * ((1 / nrow(site_subset))
        + (1 / params$sample_size_calibration) + (((log10(stat_function(site_subset$petiole_metric)) -
            params$mean_log_petiole_metric_calibration)^2) / params$sum_of_squares_calibration))) * params$critical_value)
    }

    return(sites)
  } else {
    warning('Resolution invalid: must be "species" or "site"')
  }
}

#' Generate a suite of leaf mass per area results
#'
#' @description
#' `lma()` takes either raw or processed leaf physiognomic data and returns
#' leaf mass per area (LMA) reconstructions of species-mean, site-mean, and site-
#' variance.
#'
#' `lma()` calls [calc_lma()] multiple times with different sets of
#' parameters.  See [calc_lma()] for more control over LMA reconstructions.
#'
#' @param specimen_data A table that must include "Site", "Morphotype", and
#' either "Petiole Metric", or "Blade Area", "Petiole Area", and "Petiole Width".
#'
#' @return A list of tables containing leaf mass per area reconstructions.
#' * species_mean_lma contains the average LMA for each morphospecies-site pair.
#' Values calculated using the regression from Royer et al. (2007).
#' * royer_site_mean_lma contains the average LMA for each site. Values calculated
#' using the regression from Royer et al. (2007)
#' * lowe_site_lma contains the average LMA for each site. Values calculated using
#' the regression from Lowe et al. (2024)
#' * lowe_variance contains the variance in LMA for each site. Values calculated
#' using the regression from Lowe et al. (2024)
#' @references
#' * Royer, D. L., L. Sack, P. Wilf, C. H. Lusk, G. J. Jordan, Ulo Niinemets, I. J. Wright, et al. 2007. Fossil Leaf Economics Quantified: Calibration, Eocene Case Study, and Implications. Paleobiology 33: 574–589
#' * Lowe, A. J., D. L. Royer, D. J. Wieczynski, M. J. Butrim, T. Reichgelt, L. Azevedo-Schmidt, D. J. Peppe, et al. 2024. Global patterns in community-scale leaf mass per area distributions of woody non-monocot angiosperms and their utility in the fossil record. In review.
#'
#' @export
#'
#' @examples
#' results <- lma(McAbeeExample)
#' results
lma <- function(specimen_data) {
  species_lma <- calc_lma(specimen_data, params = royer_species_mean_ma, resolution = "species")
  royer_site_lma <- calc_lma(species_lma, params = royer_site_mean_ma, resolution = "site")
  lowe_site_lma <- calc_lma(species_lma, params = lowe_site_mean_ma, resolution = "site")
  lowe_variance <- calc_lma(species_lma, params = lowe_site_variance_ma, resolution = "site")

  return(list(
    species_mean_lma = species_lma,
    royer_site_mean_lma = royer_site_lma,
    lowe_site_mean_lma = lowe_site_lma,
    lowe_site_variance_lma = lowe_variance
  ))
}
