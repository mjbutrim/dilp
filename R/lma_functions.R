calc_lma <- function(data, params, resolution = "species") {
  if ("petiole metric" %in% colnames(data)) {
    data <- dplyr::filter(data, data$`petiole metric` > 0)
  } else {
    data$`Petiole area` <- ifelse(data$`Blade area` > 0 & is.na(data$`Petiole area`), 0, data$`Petiole area`)
    data$`Leaf area` <- data$`Blade area` + data$`Petiole area`
    data$`petiole metric` <- (data$`Petiole width`^2) / data$`Leaf area`
    data <- dplyr::filter(data, data$`petiole metric` > 0)
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
    stop("Params$stat is invalid")
  }

  if (resolution == "species") {
    species_site_combos <- data %>%
      dplyr::group_by(Site) %>%
      dplyr::distinct(Morphotype) %>%
      dplyr::ungroup() %>%
      cbind("n" = NA, "petiole metric" = NA, "lower" = NA, "value" = NA, "upper" = NA)

    for (i in 1:nrow(species_site_combos)) {
      ssp_subset <- dplyr::filter(data, data$Site ==
        as.character(species_site_combos$Site[i]) &
        data$Morphotype ==
          as.character(species_site_combos$Morphotype[i]))
      value <- 10^(params$regression_slope * log10(stat_function(ssp_subset$`petiole metric`))
        + params$y_intercept)
      species_site_combos$`petiole metric`[i] <- mean(ssp_subset$`petiole metric`)
      species_site_combos$n[i] <- nrow(ssp_subset)
      species_site_combos$value[i] <- value
      species_site_combos$upper[i] <-
        10^(log10(value) + sqrt(params$unexplained_mean_square * ((1 / nrow(ssp_subset))
        + (1 / params$sample_size_calibration) + (((log10(stat_function(ssp_subset$`petiole metric`)) -
            params$mean_log_petiole_metric_calibration)^2) / params$sum_of_squares_calibration))) * params$critical_value)
      species_site_combos$lower[i] <-
        10^(log10(value) - sqrt(params$unexplained_mean_square * ((1 / nrow(ssp_subset))
        + (1 / params$sample_size_calibration) + (((log10(stat_function(ssp_subset$`petiole metric`)) -
            params$mean_log_petiole_metric_calibration)^2) / params$sum_of_squares_calibration))) * params$critical_value)
    }

    return(species_site_combos)
  } else if (resolution == "site") {
    sites <- data %>%
      dplyr::distinct(Site) %>%
      cbind("n" = NA, "lower" = NA, "value" = NA, "upper" = NA)

    for (i in 1:nrow(sites)) {
      site_subset <- dplyr::filter(data, data$Site
      == as.character(sites$Site[i]))
      value <- 10^(params$regression_slope * log10(stat_function(site_subset$`petiole metric`))
        + params$y_intercept)
      sites$n[i] <- nrow(site_subset)
      sites$value[i] <- value
      sites$lower[i] <-
        10^(log10(value) - sqrt(params$unexplained_mean_square * ((1 / nrow(site_subset))
        + (1 / params$sample_size_calibration) + (((log10(stat_function(site_subset$`petiole metric`)) -
            params$mean_log_petiole_metric_calibration)^2) / params$sum_of_squares_calibration))) * params$critical_value)
      sites$upper[i] <-
        10^(log10(value) + sqrt(params$unexplained_mean_square * ((1 / nrow(site_subset))
        + (1 / params$sample_size_calibration) + (((log10(stat_function(site_subset$`petiole metric`)) -
            params$mean_log_petiole_metric_calibration)^2) / params$sum_of_squares_calibration))) * params$critical_value)
    }

    return(sites)
  } else {
    warning('Resolution invalid: must be "species" or "site"')
  }
}

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
