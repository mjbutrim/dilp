temp_slr <- function(data, regression = "peppe2018"){
  colnames(data) <- colnameClean(data) %>%
    stringr::str_replace_all("species", "morphotype")
  required_columns <- c("morphotype", "margin")
  missing_columns <- required_columns[!required_columns %in% colnames(data)]
  if(length(missing_columns) > 0){
    stop(paste("data is missing required columns:", stringr::str_flatten(missing_columns, collapse = ", ")))
  }
  if (length(unique(data$margin)) > 2) {
    data$margin[data$margin > 0 & data$margin < 1] <- 0.5
    if (length(unique(data$margin)) > 2) {
      warning("Margin states outside the bounds of [0 - 1] present")
    }
  }
  regression <- list(slope = 0.204, constant = 4.6, error = 5)
  sites <- data %>%
    dplyr::distinct(.data$site) %>%
    cbind("n" = NA, "lower" = NA, "MAT" = NA, "upper" = NA)

  for (i in 1:nrow(sites)) {
    site_subset <- dplyr::filter(data, data$site == as.character(sites$site[i])) %>%
      dplyr::group_by(morphotype) %>%
      dplyr::summarize(margin = mean(margin, na.rm = TRUE)) %>%
      stats::na.omit()

    num_morph <- nrow(site_subset)
    value <- regression$slope * (100* sum(site_subset$margin)/num_morph) + regression$constant
    sites$n[i] <- num_morph
    sites$MAT[i] <- value
    sites$lower[i] <- value - regression$error
    sites$upper[i] <- value + regression$error
  }
  return(sites)
}


precip_slr <- function(data, regression = "peppe2018"){
  colnames(data) <- colnameClean(data)

  if ("leaf_area" %in% colnames(data)) {

  } else if ("blade_area" %in% colnames(data)) {
    if("petiole_area" %in% colnames(data)){

    } else {
      data[["petiole_area"]] <- NA
      warning("petiole_area missing, blade_area used as leaf_area", call. = FALSE)
    }
    data$petiole_area <- ifelse(data$blade_area > 0 & is.na(data$petiole_area), 0, data$petiole_area)
    data$leaf_area <- data$blade_area + data$petiole_area
  }
  required_columns <- c("morphotype", "leaf_area", "specimen_number")
  missing_columns <- required_columns[!required_columns %in% colnames(data)]
  if(length(missing_columns) > 0){
    stop(paste("data is missing required columns:", stringr::str_flatten(missing_columns, collapse = ", ")))
  }

  regression <- list(slope = 0.283, constant = 2.92, error = 0.61)
  sites <- data %>%
    dplyr::distinct(.data$site) %>%
    cbind("n" = NA, "lower" = NA, "MAP" = NA, "upper" = NA)

  for (i in 1:nrow(sites)) {
    site_subset <- dplyr::filter(data, data$site == as.character(sites$site[i])) %>%
      dplyr::group_by(morphotype) %>%
      dplyr::mutate(ln_leaf_area = log(100 * leaf_area)) %>%
      dplyr::summarize(ln_leaf_area = mean(ln_leaf_area, na.rm = TRUE)) %>%
      stats::na.omit()

    value <- (mean(site_subset$ln_leaf_area) * regression$slope) + regression$constant
    sites$n[i] <- nrow(site_subset)
    sites$MAP[i] <- exp(value)
    sites$lower[i] <- exp(value) - exp(value - regression$error)
    sites$upper[i] <- exp(value) + exp(value + regression$error)
  }
  return(sites)


}
