# Make it so that you can have either raw data or a `dilp()` results table for the dilp_table parameter?
#' Test if site leaf physiognomy falls within the physiognomic space of the DiLP calibration dataset
#'
#' `dilp_cca` plots a canonical correspondence analysis (CCA) ordination of the leaf
#' physiognomic space represented in the calibration dataset of Peppe et al. (2011).
#' The fossil sites being tested are placed along the CCA axes.  If a fossil site
#' falls outside of the plotted calibration space, paleoclimate reconstructions
#' for that fossil site should be treated with caution.
#'
#' @param dilp_table The results of a call to [dilp()]
#' @param physiognomy_calibration A physiognomic calibration dataset. Defaults to
#' \code{\link[data]{physiognomyCalibration}}, the calibration dataset used by Peppe
#' et al. (2011)
#' @param climate_calibration A climate calibration dataset. Defaults to
#' \code{\link[data]{climateCalibration}}, the calibration dataset used by Peppe
#' et al. (2011)
#'
#' @return A ggplot2 plot
#' @export
#'
#' @examples
#' results <- dilp(McAbeeExample)
#' dilp_cca(results)
#'
dilp_cca <- function(dilp_table, physiognomy_calibration = physiognomyCalibration, climate_calibration = climateCalibration) {
  # sort by alphabetical order, to ensure sites line up between this and the climate dataframe
  physiognomy_calibration <- physiognomy_calibration[order(physiognomy_calibration$Site), ]
  colnames(physiognomy_calibration) <- gsub("\\.", " ", colnames(physiognomy_calibration))

  # remove the site column
  cca_leaf <- subset(physiognomy_calibration, select = -c(Site))

  # sort by alphabetical order, to ensure sites line up between this and the leaf trait dataframe
  climate_calibration <- climate_calibration[order(climate_calibration$Site), ]

  # remove the site column
  cca_climate <- subset(climate_calibration, select = -c(Site))

  ###### Preform the CCA, excluding the fossil site(s)
  cca_analysis <- vegan::cca(X = cca_leaf, Y = cca_climate, scale = "TRUE")

  ##### Bring in the fossil sites(s) passively as an unknown sample
  #### Create new data frame that has the fossil site(s)
  coln <- colnames(physiognomy_calibration)
  cca_fossil_site <- subset(dilp_table$processed_site_data, select = coln)
  cca_fossil <- subset(cca_fossil_site, select = -c(Site))

  ## Predict CCA1 and CCA2 scores from the existing model
  cca_analysis_fossil <- stats::predict(cca_analysis, type = "wa", newdata = cca_fossil)


  ######### Plot CCA
  #### Convert CCA1 and CC2 axes scores to a dataframe
  cca_df <- data.frame(vegan::scores(cca_analysis, display = "sites"))
  # designate as calibration data
  cca_df$data <- "calibration"
  # bring the site column back
  cca_df <- cbind(cca_df, subset(climate_calibration, select = ("Site")))

  #### Merge fossil data
  # Convert CCA predictions to dataframe
  cca_df_fossil <- as.data.frame(cca_analysis_fossil)
  # designate as fossil data
  cca_df_fossil$data <- "fossil"
  # bring the site column back
  cca_df_fossil <- cbind(cca_df_fossil, subset(dilp_table$processed_site_data, select = ("Site")))
  # merge the fossil and calibration CCA data
  cca_df_all <- rbind(cca_df, cca_df_fossil)

  #### Create the plot
  # Create the convex hull
  hull_data <- cca_df %>% dplyr::slice(grDevices::chull(CCA1, CCA2))

  # Plot
  cca_plot <- ggplot2::ggplot(data = cca_df_all, ggplot2::aes(x = CCA1, y = CCA2)) +
    ggplot2::geom_point(ggplot2::aes(color = data, shape = data), size = 4) +
    ggplot2::theme_classic() +
    ggplot2::geom_polygon(
      data = hull_data,
      ggplot2::aes(fill = data),
      alpha = 0.2,
      show.legend = FALSE
    ) +
    ggrepel::geom_label_repel(
      data = cca_df_fossil, ggplot2::aes(label = Site),
      box.padding = 0.35, point.padding = 0.5, segment.color = "grey50", max.overlaps = 50
    )
  return(cca_plot)
}
