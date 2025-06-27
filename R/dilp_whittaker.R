base_whittaker <- function(){
  biomes <- Whittaker_biomes
  Whittaker_biomes$biomeorder <- 1:nrow(Whittaker_biomes)
  biome<- c("Tundra",
                    "Boreal forest",
                    "Temperate seasonal forest",
                    "Temperate rain forest",
                    "Tropical rain forest",
                    "Tropical seasonal forest/savanna",
                    "Subtropical desert",
                    "Temperate grassland/desert",
                    "Woodland/shrubland")
  order <- c("1", "2", "3", "4", "5", "6", "7", "8", "9")

  colors <- c("#C1E1DD", "#A5C790", "#97B669", "#75A95E", "#317A22", "#A09700", "#DCBB50", "#FCD57A", "#D16E3F")

  all_biomes <- data.frame(biome, order)
  all_biomes <- merge(all_biomes, Whittaker_biomes)
  all_biomes <- all_biomes[order(all_biomes$order, all_biomes$biomeorder),]
  all_biomes$biome <- factor(all_biomes$biome, levels = biome)
  plot <- ggplot2::ggplot() + ggplot2::labs(x = paste0("Mean Annual Temperature (", "\u00B0", "C)"),
                                            y = "Mean Annual Precipitation (cm/yr)") +
    ggplot2::geom_polygon(data = all_biomes, ggplot2::aes(x = .data$temp_c, y = .data$precp_cm, fill = biome),
                          color = "black", linewidth = 0.5) +
    ggplot2::scale_fill_manual(values = colors, name = "Biome") +
    ggplot2::theme_bw()
  return(plot)
}

#' Plot climate reconstructions on a Whittaker Biome plot
#'
#' @param climate_data A data frame containing the cliamte results from a `dilp()` call, or
#' the base output of `dilp()`.  Can also be a data frame with the following columns:
#' * site
#' * MAT.MLR
#' * MAT.MLR.error
#' * MAP.MLR
#' * MAP.MLR.error.minus
#' * MAP.MLR.error.plus
#'
#'
#' @returns A ggplot with dilp climate-reconstructed sites plotted onto a Whittaker diagram.
#' @export
#'
#' @examples
#' results <- dilp(McAbeeExample)
#' dilp_whittaker(results)
#'
dilp_whittaker <- function(climate_data){
  if (is.list(climate_data)) {
    if(!is.null(climate_data$results)) {
      climate_data <- climate_data$results
    }
  }
  base_plot <- base_whittaker()
  labels <- 1:nrow(climate_data)

  label_caption <- paste0(labels, ": ", climate_data$site)
  caption_text <- paste(label_caption, collapse = ", ")


  final_plot <- base_plot +
    ggplot2::geom_errorbar(data = climate_data,
                           ggplot2::aes(xmin = .data$MAT.MLR - .data$MAT.MLR.error,
                                        xmax = .data$MAT.MLR + .data$MAT.MLR.error,
                                        y = .data$MAP.MLR)) +
    ggplot2::geom_errorbar(data = climate_data,
                            ggplot2::aes(ymin = .data$MAP.MLR - .data$MAP.MLR.error.minus,
                                         ymax = .data$MAP.MLR + .data$MAP.MLR.error.plus,
                                         x = .data$MAT.MLR)) +
    ggplot2::geom_label(data = climate_data,
                        ggplot2::aes(x = .data$MAT.MLR, y = .data$MAP.MLR, group = .data$site, label = labels),
                        size = 5) +
    ggplot2::labs(title = caption_text) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 12))

  return(final_plot)

}
