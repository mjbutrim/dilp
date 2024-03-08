colnameClean <- function(data) {
  names <- colnames(data) %>%
    stringr::str_trim() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[.]", " ") %>%
    stringr::str_replace_all("[ ]", "_") %>%
    stringr::str_replace_all("__", "_")
  names <- colnameSynonym(names, "petiole_metric", c("pwa", "pw^2/a"))
  names <- colnameSynonym(names, "leaf_area", c("la"))
  names <- colnameSynonym(names, "petiole_width", c("pw"))
  names <- colnameSynonym(names, "internal_raw_blade_area", c("raw_internal_blade_area"))
  names <- colnameSynonym(names, "internal_raw_blade_perimeter", c("raw_internal_blade_perimeter"))
  names <- colnameSynonym(names, "no_primary_teeth", c("no_of_primary_teeth", "number_primary_teeth", "number_of_primary_teeth", "#primary_teeth", "#_of_primary_teeth"))
  names <- colnameSynonym(names, "no_of_subsidiary_teeth", c(
    "no_subsidiary_teeth", "number_subsidiary_teeth", "number_of_subsidiary_teeth", "#subsidiary_teeth", "#_of_subsidiary_teeth",
    "no_secondary_teeth", "number_secondary_teeth", "number_of_secondary_teeth", "#secondary_teeth", "#_of_secondary_teeth",
    "no_of_secondary_teeth"
  ))
  names <- colnameSynonym(names, "feret", c("feret_diameter"))
  names <- colnameSynonym(names, "margin", c("Margin (0 - Toothed; 1 - Untoothed)"))
  return(names)
}

colnameSynonym <- function(data, name, synonyms) {
  for (i in 1:length(synonyms)) {
    index <- which(data == tolower(synonyms[i]))
    data[index] <- name
  }
  return(data)
}

grab_regression <- function(name, type) {
  if (type == "temp") {
    index <- which(names(temp_regressions) == name)
    if (length(index) == 0) {
      stop(paste("Regression ", name, "does not exist. Check for typos or provide parameters manually"))
    } else {
      return(temp_regressions[[index]])
    }
  }
  if (type == "precip") {
    index <- which(names(precip_regressions) == name)
    if (length(index) == 0) {
      stop(paste("Regression ", name, "does not exist. Check for typos or provide parameters manually"))
    } else {
      return(precip_regressions[[index]])
    }
  }
  if (type == "dilp") {
    index <- which(names(dilp_parameters) == name)
    if (length(index) == 0) {
      stop(paste("DiLP parameter set ", name, "does not exist. Check for typos or provide parameters manually"))
    } else {
      return(dilp_parameters[[index]])
    }
  }
}
