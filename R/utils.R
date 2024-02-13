colnameClean <- function(data){
  names <- colnames(data) %>%
    stringr::str_trim() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[.]", " ") %>%
    stringr::str_replace_all("[ ]","_") %>%
    stringr::str_replace_all("__", "_")
  return(names)
}
