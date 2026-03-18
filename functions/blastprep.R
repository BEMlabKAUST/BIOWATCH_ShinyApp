get_synonyms_gbif <- function(species_name) {
  
  bb <- tryCatch({
    name_backbone(name = species_name)
  }, error = function(e) NULL)
  
  if (is.null(bb) || is.null(bb$speciesKey)) {
    return(data.frame(
      SOI           = species_name,
      canonicalName = species_name,
      stringsAsFactors = FALSE
    ))
  }
  
  key  <- bb$speciesKey
  syns <- tryCatch({
    name_usage(key = key, data = "synonyms")$data
  }, error = function(e) NULL)
  
  if (is.null(syns) || nrow(syns) == 0) {
    return(data.frame(
      SOI           = species_name,
      canonicalName = species_name,
      stringsAsFactors = FALSE
    ))
  }
  
  canonical_list <- unique(c(
    species_name,
    syns$species,
    syns$canonicalName
  ))
  canonical_list <- canonical_list[!is.na(canonical_list) & canonical_list != ""]
  
  species_df <- data.frame(
    SOI           = rep(species_name, length(canonical_list)),
    canonicalName = canonical_list,
    stringsAsFactors = FALSE
  ) %>%
    tidyr::separate(canonicalName, c("Genus", "Species", "Other"),
                    sep = " ", fill = "right") %>%
    tidyr::unite("canonicalName", Genus, Species, Other,
                 sep = " ", na.rm = TRUE) %>%
    dplyr::select(SOI, canonicalName) %>%
    dplyr::distinct()
  
  return(species_df)
}


get_available_databases <- function(db_folder = "databases/") {
  db_files <- list.files(db_folder, pattern = "\\.nin$", full.names = FALSE)
  gsub("\\.nin$", "", db_files)
}