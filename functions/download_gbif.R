get_family_taxon_ids <- function(species_list) {
  
  family_taxon_ids <- unique(na.omit(sapply(species_list, function(species) {
    result <- name_backbone(name = species)
    if (!is.null(result$familyKey)) result$familyKey else NA
  })))
  
  # Special case adjustment
  if (any(family_taxon_ids %in% c(7336, 7334))) {
    family_taxon_ids <- unique(c(family_taxon_ids, 8596, 8597))
  }
  
  return(family_taxon_ids)
}


check_gbif_cache <- function(family_taxon_ids, wkt_polygon) {
  
  cache_dir <- file.path(getwd(), "gbif_cache")
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  
  cache_key       <- digest::digest(list(family_taxon_ids, wkt_polygon), algo = "md5")
  cache_file_path <- file.path(cache_dir, paste0(cache_key, ".rds"))
  
  if (file.exists(cache_file_path)) {
    cache_age <- difftime(Sys.time(), file.info(cache_file_path)$mtime, units = "days")
    if (cache_age < 30) {
      return(list(use_cache = TRUE, 
                  data = readRDS(cache_file_path),
                  path = cache_file_path))
    }
  }
  
  return(list(use_cache = FALSE, 
              data = NULL, 
              path = cache_file_path))
}


submit_gbif_download <- function(family_taxon_ids, wkt_polygon, 
                                 gbif_user, gbif_pwd, gbif_email) {
  
  req_download <- occ_download(
    pred_and(pred_in("taxonKey", family_taxon_ids),
             pred("geometry", wkt_polygon)),
    format = "SIMPLE_CSV",
    user  = gbif_user,
    pwd   = gbif_pwd,
    email = gbif_email
  )
  
  return(req_download)
}


process_gbif_zip <- function(zipfile) {
  
  zipfile <- occ_download_get(zipfile_id, 
                              path = "intermediate")
  
  zip_contents <- unzip(zipfile, list = TRUE)$Name
  occ_file     <- zip_contents[grepl("\\.csv$", zip_contents)][1]
  
  if (is.na(occ_file)) return(NULL)
  
  occ <- read.delim(unz(zipfile, occ_file), sep = "\t", stringsAsFactors = FALSE)
  return(occ)
}


save_gbif_cache <- function(occ, cache_file_path) {
  tryCatch({
    saveRDS(occ, cache_file_path)
    return(TRUE)
  }, error = function(e) {
    warning(paste("Cache save failed:", e$message))
    return(FALSE)
  })
}


get_synonyms <- function(species_list, db_name, get_synonyms_gbif) {
  
  print("Getting synonyms from GBIF...")
  
  df <- do.call(rbind, lapply(species_list, function(sp) {
    Sys.sleep(0.2)
    get_synonyms_gbif(sp)
  }))
  
  write.table(df,
              paste0("databases/", db_name, "_synonyms.txt"),
              sep = "\t", row.names = FALSE, quote = FALSE)
  
  print("Synonyms complete")
  return(df)
}