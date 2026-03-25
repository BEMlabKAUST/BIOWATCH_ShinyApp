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

#check if there is already a gbif cache available for the taxa list and coordinates.
check_gbif_cache <- function(family_taxon_ids, wkt_polygon) {
  
  cache_dir <- file.path(getwd(), "gbif_cache")
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  
  cache_key       <- digest::digest(list(family_taxon_ids, wkt_polygon), algo = "md5")
  cache_file_path <- file.path(cache_dir, paste0(cache_key, ".rds"))
  #if exists and is less than 30 days old the read cache
  if (file.exists(cache_file_path)) {
    cache_age <- difftime(Sys.time(), file.info(cache_file_path)$mtime, units = "days")
    if (cache_age < 30) {
      return(list(
        use_cache = TRUE,
        data      = readRDS(cache_file_path),
        path      = cache_file_path
      ))
    }
  }
  
  return(list(
    use_cache = FALSE,
    data      = NULL,
    path      = cache_file_path
  ))
}

#submit a GBIF occurrence download request for the taxa and geographic coordinates
submit_gbif_download <- function(family_taxon_ids, wkt_polygon, user, pwd, email) {
  occ_download(
    pred_and(
      pred_in("taxonKey", family_taxon_ids),
      pred("geometry", wkt_polygon)
    ),
    format = "SIMPLE_CSV",
    user  = user,
    pwd   = pwd,
    email = email
  )
}

#Retrieve and read the GBIF occurrence data from the zip file
process_gbif_zip <- function(request_id) {
  
  zipfile      <- occ_download_get(request_id, path = "intermediate")
  zip_contents <- unzip(zipfile, list = TRUE)$Name
  occ_file     <- zip_contents[grepl("\\.csv$", zip_contents)][1]
  
  if (is.na(occ_file)) return(NULL)
  
  read.delim(unz(zipfile, occ_file), sep = "\t", stringsAsFactors = FALSE)
}

#Save GBIF cahce
save_gbif_cache <- function(occ, cache_file_path) {
  tryCatch({
    saveRDS(occ, cache_file_path)
    return(TRUE)
  }, error = function(e) {
    warning(paste("Cache save failed:", e$message))
    return(FALSE)
  })
}

#Get the synonyms from gbif and write table
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

#Build the taxa vector for download of sequences.
build_taxa_vector <- function(genera, gbif_occ, synonyms_df) {
  
  gbif_species <- if (!is.null(gbif_occ) && "species" %in% colnames(gbif_occ)) {
    unique(gbif_occ$species[!is.na(gbif_occ$species) & nzchar(gbif_occ$species)])
  } else {
    character(0)
  }
  
  unique(c(genera, gbif_species, synonyms_df$canonicalName))
}

