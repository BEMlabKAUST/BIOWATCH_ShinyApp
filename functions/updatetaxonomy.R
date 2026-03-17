check_taxonomy_files <- function(data_dir, max_age_days = 180) {
  
  files <- c(
    "nodes.dmp",
    "names.dmp",
    "nucl_gb.accession2taxid"
  )
  
  paths  <- file.path(data_dir, files)
  exists <- file.exists(paths)
  
  if (!all(exists)) {
    return(list(
      status  = "missing",
      missing = files[!exists]
    ))
  }
  
  file_ages <- sapply(paths, function(f) {
    as.numeric(Sys.time() - file.info(f)$mtime, units = "days")
  })
  
  if (any(file_ages > max_age_days)) {
    return(list(
      status = "old",
      ages   = round(file_ages, 1)
    ))
  }
  
  list(status = "ok")
}


download_taxonomy_async <- function(data_dir, crabs_path = "crabs") {
  future({
    cmd <- paste(
      shQuote(crabs_path),
      "--download-taxonomy",
      "--output",
      shQuote(data_dir)
    )
    system(cmd, intern = TRUE, wait = TRUE)
  })
}