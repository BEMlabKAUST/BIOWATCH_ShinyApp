download_bold <- function(crabs_path, taxa_vector, bold_marker) {
  
  #create a vector of the genera in the list
  genera_vector <- unique(sub(" .*", "", taxa_vector))
  
  print("Downloading BOLD database...")
  
  all_exec_output <- character()
  
  #Loop for each genera in genera vector
  for (i in seq_along(genera_vector)) {
    
    taxon <- genera_vector[i]
    print(paste("Downloading genus:", taxon))
    
    #bold download command
    bold_command <- paste(
      crabs_path,
      "--download-bold",
      "--taxon",  shQuote(taxon),
      "--output", shQuote(paste0("intermediate/bold_", taxon, ".tsv")),
      "--marker", shQuote(bold_marker)
    )
    
    bold_exec_output <- tryCatch({
      system(bold_command, intern = TRUE, wait = TRUE)
    }, error = function(e) {
      paste("Error executing command for genus", taxon, ":", e$message)
    })
    
    all_exec_output <- c(all_exec_output, bold_exec_output)
    
    if (i < length(genera_vector)) {
      Sys.sleep(runif(1, min = 3, max = 8))
    }
  }
  
  # Merge downloaded files
  all_bold_files <- list.files(path = "intermediate", 
                               pattern = "^bold_.*\\.tsv$", 
                               full.names = TRUE)
  
  if (length(all_bold_files) == 0) {
    print("No BOLD files downloaded")
    return(FALSE)
  }
  
  combined_data <- lapply(all_bold_files, function(file) {
    read.table(file, header = TRUE, sep = "\t", quote = "",
               fill = TRUE, stringsAsFactors = FALSE,
               colClasses = "character")
  }) %>% bind_rows()
  
  # Filter for species/genera in taxa_vector
  combined_data <- combined_data[
    combined_data$genus   %in% taxa_vector |
      combined_data$species %in% taxa_vector,
  ]
  
  #Write the combined bold data to a intermediate file
  write.table(combined_data, "intermediate/bold_combined.tsv", 
              sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Import the BOLD combined file into CRABS for futher processing
  system(paste(crabs_path,
               "--import --import-format BOLDV5",
               "--input intermediate/bold_combined.tsv",
               "--output intermediate/merged_bold_output.tax.tsv",
               "--acc2tax data/nucl_gb.accession2taxid",
               "--nodes data/nodes.dmp",
               "--names data/names.dmp",
               "--ranks 'superkingdom;phylum;class;order;family;genus;species'"))
  
  if (!file.exists("intermediate/merged_bold_output.tax.tsv")) {
    print("BOLD import failed")
    return(FALSE)
  }
  
  print(paste("BOLD download complete -", length(genera_vector), "genera processed"))
  return(TRUE)
}