download_silva <- function(crabs_path, silva_marker, taxa_vector) {
  
  print("Downloading SILVA database...")
  
  system(paste(
    crabs_path,
    "--download-silva",
    "--output intermediate/silva_138.2_subset.fasta",
    "--gene", shQuote(silva_marker),
    "--db-type subset",
    "--db-version 138.2"
  ))
  
  if (!file.exists("intermediate/silva_138.2_subset.fasta")) {
    print("SILVA download failed")
    return(FALSE)
  }
  
  print("Importing SILVA data...")
  
  system(paste(crabs_path,
               "--import --import-format silva",
               "--input intermediate/silva_138.2_subset.fasta",
               "--output intermediate/silva_output.tax.tsv",
               "--acc2tax data/nucl_gb.accession2taxid",
               "--nodes data/nodes.dmp",
               "--names data/names.dmp",
               "--ranks 'superkingdom;phylum;class;order;family;genus;species'"))
  
  if (!file.exists("intermediate/silva_output.tax.tsv")) {
    print("SILVA import failed")
    return(FALSE)
  }
  
  silva_data <- read.table("intermediate/silva_output.tax.tsv", header = TRUE, sep = "\t",
                           quote = "", comment.char = "",
                           fill = TRUE, stringsAsFactors = FALSE,
                           colClasses = "character")
  
  silva_data <- silva_data[
    silva_data[, 9] %in% taxa_vector |
      silva_data[, 10] %in% taxa_vector,
  ]
  
  write.table(silva_data, "intermediate/merged_silva_output.tax.tsv", sep = "\t",
              quote = FALSE, row.names = FALSE, col.names = FALSE)
  
  print(paste("Filtered SILVA data:", nrow(silva_data), "sequences"))
  return(TRUE)
}