download_silva <- function(crabs_path, silva_marker, taxa_vector) {
  
  print("Downloading SILVA database...")
  
  #Download the SILVA (version 138.2) database using crabs....
  system(paste(
    crabs_path,
    "--download-silva",
    "--output intermediate/silva_138.2_subset.fasta",
    "--gene", shQuote(silva_marker),
    "--db-type subset",
    "--db-version 138.2"
  ))
  
  # Check if download succeeded
  if (file.exists("intermediate/silva_138.2_subset.fasta")) {
    
    print("Importing SILVA data...")
    
    # Run CRABS import for the SILVA import
    system(paste(crabs_path,
                 "--import --import-format silva",
                 "--input intermediate/silva_138.2_subset.fasta",
                 "--output intermediate/silva_output.tax.tsv",
                 "--acc2tax data/nucl_gb.accession2taxid",
                 "--nodes data/nodes.dmp",
                 "--names data/names.dmp",
                 "--ranks 'superkingdom;phylum;class;order;family;genus;species'"))
    
    # Filter the imported SILVA file for species in the list
    if (file.exists("intermediate/silva_output.tax.tsv")) {
      silva_data <- read.table("intermediate/silva_output.tax.tsv", header = TRUE, sep = "\t", 
                               quote = "", comment.char = "",
                               fill = TRUE, stringsAsFactors = FALSE, 
                               colClasses = "character")
      
      silva_data <- silva_data[
        silva_data[, 9] %in% taxa_vector | 
          silva_data[, 10] %in% taxa_vector,
      ]
      
      #output the SILVA table to the intermediate folder
      write.table(silva_data, "intermediate/merged_silva_output.tax.tsv", sep = "\t", 
                  quote = FALSE, row.names = FALSE, col.names = FALSE)
      
      print(paste("Filtered SILVA data:", nrow(silva_data), "sequences"))
    }
  } else {
    print("SILVA download failed")
  }
}