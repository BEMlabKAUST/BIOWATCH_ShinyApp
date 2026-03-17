import_custom_fasta <- function(crabs_path, custom_fasta_path) {
  
  print("Importing custom FASTA...")
  
  exit_code <- system(paste(crabs_path,
                            "--import --import-format BOLDV3",
                            "--input", shQuote(custom_fasta_path),
                            "--output intermediate/custom_fasta_output.tax.tsv",
                            "--acc2tax data/nucl_gb.accession2taxid",
                            "--nodes data/nodes.dmp",
                            "--names data/names.dmp",
                            "--ranks 'superkingdom;phylum;class;order;family;genus;species'"))
  
  if (!file.exists("intermediate/custom_fasta_output.tax.tsv")) {
    print("Custom FASTA import failed")
    return(FALSE)
  }
  
  print("Custom FASTA import succeeded")
  return(TRUE)
}