download_ncbi <- function(crabs_path, taxa_vector, final_query_str, ncbi_email) {
  
  print("Downloading NCBI database...")
  
  chunk_size <- 10
  chunks <- split(taxa_vector, ceiling(seq_along(taxa_vector) / chunk_size))
  
  for (i in seq_along(chunks)) {
    taxa_chunk <- chunks[[i]]
    
    # For each taxon, apply it to each OR-separated part of the gene query
    taxa_with_genes <- lapply(taxa_chunk, function(taxon) {
      gene_parts <- strsplit(final_query_str, "\\s+OR\\s+", perl = TRUE)[[1]]
      
      gene_with_taxon <- lapply(gene_parts, function(gene_part) {
        paste0(taxon, "[ORGN] AND (", trimws(gene_part), ")")
      })
      
      paste(gene_with_taxon, collapse = " OR ")
    })
    
    final_chunk_query <- paste0("(", paste(taxa_with_genes, collapse = " OR "), 
                                ") AND (200 [SLEN] :50000[SLEN])")
    
    cat("\n=== NCBI Query Debug ===\n")
    cat("Constructed Query:\n", final_query_str, "\n\n")
    cat("Actual Executed Query:\n", final_chunk_query, "\n")
    cat("========================\n\n")
    
    ncbi_command <- paste(crabs_path,
                          "--download-ncbi",
                          "--database nucleotide",
                          "--query", shQuote(final_chunk_query),
                          "--output", paste0("intermediate/ncbi_", i, ".fasta"),
                          "--email", ncbi_email,
                          "--batchsize 5000")
    system(ncbi_command, wait = TRUE)
  }
  
  system("cat intermediate/ncbi_*.fasta > intermediate/merged_ncbi_output.fasta")
  
  system("awk '{if (/>.*/) {print} else { sub(/^N*/, \"\"); sub(/N*$/, \"\"); print}}' intermediate/merged_ncbi_output.fasta > intermediate/merged_ncbi_output_cleaned.fasta")
  
  system(paste(crabs_path,
               "--import --import-format ncbi",
               "--input intermediate/merged_ncbi_output_cleaned.fasta",
               "--output intermediate/merged_ncbi_output.tax.tsv",
               "--acc2tax data/nucl_gb.accession2taxid",
               "--nodes data/nodes.dmp",
               "--names data/names.dmp",
               "--ranks 'superkingdom;phylum;class;order;family;genus;species'"))
  
  if (!file.exists("intermediate/merged_ncbi_output.tax.tsv")) {
    print("NCBI download failed")
    return(FALSE)
  }
  
  print(paste("NCBI download complete -", length(chunks), "chunks processed"))
  return(TRUE)
}