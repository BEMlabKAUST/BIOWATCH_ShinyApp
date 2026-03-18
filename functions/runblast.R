build_blast_command <- function(query_path, db_path, output_file) {
  paste(
    "blastn",
    "-query", query_path,
    "-db", db_path,
    "-out", output_file,
    "-max_target_seqs 5",
    "-num_threads 10",
    "-qcov_hsp_perc 80",
    "-outfmt", shQuote("6 qseqid sseqid staxids pident length mismatch gapopen qstart qend sstart send evalue bitscore")
  )
}


run_blast <- function(blast_command, output_file) {
  
  system(blast_command, intern = TRUE)
  
  blast_results.df <- fread(output_file,
                            header = FALSE,
                            sep    = "\t",
                            quote  = "")
  
  colnames(blast_results.df) <- c("qseqid", "sseqid", "staxids",
                                  "pident", "length", "mismatch", "gapopen",
                                  "qstart", "qend", "sstart", "send",
                                  "evalue", "bitscore")
  
  return(blast_results.df)
}


merge_blast_taxonomy <- function(blast_results.df, names_dmp_path = "data/names.dmp") {
  
  names_dmp     <- read.table(names_dmp_path, sep = "|", quote = "", 
                              fill = TRUE, stringsAsFactors = FALSE)
  names_dmp     <- names_dmp[names_dmp$V4 == "\tscientific name\t", ]
  names_dmp$V1  <- as.numeric(trimws(names_dmp$V1))
  names_dmp$V2  <- trimws(names_dmp$V2)
  
  blast_results.df <- merge(blast_results.df, names_dmp[, c(1, 2)],
                            by.x = "staxids", by.y = "V1", all.x = TRUE)
  names(blast_results.df)[ncol(blast_results.df)] <- "sscinames"
  
  # Clean quotes from character columns
  char_cols <- names(blast_results.df)[sapply(blast_results.df, is.character)]
  blast_results.df[, (char_cols) := lapply(.SD, function(x) {
    gsub('"', '', x, fixed = TRUE)
  }), .SDcols = char_cols]
  
  return(blast_results.df)
}