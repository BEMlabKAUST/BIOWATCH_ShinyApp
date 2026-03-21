# Read and validate the synonym file for the given database
load_synonym_file <- function(db_name) {
  synonym_file <- file.path("databases", paste0(db_name, "_synonyms.txt"))
  if (!file.exists(synonym_file)) {
    stop("Synonym file not found: ", synonym_file)
  }
  read.delim(synonym_file, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
}

# Extract species from the detailed presence table that are confirmed present
get_present_species <- function(species_presence_detailed) {
  detailed <- species_presence_detailed$detailed_df
  detailed$SOI[detailed$SOI_Present == "Yes"]
}

# Get all canonical names for the present SOI species from the synonyms dataframe
get_canonical_names <- function(present_species, synonyms_df) {
  synonyms_df$canonicalName[synonyms_df$SOI %in% present_species]
}

# Read the full merged database TSV and filter to canonical names only.
# Returns NULL if no matching sequences are found.
load_filtered_db <- function(canonical_names,
                             db_path = "intermediate/merged_combined_output.tax.cleaned.final.tsv",
                             species_col = 10) {
  if (!file.exists(db_path)) {
    stop("Merged database file not found: ", db_path)
  }
  full_db <- read.delim(db_path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
  filtered <- full_db[full_db[, species_col] %in% canonical_names, ]
  if (nrow(filtered) == 0) return(NULL)
  list(full_db = full_db, filtered_db = filtered)
}

# Map each sequence in filtered_db back to its parent SOI name using the synonym table.
# Adds an SOI column to filtered_db for downstream grouping.
add_soi_column <- function(filtered_db, synonyms_df, species_col = 10) {
  filtered_db$SOI <- sapply(filtered_db[, species_col], function(sp) {
    soi <- synonyms_df$SOI[synonyms_df$canonicalName == sp][1]
    if (is.na(soi)) sp else soi
  })
  filtered_db
}

# Write a temporary FASTA file from a filtered database dataframe.
# Returns the path to the temp file.
write_temp_fasta <- function(filtered_db, seqid_col = 1, seq_col = 11) {
  temp_fasta  <- tempfile(fileext = ".fasta")
  fasta_lines <- character(nrow(filtered_db) * 2)
  for (i in seq_len(nrow(filtered_db))) {
    fasta_lines[2 * i - 1] <- paste0(">", filtered_db[i, seqid_col])
    fasta_lines[2 * i]     <- filtered_db[i, seq_col]
  }
  writeLines(fasta_lines, temp_fasta)
  temp_fasta
}

# Build and execute the blastn command for 100% identity matching.
# Returns path to the BLAST output file, or stops on failure.
run_differentiation_blast <- function(temp_fasta, db_name) {
  blast_output <- tempfile(fileext = ".txt")
  blast_cmd <- paste(
    "blastn",
    "-query",          shQuote(temp_fasta),
    "-db",             shQuote(file.path("databases", paste0(db_name, ".blast.db"))),
    "-outfmt",         "'6 qseqid sseqid pident length qlen slen'",
    "-out",            shQuote(blast_output),
    "-max_target_seqs 100",
    "-perc_identity 100"
  )
  exit_code <- system(blast_cmd)
  if (exit_code != 0) {
    stop("BLAST command failed with exit code: ", exit_code)
  }
  blast_output
}

# Read and filter raw BLAST output:
# - Removes self-hits (query == subject)
# - Keeps only full-length 100% identity matches
parse_differentiation_blast <- function(blast_output) {
  if (!file.exists(blast_output) || file.info(blast_output)$size == 0) {
    return(NULL)
  }
  blast_results <- read.table(
    blast_output, sep = "\t", stringsAsFactors = FALSE,
    col.names = c("qseqid", "sseqid", "pident", "length", "qlen", "slen")
  )
  # Clean subject sequence IDs (strip pipe-delimited accession formatting)
  blast_results$sseqid_clean <- gsub("^[a-z]+\\|(.+)\\|$", "\\1", blast_results$sseqid)
  
  # Keep only non-self, full-length, 100% identity hits
  blast_results[
    blast_results$qseqid != blast_results$sseqid_clean &
      blast_results$pident == 100 &
      blast_results$length == blast_results$qlen &
      blast_results$length == blast_results$slen,
  ]
}

# For each present SOI species, summarise how many of its sequences have
# 100% identity matches to sequences from other species in the database.
compute_differentiation_results <- function(present_species, filtered_db, full_db,
                                            blast_results, synonyms_df,
                                            seqid_col = 1, species_col = 10) {
  diff_results <- lapply(present_species, function(soi) {
    
    # Get all sequences in the database belonging to this SOI
    soi_seqs <- filtered_db[filtered_db$SOI == soi, seqid_col]
    
    if (length(soi_seqs) == 0) {
      return(data.frame(
        Species              = soi,
        SoI_Sequences        = 0,
        Num_Matching_Species = 0,
        Matching_Species     = "",
        stringsAsFactors     = FALSE
      ))
    }
    
    # Find which SOI sequences have a 100% match to another sequence
    seqs_with_matches <- unique(blast_results$qseqid[blast_results$qseqid %in% soi_seqs])
    
    if (length(seqs_with_matches) > 0) {
      # Get the subject sequence IDs and map them back to species names
      matching_seqids   <- unique(blast_results$sseqid_clean[blast_results$qseqid %in% seqs_with_matches])
      matching_canonical <- unique(full_db[full_db[, seqid_col] %in% matching_seqids, species_col])
      
      # Map canonical names back to SOI, excluding self-matches
      matching_soi <- unique(sapply(matching_canonical, function(cn) {
        soi_match <- synonyms_df$SOI[synonyms_df$canonicalName == cn][1]
        if (is.na(soi_match)) cn else soi_match
      }))
      matching_soi <- matching_soi[matching_soi != soi]
    } else {
      matching_soi <- character(0)
    }
    
    data.frame(
      Species              = soi,
      SoI_Sequences        = length(soi_seqs),
      Num_Matching_Species = length(matching_soi),
      Matching_Species     = paste(matching_soi, collapse = "; "),
      stringsAsFactors     = FALSE
    )
  })
  
  do.call(rbind, diff_results)
}

# Save the differentiation results to the data folder as a CSV.
save_differentiation_results <- function(result, db_name) {
  out_path <- file.path("data", paste0(db_name, "_species_differentiation.csv"))
  write.csv(result, out_path, row.names = FALSE)
  out_path
}

# Master wrapper that orchestrates all differentiation steps.
# Returns the result dataframe or stops with an informative error message
# that will be caught by log_error() in the Shiny server.
run_species_differentiation <- function(species_presence_detailed, db_name,
                                        species_col = 10, seq_col = 11, seqid_col = 1) {
  
  synonyms_df     <- load_synonym_file(db_name)
  present_species <- get_present_species(species_presence_detailed)
  
  if (length(present_species) == 0) return(NULL)
  
  canonical_names <- get_canonical_names(present_species, synonyms_df)
  if (length(canonical_names) == 0) return(NULL)
  
  db_data <- load_filtered_db(canonical_names, species_col = species_col)
  if (is.null(db_data)) return(NULL)
  
  full_db     <- db_data$full_db
  filtered_db <- add_soi_column(db_data$filtered_db, synonyms_df, species_col)
  temp_fasta  <- write_temp_fasta(filtered_db, seqid_col = seqid_col, seq_col = seq_col)
  
  blast_output  <- run_differentiation_blast(temp_fasta, db_name)
  blast_results <- parse_differentiation_blast(blast_output)
  
  # If no BLAST hits at all, return a zeroed results table
  if (is.null(blast_results) || nrow(blast_results) == 0) {
    return(data.frame(
      Species              = present_species,
      SoI_Sequences        = 0,
      Num_Matching_Species = 0,
      Matching_Species     = "",
      stringsAsFactors     = FALSE
    ))
  }
  
  result <- compute_differentiation_results(
    present_species = present_species,
    filtered_db     = filtered_db,
    full_db         = full_db,
    blast_results   = blast_results,
    synonyms_df     = synonyms_df,
    seqid_col       = seqid_col,
    species_col     = species_col
  )
  
  save_differentiation_results(result, db_name)
  result
}