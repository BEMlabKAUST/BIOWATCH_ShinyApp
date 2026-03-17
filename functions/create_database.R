
create_database <- function(crabs_path, selected_sources, custom_fasta, 
                            forward_primer, reverse_primer, db_name) {

###################     
####MERGE FILES####
###################  

combined_output <- "intermediate/merged_combined_output.tax.tsv"

# Build list of files to merge based on selected sources
files_to_merge <- character()

if ("ncbi" %in% selected_sources && file.exists("intermediate/merged_ncbi_output.tax.tsv")) {
  files_to_merge <- c(files_to_merge, "intermediate/merged_ncbi_output.tax.tsv")
}

if ("bold" %in% selected_sources && file.exists("intermediate/merged_bold_output.tax.tsv")) {
  files_to_merge <- c(files_to_merge, "intermediate/merged_bold_output.tax.tsv")
}

if ("midori" %in% selected_sources && file.exists("intermediate/merged_midori_output.tax.tsv")) {
  files_to_merge <- c(files_to_merge, "intermediate/merged_midori_output.tax.tsv")
}

if ("silva" %in% selected_sources && file.exists("intermediate/merged_silva_output.tax.tsv")) {
  files_to_merge <- c(files_to_merge, "intermediate/merged_silva_output.tax.tsv")
}

if (!is.null(custom_fasta) && file.exists("intermediate/custom_fasta_output.tax.tsv")) {
  files_to_merge <- c(files_to_merge, "intermediate/custom_fasta_output.tax.tsv")
}

if (length(files_to_merge) > 1) {
  # Multiple files - merge them
  system(paste(crabs_path,
               "--merge --input", shQuote(paste(files_to_merge, collapse = ";")),
               "--uniq --output", combined_output))
} else if (length(files_to_merge) == 1) {
  # Single file - just use it directly
  combined_output <- files_to_merge[1]
} else {
  stop("No data sources selected or available")
}

###########################      
####DATABASE PROCESSING####      
###########################

#Dereplicate sequences and clean output
system(paste(crabs_path, "--dereplicate --input", combined_output, "--output intermediate/merged_combined_output.filtered.tsv --dereplication-method \'unique_species\'"))

#Sort out naming issues
system("awk -F'\t' 'NF>=1 && length($NF)>0' intermediate/merged_combined_output.filtered.tsv > intermediate/merged_combined_output.filtered.cleaned.tsv")
system("awk -F'\t' 'NF>=1 && length($NF)>0 {gsub(/ /,\"-\",$1); print}' OFS='\t' intermediate/merged_combined_output.filtered.cleaned.tsv > intermediate/merged_combined_output.filtered.cleaned.final.tsv")

# Insilico PCR using selected primers
system(paste(crabs_path, "--in-silico-pcr --input intermediate/merged_combined_output.filtered.cleaned.final.tsv --output intermediate/insilico_relaxed.txt --forward", shQuote(forward_primer), "--reverse", shQuote(reverse_primer), "--relaxed"))

#Pairwise global alignment to find any sequences with no primers but correct region
system(paste(crabs_path, "--pairwise-global-alignment --input intermediate/merged_combined_output.filtered.cleaned.final.tsv --amplicons intermediate/insilico_relaxed.txt --output intermediate/insilico_aligned.txt --forward", shQuote(forward_primer), "--reverse", shQuote(reverse_primer), "--size-select 10000 --percent-identity 0.95 --coverage 95"))

#Further dereplication after insilico PCR
system(paste(crabs_path, "--dereplicate --input intermediate/insilico_aligned.txt --output intermediate/merged_combined_output.tax.cleaned.tsv --dereplication-method \'unique_species\'"))

#Filtering of results - possibly requires minimum length to be changed. 
system(paste(crabs_path, "--filter", 
             "--input intermediate/merged_combined_output.tax.cleaned.tsv", 
             "--output intermediate/merged_combined_output.tax.cleaned.final.tsv", 
             "--minimum-length 50",
             "--maximum-n 5",
             "--environmental",
             "--no-species-id",
             "--rank-na 2"))

#Sort out issues with Sequence ID names to enable BLAST database to be constructed. 
dt <- fread("intermediate/merged_combined_output.tax.cleaned.final.tsv", sep = "\t")

dt[[1]] <- gsub(":", "-", dt[[1]])

# Case-insensitive key
key <- tolower(dt[[1]])

# Count duplicates by lowercase key
dup_index <- ave(seq_along(key), key, FUN = function(x) seq_along(x) - 1)

# Build new names
dt[[1]] <- ifelse(dup_index == 0,
                  dt[[1]],
                  paste0(dt[[1]], "_", dup_index))

fwrite(dt,
       "intermediate/merged_combined_output.tax.cleaned.final.tsv",
       sep = "\t",
       quote = FALSE,
       col.names = FALSE)

#Export into fasta format
system(paste(crabs_path, "--export --input intermediate/merged_combined_output.tax.cleaned.final.tsv --output intermediate/combined.db.fasta --export-format rdp"))
system(paste("awk '/^>/ {print $1; next}1'", shQuote("intermediate/combined.db.fasta"), ">", shQuote(file.path("databases", paste0(db_name, ".fasta")))))

#Create taxonomy file
system("awk -F\"\\t\" '{ print $1,$3 }' intermediate/merged_combined_output.tax.cleaned.final.tsv > intermediate/taxonomy.dictionary.combined.v1.txt")
system(paste("grep -v 'seqID'", shQuote("intermediate/taxonomy.dictionary.combined.v1.txt"), ">", shQuote(file.path("databases", paste0(db_name, ".txt")))))

#Make BLAST database
system(paste(
  "makeblastdb",
  "-in", shQuote(file.path("databases", paste0(db_name, ".fasta"))),
  "-parse_seqids",
  "-blastdb_version", "5",
  "-dbtype", "nucl",
  "-taxid_map", shQuote(file.path("databases", paste0(db_name, ".txt"))),
  "-title", shQuote(file.path("databases", paste0(db_name, ".blast.db"))),
  "-out", shQuote(file.path("databases", paste0(db_name, ".blast.db")))
))

print(paste("Database", db_name, "created successfully"))
return(TRUE)
}
