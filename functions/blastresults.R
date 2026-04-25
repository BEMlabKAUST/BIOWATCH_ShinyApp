filter_blast_results <- function(blast_results, pident_range, length_range, species_path, bitscore_threshold_pct) {
  
  filtered <- blast_results
  
  # Filter by percent identity
  filtered <- filtered[!is.na(filtered$pident) &
                         filtered$pident >= pident_range[1] &
                         filtered$pident <= pident_range[2], ]
  
  # Filter by coverage
  filtered <- filtered[!is.na(filtered$qcovhsp) &
                         filtered$qcovhsp >= length_range[1] &
                         filtered$qcovhsp <= length_range[2], ]
  
  
  # Filter by bitscore threshold
  top_bitscore_per_asv <- filtered %>%
    group_by(qseqid) %>%
    summarise(max_bitscore = max(bitscore, na.rm = TRUE), .groups = "drop")
  
  filtered <- filtered %>%
    inner_join(top_bitscore_per_asv, by = "qseqid") %>%
    filter(bitscore >= max_bitscore * (1 - bitscore_threshold_pct / 100)) %>%
    select(-max_bitscore)
  
  # Load and join SOI list
  SOI_list.df <- fread(species_path, header = TRUE, sep = "\t") %>%
    distinct(SOI, canonicalName)
  
  filtered <- filtered %>%
    left_join(SOI_list.df, by = c("sscinames" = "canonicalName")) %>%
    filter(sscinames != "N/A") %>%
    mutate(SOI_flag = !is.na(SOI))
  
  return(filtered)
}


get_top_pident_results <- function(filtered) {
  
  # Find max pident per ASV
  top_pident_per_asv <- filtered %>%
    group_by(qseqid) %>%
    summarise(pident = max(pident, na.rm = TRUE))
  
  #Retain results where the result matches the top bitscore for each ASV
  merged_df <- filtered %>%
    inner_join(top_pident_per_asv, by = c("qseqid", "pident")) %>%
    select(qseqid, sscinames, SOI_flag, pident) %>%
    distinct()
  
  # Count how many species share the top pident per ASV
  asv_top_pident_counts <- merged_df %>%
    group_by(qseqid) %>%
    summarise(Top_pident_Count = n())
  
  # Flag if multiple species share the top pident
  merged_df %>%
    inner_join(asv_top_pident_counts, by = "qseqid") %>%
    mutate(Multiple_Instances = Top_pident_Count > 1) %>%
    select(-Top_pident_Count)
}


build_species_box <- function(species, soi_results) {
  
  # Get ASVs and their pident values for this species
  species_results    <- soi_results %>% filter(sscinames == species)
  multiple_instances <- species_results %>% pull(Multiple_Instances) %>% dplyr::first()
  asv_color          <- ifelse(multiple_instances, "#98C7B1", "#716fa8")
  
  # Build ASV labels with similarity percentage in brackets
  asv_labels <- species_results %>%
    select(qseqid, pident) %>%
    distinct() %>%
    mutate(label = paste0(qseqid, " (", round(pident, 1), "%)")) %>%
    pull(label)
  
  # Look up GBIF taxonomic key for species page link
  taxonKey <- tryCatch({
    key <- name_backbone(species)$usageKey
    if (!is.null(key)) key else NA
  }, error = function(e) NA)
  
  # Create clickable species name linking to GBIF
  species_link <- tags$a(
    href    = "#",
    onclick = sprintf("window.open('%s', '_blank')",
                      paste0("https://www.gbif.org/species/", taxonKey)),
    style   = "font-size: 18px; font-weight: bold; color: #b0dce9; text-decoration: underline; cursor: pointer;",
    species
  )
  
  # Create ASV list with similarity percentages
  asv_content <- paste0("<span style='color:", asv_color, ";'> ASVs: ",
                        paste(asv_labels, collapse = ", "), "</span>")
  
  box_content <- paste(species_link, "<br>", asv_content)
  
  div(
    style = "border: 1px solid #2171B5; padding: 10px; margin: 5px; background-color: #254550; width: 300px; display: inline-block; vertical-align: top;",
    HTML(box_content)
  )
}