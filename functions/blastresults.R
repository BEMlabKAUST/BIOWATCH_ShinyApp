filter_blast_results <- function(blast_results, pident_range, length_range, species_path) {
  
  filtered <- blast_results
  
  # Filter by percent identity
  filtered <- filtered[!is.na(filtered$pident) &
                         filtered$pident >= pident_range[1] &
                         filtered$pident <= pident_range[2], ]
  
  # Filter by length
  filtered <- filtered[!is.na(filtered$length) &
                         filtered$length >= length_range[1] &
                         filtered$length <= length_range[2], ]
  
  # Load and join SOI list
  SOI_list.df <- fread(species_path, header = TRUE, sep = "\t") %>%
    distinct(SOI, canonicalName)
  
  filtered <- filtered %>%
    left_join(SOI_list.df, by = c("sscinames" = "canonicalName")) %>%
    filter(sscinames != "N/A") %>%
    mutate(SOI_flag = !is.na(SOI))
  
  return(filtered)
}


get_top_bitscore_results <- function(filtered) {
  
  top_bitscore_per_asv <- filtered %>%
    group_by(qseqid) %>%
    summarise(bitscore = max(bitscore, na.rm = TRUE))
  
  merged_df <- filtered %>%
    inner_join(top_bitscore_per_asv, by = c("qseqid", "bitscore")) %>%
    select(qseqid, sscinames, SOI_flag) %>%
    distinct()
  
  asv_top_bitscore_counts <- merged_df %>%
    group_by(qseqid) %>%
    summarise(Top_bitscore_Count = n())
  
  merged_df %>%
    inner_join(asv_top_bitscore_counts, by = "qseqid") %>%
    mutate(Multiple_Instances = Top_bitscore_Count > 1) %>%
    select(-Top_bitscore_Count)
}


build_species_box <- function(species, soi_results) {
  
  asvs               <- soi_results %>% filter(sscinames == species) %>% pull(qseqid) %>% unique()
  multiple_instances <- soi_results %>% filter(sscinames == species) %>% pull(Multiple_Instances) %>% dplyr::first()
  asv_color          <- ifelse(multiple_instances, "#98C7B1", "#716fa8")
  
  taxonKey <- tryCatch({
    key <- name_backbone(species)$usageKey
    if (!is.null(key)) key else NA
  }, error = function(e) NA)
  
  species_link <- tags$a(
    href    = "#",
    onclick = sprintf("window.open('%s', '_blank')",
                      paste0("https://www.gbif.org/species/", taxonKey)),
    style   = "font-size: 18px; font-weight: bold; color: #b0dce9; text-decoration: underline; cursor: pointer;",
    species
  )
  
  asv_content <- paste0("<span style='color:", asv_color, ";'> ASVs: ",
                        paste(asvs, collapse = ", "), "</span>")
  box_content <- paste(species_link, "<br>", asv_content)
  
  div(
    style = "border: 1px solid #2171B5; padding: 10px; margin: 5px; background-color: #254550; width: 300px; display: inline-block; vertical-align: top;",
    HTML(box_content)
  )
}