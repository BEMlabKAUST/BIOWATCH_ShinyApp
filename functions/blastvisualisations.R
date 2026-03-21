
#calculate the zoom level for the map dependent on the range of lat and long.
calculate_zoom_level <- function(lon_range, lat_range) {
  spread <- max(lon_range, lat_range)
  case_when(
    spread > 30 ~ 3,
    spread > 10 ~ 4,
    spread > 2  ~ 5,
    TRUE        ~ 6
  )
}

#Subset to species of interest, remvoe controls and pivot longer
prepare_soi_summary <- function(asv_table, filtered_results2, coordinates) {
  
  soi_results <- filtered_results2 %>%
    filter(SOI_flag == TRUE) %>%
    select(qseqid, sscinames)  
  
  ASV_list <- unique(soi_results$qseqid)
  
  asv_table %>%
    filter(ASV %in% ASV_list) %>%
    pivot_longer(-ASV, names_to = "SampleID", values_to = "Abundance") %>%
    left_join(soi_results,  by = c("ASV" = "qseqid")) %>%
    left_join(coordinates,  by = "SampleID") %>%
    filter(Type != "Control")
}

#Aggregate speices of interest by either site or region depending on the selection by the user. 
#Only include species that are present. 
#COunt the number of ASVs detected and scale the bubble appropriately
prepare_map_summary <- function(df, summarise_by) {
  
  df %>%
    filter(Abundance > 0) %>%
    distinct(!!sym(summarise_by), ASV, Latitude, Longitude) %>%
    group_by(across(all_of(summarise_by))) %>%
    summarise(
      n         = n(),
      Latitude  = mean(Latitude,  na.rm = TRUE),
      Longitude = mean(Longitude, na.rm = TRUE),
      .groups   = "drop"
    ) %>%
    mutate(
      radius     = scales::rescale(n, to = c(5, 30)),
      popup_text = paste0(
        summarise_by, ": ", .data[[summarise_by]],
        "<br># ASVs of species of interest: ", n
      )
    )
}

#Build a summay table showing the species of interest abundance and coordinates for mapping.
prepare_merged_data <- function(selected_asvs, coordinates, asv_table, 
                                max_proportion = NULL, min_proportion = NULL) {
  #Tag if ASVs are present and sum the abundance across all asvs
  asv_presence <- selected_asvs %>%
    pivot_longer(-ASV, names_to = "SampleID", values_to = "Abundance") %>%
    group_by(SampleID) %>%
    summarise(
      SOI_Present = sum(Abundance) > 0,
      Abundance   = sum(Abundance)
    )
  #Calculate the total community abundance across all ASVs
  total_community <- asv_table %>%
    pivot_longer(-ASV, names_to = "SampleID", values_to = "Abundance") %>%
    group_by(SampleID) %>%
    summarise(Total_Community = sum(Abundance))
  
  #join the results together and summarise the data at the site level
  result <- coordinates %>%
    left_join(asv_presence,    by = "SampleID") %>%
    left_join(total_community, by = "SampleID") %>%
    filter(Type != "Control") %>%
    group_by(Site, Latitude, Longitude) %>%
    summarise(
      SOI_Present     = sum(Abundance, na.rm = TRUE) > 0,
      Abundance       = sum(Abundance, na.rm = TRUE),
      Total_Community = sum(Total_Community, na.rm = TRUE),
      .groups         = "drop"
    ) %>%
    mutate(
      SOI_Present = ifelse(is.na(SOI_Present), FALSE, SOI_Present),
      Proportion  = ifelse(Total_Community > 0, Abundance / Total_Community, 0)
    )
  
  #set the scaling max and min for the size of the bubbles
  scale_max <- if (!is.null(max_proportion)) max_proportion else max(result$Proportion[result$Proportion > 0])
  scale_min <- if (!is.null(min_proportion)) min_proportion else min(result$Proportion[result$Proportion > 0])
  #create the scaling column
  result %>%
    mutate(
      radius_scaled = ifelse(
        Proportion == 0,
        0,
        scales::rescale(Proportion, to = c(4, 20), from = c(scale_min, scale_max))
      )
    )
}

#Subset the ASV table to the ASVs for a selected species for mapping.
get_asvs_for_species <- function(filtered_results2, asv_table, species_choice) {
  
  asvs_for_species <- filtered_results2 %>%
    filter(sscinames == species_choice) %>%
    pull(qseqid) %>%
    unique()
  
  asv_table %>%
    select(c("ASV", everything())) %>%
    filter(ASV %in% asvs_for_species)
}