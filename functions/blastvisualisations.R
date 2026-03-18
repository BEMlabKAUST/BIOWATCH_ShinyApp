prepare_heatmap_data <- function(asv_table, filtered_results2, coordinates, summarise_by) {
  
  soi_results <- filtered_results2 %>%
    filter(SOI_flag == TRUE) %>%
    select(qseqid, sscinames)
  
  ASV_list <- unique(soi_results$qseqid)
  
  filtered_asv_table_heatmap <- asv_table %>% filter(ASV %in% ASV_list)
  
  df_long <- filtered_asv_table_heatmap %>%
    pivot_longer(-ASV, names_to = "SampleID", values_to = "Abundance") %>%
    left_join(soi_results, by = c("ASV" = "qseqid")) %>%
    left_join(coordinates, by = "SampleID")
  
  df_long %>%
    group_by(sscinames, .data[[summarise_by]]) %>%
    summarise(Abundance = sum(Abundance, na.rm = TRUE), .groups = "drop") %>%
    mutate(PresenceAbsence = as.integer(Abundance > 0))
}


plot_soi_heatmap <- function(heatmap_df, summarise_by) {
  ggplot(heatmap_df,
         aes(x = .data[[summarise_by]], y = sscinames,
             fill = factor(PresenceAbsence))) +
    geom_tile(color = "grey80", linewidth = 0.5) +
    scale_fill_manual(
      values = c("0" = "#d2d2d2", "1" = "#716fa8"),
      name   = "Presence",
      labels = c("Absent", "Present")
    ) +
    theme_bw() +
    theme(
      axis.text.x       = element_text(size = 14, angle = 90, vjust = 0.5, hjust = 1, colour = "white"),
      axis.text.y       = element_text(size = 14, face = "italic", colour = "white"),
      axis.title        = element_text(size = 18, colour = "white"),
      panel.background  = element_rect(fill = "transparent", color = NA),
      plot.background   = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.text       = element_text(size = 16, colour = "white"),
      legend.title      = element_text(size = 16, colour = "white"),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      panel.border      = element_blank()
    ) +
    labs(x = "", y = "Species of Interest")
}


calculate_heatmap_dimensions <- function(asv_table, soi_results, coordinates, summarise_by) {
  
  ASV_list <- unique(soi_results$qseqid)
  
  df <- asv_table %>%
    filter(ASV %in% ASV_list) %>%
    pivot_longer(-ASV, names_to = "SampleID", values_to = "Abundance") %>%
    left_join(soi_results, by = c("ASV" = "qseqid")) %>%
    left_join(coordinates, by = "SampleID")
  
  group_count   <- df %>% pull(!!sym(summarise_by)) %>% unique() %>% length()
  species_count <- length(unique(soi_results$sscinames))
  
  list(
    width  = as.numeric(min(max(group_count   * 120, 400), 10000)),
    height = as.numeric(min(max(species_count *  50, 400), 10000))
  )
}


prepare_heatmap_download <- function(asv_table, filtered_results2) {
  
  soi_results <- filtered_results2 %>%
    filter(SOI_flag == TRUE) %>%
    select(qseqid, sscinames)
  
  ASV_list <- unique(soi_results$qseqid)
  
  filtered_asv_table_heatmap <- asv_table %>% filter(ASV %in% ASV_list)
  
  pa_long <- filtered_asv_table_heatmap %>%
    pivot_longer(-ASV, names_to = "SampleID", values_to = "Abundance") %>%
    left_join(soi_results, by = c("ASV" = "qseqid")) %>%
    group_by(sscinames, SampleID) %>%
    summarise(Abundance = sum(Abundance), .groups = "drop") %>%
    mutate(PresenceAbsence = ifelse(Abundance > 0, "Present", "Absent"))
  
  pa_long %>%
    select(sscinames, SampleID, PresenceAbsence) %>%
    pivot_wider(names_from = SampleID, values_from = PresenceAbsence,
                values_fill = "Absent")
}


calculate_zoom_level <- function(lon_range, lat_range) {
  spread <- max(lon_range, lat_range)
  case_when(
    spread > 30 ~ 3,
    spread > 10 ~ 4,
    spread > 2  ~ 5,
    TRUE        ~ 6
  )
}


prepare_soi_summary <- function(asv_table, filtered_results2, coordinates) {
  
  soi_results <- filtered_results2 %>%
    filter(SOI_flag == TRUE) %>%
    select(qseqid, sscinames)
  
  ASV_list <- unique(soi_results$qseqid)
  
  asv_table %>%
    filter(ASV %in% ASV_list) %>%
    pivot_longer(-ASV, names_to = "SampleID", values_to = "Abundance") %>%
    left_join(soi_results,  by = c("ASV" = "qseqid")) %>%
    left_join(coordinates,  by = "SampleID")
}


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


prepare_merged_data <- function(selected_asvs, coordinates, asv_table, 
                                max_proportion = NULL, min_proportion = NULL) {
  
  asv_presence <- selected_asvs %>%
    pivot_longer(-ASV, names_to = "SampleID", values_to = "Abundance") %>%
    group_by(SampleID) %>%
    summarise(
      SOI_Present = sum(Abundance) > 0,
      Abundance   = sum(Abundance)
    )
  
  total_community <- asv_table %>%
    pivot_longer(-ASV, names_to = "SampleID", values_to = "Abundance") %>%
    group_by(SampleID) %>%
    summarise(Total_Community = sum(Abundance))
  
  result <- coordinates %>%
    left_join(asv_presence,    by = "SampleID") %>%
    left_join(total_community, by = "SampleID") %>%
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
  
  scale_max <- if (!is.null(max_proportion)) max_proportion else max(result$Proportion[result$Proportion > 0])
  scale_min <- if (!is.null(min_proportion)) min_proportion else min(result$Proportion[result$Proportion > 0])
  
  result %>%
    mutate(
      radius_scaled = ifelse(
        Proportion == 0,
        0,
        scales::rescale(Proportion, to = c(4, 20), from = c(scale_min, scale_max))
      )
    )
}


get_asvs_for_species <- function(filtered_results2, asv_table, species_choice) {
  
  asvs_for_species <- filtered_results2 %>%
    filter(sscinames == species_choice) %>%
    pull(qseqid) %>%
    unique()
  
  asv_table %>%
    select(c("ASV", everything())) %>%
    filter(ASV %in% asvs_for_species)
}