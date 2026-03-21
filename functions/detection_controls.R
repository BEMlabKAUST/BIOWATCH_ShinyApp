compute_controls_summary <- function(asv_table_filtered) {
  
  #Filter dataset to the controls and identify if any of the species of interest were detected in controls
  controls_df <- asv_table_filtered %>%
    filter(Type == "Control") %>%
    group_by(ASV, sscinames) %>%
    summarise(
      Detected = any(Abundance > 0, na.rm = TRUE),
      .groups  = "drop"
    )
  
  #Summarise the control data with the number of ASVs that have detection in the controls compared to the number of ASVs found for that species.
  controls_summary <- controls_df %>%
    group_by(sscinames) %>%
    summarise(
      n_asvs           = n(),
      n_detected       = sum(Detected),
      asv_list         = paste(ASV, collapse = ", "),
      detection_status = case_when(
        n_detected == 0       ~ "Not detected",
        n_detected == n_asvs  ~ "All detected",
        TRUE                  ~ "Partially detected"
      ),
      .groups = "drop"
    )
  
  controls_summary %>%
    left_join(
      controls_df %>% select(sscinames, ASV, Detected),
      by           = "sscinames",
      relationship = "many-to-many"
    )
}

#Build species cards for the species of interest
build_control_species_card <- function(sp, asvs, index) {
  
  detection_status <- unique(asvs$detection_status)
  card_color       <- "#65BFD1"
  
  div(
    style = paste0(
      "background-color:", card_color, ";",
      "color: black;",
      "padding: 15px;",
      "border-radius: 12px;",
      "margin: 10px;",
      "display: inline-block;",
      "width: 300px;",
      "box-shadow: 0 4px 8px rgba(0,0,0,0.3);"
    ),
    h4(sp),
    p(paste0("ASVs: ", unique(asvs$n_asvs)),
      style = "font-weight: bold;"),
    p(paste0("Detected: ", unique(asvs$n_detected), "/", unique(asvs$n_asvs)),
      style = "font-size: 14px;"),
    plotOutput(paste0("control_heatmap_", index), height = "100px")
  )
}

#Create heatmap for detection of species of interest in controls
plot_control_heatmap <- function(asvs) {
  ggplot(asvs, aes(x = ASV, y = 1, fill = Detected)) +
    geom_tile(color = "grey50") +
    scale_fill_manual(
      values = c("TRUE"  = "#deabaf",
                 "FALSE" = "#d4edda"),
      labels = c("Not Detected", "Detected")
    ) +
    theme_minimal() +
    theme(
      axis.text.y      = element_blank(),
      axis.title       = element_blank(),
      panel.grid       = element_blank(),
      legend.position  = "none",
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background  = element_rect(fill = "transparent", colour = NA)
    )
}

#Prepare for dowload
prepare_controls_download <- function(controls_summary) {
  df <- controls_summary %>% select(sscinames, ASV, Detected)
  colnames(df) <- c("Species", "ASV", "Detected")
  df
}