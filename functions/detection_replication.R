compute_replicate_summary <- function(asv_table_filtered, abundance_threshold,
                                      group_by_var = "Site") {
  #Remvoe controls and group by Site or Region and identify the number of replicates where SOI were found compared to total replicates
  replicate_counts <- asv_table_filtered %>%
    filter(Type != "Control") %>%
    group_by(ASV, sscinames, !!sym(group_by_var)) %>%
    summarise(
      Replicates_Detected = n_distinct(SampleID[Abundance > abundance_threshold], na.rm = TRUE),
      Total_Replicates    = n_distinct(SampleID),
      .groups             = "drop"
    ) %>%
    #rename grouping column to Site to make things easier in subsequent commands
    rename(Site = !!sym(group_by_var)) %>%
    mutate(
      Detection_Pct = ifelse(Total_Replicates > 0,
                             Replicates_Detected / Total_Replicates, 0),
      #Set thresholds for the frequency of detection.
      Heat_Color = case_when(
        Replicates_Detected == 0 ~ "No Detection",
        Total_Replicates <= 2    ~ "Medium Detection",
        Detection_Pct >= 0.75    ~ "High Detection",
        Detection_Pct >= 0.50    ~ "Medium Detection",
        TRUE                     ~ "Low Detection"
      )
    )
}

#Prepare download of replicate data. 
prepare_replicate_download <- function(df) {
  df <- df %>%
    select(sscinames, ASV, Site, Replicates_Detected,
           Total_Replicates, Detection_Pct, Heat_Color)
  colnames(df) <- c("Species", "ASV", "Site", "Replicates_Detected",
                    "Total_Replicates", "Detection_Pct", "Detection")
  df
}

#Create heatmap coloured by the frequency of detection in replicates. 
plot_replicate_heatmap <- function(df_sp) {
  ggplot(df_sp, aes(x = Site, y = ASV, fill = Heat_Color)) +
    geom_tile(color = "grey70") +
    scale_fill_manual(
      values = c(
        "High Detection"   = "#d4edda",
        "Medium Detection" = "#FAC589",
        "Low Detection"    = "#deabaf",
        "No Detection"     = "transparent"
      ),
      name = ""
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x       = element_text(angle = 45, hjust = 1, size = 14, colour = "white"),
      axis.text.y       = element_text(size = 14, colour = "white"),
      plot.title     = element_text(size = 18, colour = "white"),
      panel.grid        = element_blank(),
      panel.background  = element_rect(fill = "transparent", color = NA),
      plot.background   = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.key        = element_rect(fill = "transparent", color = NA),
      legend.position   = ""
    ) +
    labs(x = "", y = "")
}


#Heatmap UI for one plot per species in rows of three. Change size of plot based on number of rows and columns.
build_replicate_heatmap_ui <- function(replicate_summary) {
  
  species_list <- unique(replicate_summary$sscinames)
  #split into groups of 3 to fit on page
  groups       <- split(seq_along(species_list), ceiling(seq_along(species_list) / 3))
  
  row_list <- lapply(groups, function(idx) {
    cols <- lapply(idx, function(i) {
      sp    <- species_list[i]
      df_sp <- replicate_summary %>% filter(sscinames == sp)
      #Scale plot based on number of rows and columns
      n_cols <- length(unique(df_sp$Site))
      n_rows <- length(unique(df_sp$ASV))
      height <- max(150, n_rows * 40)
      width  <- max(400, n_cols * 60)
      
      column(4,
             div(style = "height: 50px; display: flex; align-items: center;",
                 h4(sp, style = "margin: 0; color: white")),
             plotOutput(paste0("replicate_heatmap_", i),
                        height = paste0(height, "px"),
                        width  = paste0(width,  "px"))
      )
    })
    #PAd columns to make factor of 3
    while (length(cols) < 3) cols <- c(cols, list(column(4)))
    
    do.call(fluidRow, cols)
  })
  
  do.call(tagList, row_list)
}