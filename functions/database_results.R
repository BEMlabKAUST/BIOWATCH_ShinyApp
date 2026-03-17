compute_species_presence <- function(species_dataframe, cleaned_data_path, db_name) {
  
  cleaned_data   <- read.delim(cleaned_data_path, header = FALSE, sep = "\t")
  species_column <- cleaned_data[, 10]
  
  presence_df <- species_dataframe %>%
    dplyr::mutate(Present = canonicalName %in% species_column) %>%
    dplyr::group_by(SOI) %>%
    dplyr::summarise(Presence = ifelse(any(Present), "Yes", "No"))
  
  colnames(presence_df) <- c("Species", "Presence")
  
  write.csv(presence_df,
            file.path("data", paste0(db_name, "_species_presence.csv")),
            row.names = FALSE)
  
  percentage_present <- (sum(presence_df$Presence == "Yes") / nrow(presence_df)) * 100
  
  list(presence_df = presence_df, percentage_present = percentage_present)
}


plot_species_presence <- function(presence_df) {
  
  ggplot(as.data.frame(presence_df), 
         aes(x = "Presence", y = Species, fill = factor(Presence))) +
    geom_tile(colour = "grey80", linewidth = 1) +
    scale_fill_manual(values = c("No" = "#d2d2d2", "Yes" = "#716fa8"), name = "Presence") +
    theme_bw() +
    theme(
      axis.text.x       = element_blank(),
      axis.text.y       = element_text(size = 12),
      axis.title.y      = element_text(size = 14),
      legend.position   = "",
      panel.background  = element_rect(fill = "transparent", color = NA),
      plot.background   = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      plot.margin       = margin(0, 0, 0, 0),
      axis.title        = element_text(size = 18, colour = "white"),
      axis.text         = element_text(size = 16, colour = "white"),
      plot.title        = element_text(size = 20, colour = "white"),
      legend.text       = element_text(size = 16, colour = "white"),
      legend.title      = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor  = element_blank(),
      strip.background  = element_blank(),
      strip.text        = element_blank()
    ) +
    labs(x = "", y = "Species of Interest")
}