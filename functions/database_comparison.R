load_dbcomparison_data <- function(comparison_data_folder, prefix) {
  
  files <- list.files(
    comparison_data_folder,
    pattern = paste0("^", prefix, "_.*_species_presence\\.csv$"),
    full.names = TRUE
  )
  
  dfs <- lapply(files, function(f) {
    
    db_name <- basename(f) |>
      sub(paste0("^", prefix, "_"), "", x = _) |>
      sub("_species_presence\\.csv$", "", x = _)
    
    read.csv(f) |>
      dplyr::rename(!!db_name := Presence)
  })
  
  combined <- Reduce(function(x, y) dplyr::full_join(x, y, by = "Species"), dfs)
  combined[is.na(combined)] <- "No"
  
  return(combined)
}


plot_dbcomparison_heatmap <- function(df) {
  
  df_long <- df %>%
    pivot_longer(-Species, names_to = "Database", values_to = "Presence")
  
  ggplot(df_long, aes(x = Database, y = Species, fill = Presence)) +
    geom_tile(color = "white") +
    scale_fill_manual(values = c("No" = "#d2d2d2", "Yes" = "#716fa8"), name = "Presence") +
    theme_bw() +
    labs(x = "", y = "Species of Interest") +
    theme(legend.position = "") +
    theme(
      panel.background  = element_rect(fill = "transparent", color = NA),
      plot.background   = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      plot.margin       = margin(0, 0, 0, 0),
      axis.title        = element_text(size = 18, colour = "white"),
      axis.text.x       = element_text(size = 16, colour = "white", angle = 90, hjust = 1),
      axis.text.y       = element_text(size = 16, colour = "white"),
      plot.title        = element_text(size = 20, colour = "white"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor  = element_blank(),
      strip.background  = element_blank(),
      strip.text        = element_blank()
    )
}


dbcomparison_plot_height <- function(df) {
  max(300, nrow(df) * 30)
}

dbcomparison_plot_width <- function(df) {
  max(500, (ncol(df) - 1) * 120)
}


get_dbcomparison_prefixes <- function(comparison_data_folder) {
  
  files <- list.files(
    comparison_data_folder,
    pattern = "_species_presence\\.csv$"
  )
  
  prefixes <- unique(
    sub("_[^_]+_species_presence\\.csv$", "", files)
  )
  
  return(prefixes)
}
