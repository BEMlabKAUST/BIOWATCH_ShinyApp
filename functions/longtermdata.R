filter_historic_data <- function(data, species, gene, year_range, samplemethod) {
  
  if ("Presence" %in% colnames(data)) {
    result <- data %>%
      filter(
        Species == species,
        as.numeric(Year) >= year_range[1],
        as.numeric(Year) <= year_range[2],
        if (samplemethod != "All") Methodology == samplemethod else TRUE
      )
    return(result)
  }
  #filter based on year slider
  data %>%
    filter(
      as.numeric(Year) >= year_range[1],
      as.numeric(Year) <= year_range[2],
      if (samplemethod != "All") Methodology == samplemethod else TRUE
    )
}

#select the data based on species, gene and sampling method. 
build_historic_ui_selects <- function(data) {
  
  if (nrow(data) == 0) {
    return(list(
      species    = selectInput("species",      "Select Species:",         choices = c("No data available" = "")),
      gene       = selectInput("gene",         "Select Gene:",            choices = c("No data available" = "")),
      samplemethod = selectInput("samplemethod", "Select Sampling Method:", choices = c("No data available" = ""))
    ))
  }
  
  list(
    species = selectInput(
      inputId  = "species",
      label    = "Select Species:",
      choices  = c("Select species" = "", unique(data$Species)),
      selected = ""
    ),
    gene = selectInput(
      inputId  = "gene",
      label    = "Select Gene:",
      choices  = c("Select Gene" = "", unique(data$Gene)),
      selected = ""
    ),
    samplemethod = selectInput(
      inputId  = "samplemethod",
      label    = "Select Sampling Method:",
      choices  = c("Select Method" = "", "All", unique(data$Methodology)),
      selected = ""
    )
  )
}

#Create a year slider based on the data.
build_historic_year_slider <- function(data) {
  
  if (nrow(data) == 0 || !"Year" %in% colnames(data)) {
    return(p("No year data available", style = "color: white;"))
  }
  
  min_year <- min(data$Year, na.rm = TRUE)
  max_year <- max(data$Year, na.rm = TRUE)
  
  sliderInput(
    "year_range",
    "Select Year Range:",
    min   = min_year,
    max   = max_year,
    value = c(min_year, max_year),
    step  = 1,
    sep   = ""
  )
}

#calculate the level of zoom for the map based on the spread of Lat/Long. Smaller spread = greater zoom
calculate_zoom_level <- function(filtered_data) {
  lon_range <- diff(range(filtered_data$Longitude, na.rm = TRUE))
  lat_range <- diff(range(filtered_data$Latitude,  na.rm = TRUE))
  spread    <- max(lon_range, lat_range)
  
  case_when(
    spread > 30 ~ 3,
    spread > 10 ~ 4,
    spread > 2  ~ 5,
    TRUE        ~ 6
  )
}

#Plot the data on heatmap to show presence of species of interest
plot_historic_heatmap <- function(site_data, clicked_site) {
  ggplot(site_data, aes(x = as.factor(Year), y = Site, fill = Presence)) +
    geom_tile(color = "white") +
    scale_fill_manual(values = c("Absent" = "#d2d2d2", "Present" = "#716fa8")) +
    theme_minimal() +
    labs(
      title = paste("Species of Interest Presence at", clicked_site),
      x     = "Year",
      y     = "Site"
    )
}


#Create bar plot of the 
plot_historic_bar <- function(filtered_data, species) {
  
  filtered_data <- filtered_data %>%
    mutate(Presence = case_when(
      Presence %in% c("Yes", "Present", "TRUE", "1", TRUE)  ~ "Present",
      Presence %in% c("No", "Absent",  "FALSE", "0", FALSE) ~ "Absent",
      TRUE ~ as.character(Presence)
    ))
  
  plot_bar_data <- filtered_data %>%
    group_by(Year, Presence) %>%
    summarise(Count = n(), .groups = "drop")
  
  ggplot(plot_bar_data, aes(x = factor(Year), y = Count, fill = factor(Presence))) +
    geom_bar(stat = "identity", position = "stack") +
    labs(x = "Year", y = "Site Count", fill = "Presence") +
    scale_fill_manual(values = c("Absent" = "#d2d2d2", "Present" = "#716fa8")) +
    scale_y_continuous(
      breaks = scales::pretty_breaks(),
      labels = scales::number_format(accuracy = 1)
    ) +
    ggtitle(bquote(Presence/Absence~of~italic(.(species))~by~Year)) +
    theme(
      legend.position   = "top",
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
    )
}