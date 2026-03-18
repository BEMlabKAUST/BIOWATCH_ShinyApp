load_historic_data <- function() {
  tryCatch({
    if (file.exists("data/historic.csv")) {
      df <- read.csv("data/historic.csv", header = TRUE, stringsAsFactors = FALSE)
      if ("Year"        %in% colnames(df)) df$Year        <- as.numeric(df$Year)
      if ("Presence"    %in% colnames(df)) df$Presence    <- trimws(df$Presence)
      if ("Methodology" %in% colnames(df)) df$Methodology <- trimws(df$Methodology)
      return(df)
    }
  }, error = function(e) {
    print(paste("Error loading historic data:", e$message))
  })
  return(data.frame())
}


generate_pa_table <- function(filtered_results2, asv_table, coordinates,
                              selected_species) {
  
  soi_results <- filtered_results2 %>%
    filter(SOI_flag == TRUE, sscinames %in% selected_species) %>%
    select(qseqid, sscinames, Multiple_Instances)
  
  if (nrow(soi_results) == 0) return(NULL)
  
  asv_list   <- unique(soi_results$qseqid)
  filtered_asv <- asv_table %>% filter(ASV %in% asv_list)
  
  pa_table <- filtered_asv %>%
    pivot_longer(-ASV, names_to = "SampleID", values_to = "Abundance") %>%
    left_join(soi_results, by = c("ASV" = "qseqid")) %>%
    group_by(sscinames, SampleID) %>%
    summarise(Total_Abundance = sum(Abundance), .groups = "drop")
  
  pa_table_coord <- left_join(pa_table, coordinates, by = "SampleID") %>%
    group_by(SampleID, Site, Region, Latitude, Longitude, sscinames) %>%
    summarise(
      Total_Abundance = sum(Total_Abundance),
      Presence        = ifelse(sum(Total_Abundance) > 0, "Present", "Absent"),
      .groups         = "drop"
    ) %>%
    select(Species = sscinames, SampleID, Site, Region,
           Presence, Latitude, Longitude) %>%
    arrange(Species, Site)
  
  return(pa_table_coord)
}


add_manual_metadata <- function(pa_table, year, gene, methodology) {
  pa_table$Year        <- year
  pa_table$Gene        <- gene
  pa_table$Methodology <- methodology
  return(pa_table)
}


add_uploaded_metadata <- function(pa_table, metadata_path) {
  uploaded_metadata <- read.csv(metadata_path, stringsAsFactors = FALSE)
  
  if (!"SampleID" %in% colnames(uploaded_metadata)) {
    stop("Uploaded metadata must include a 'SampleID' column for joining.")
  }
  
  left_join(pa_table, uploaded_metadata, by = "SampleID")
}


reorder_pa_columns <- function(pa_table) {
  pa_table %>%
    select(Species, SampleID, Site, Region, Presence,
           Gene, Latitude, Longitude, Year, Methodology)
}


update_pa_cell <- function(data, row_index, col_index, value) {
  col_name <- colnames(data)[col_index]
  
  new_value <- if (col_name %in% c("Latitude", "Longitude", "Total_Abundance")) {
    as.numeric(value)
  } else {
    as.character(value)
  }
  
  data[row_index, col_index] <- new_value
  return(data)
}


add_to_historic <- function(new_data, current_historic) {
  
  new_data$DateAdded   <- as.character(Sys.Date())
  new_data$Year        <- as.character(new_data$Year)
  new_data$Methodology <- as.character(new_data$Methodology)
  
  if (nrow(current_historic) > 0) {
    
    if ("Year"        %in% colnames(current_historic)) current_historic$Year        <- as.character(current_historic$Year)
    if ("Methodology" %in% colnames(current_historic)) current_historic$Methodology <- as.character(current_historic$Methodology)
    if ("DateAdded"   %in% colnames(current_historic)) current_historic$DateAdded   <- as.character(current_historic$DateAdded)
    
    all_cols <- union(colnames(current_historic), colnames(new_data))
    
    for (col in all_cols) {
      if (!col %in% colnames(current_historic)) current_historic[[col]] <- NA_character_
      if (!col %in% colnames(new_data))         new_data[[col]]         <- NA_character_
    }
    
    updated_historic <- bind_rows(current_historic, new_data)
    
  } else {
    updated_historic <- new_data
  }
  
  updated_historic %>%
    arrange(desc(DateAdded)) %>%
    distinct(Species, SampleID, Site, Region, Year, Methodology, .keep_all = TRUE)
}


render_pa_datatable <- function(pa_table) {
  datatable(
    pa_table,
    editable = list(target = "cell", disable = list(columns = c(0, 1, 2, 3, 4, 5))),
    options  = list(
      pageLength = 25,
      scrollX    = TRUE,
      scrollY    = "400px",
      dom        = 'Bfrtip',
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().container()).css({'color': 'white'});",
        "}"
      )
    ),
    class    = 'cell-border stripe',
    rownames = FALSE
  ) %>%
    formatStyle(
      columns         = 1:ncol(pa_table),
      color           = 'white',
      backgroundColor = 'transparent'
    )
}