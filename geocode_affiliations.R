# Geocode EPSA 2025 Affiliations
# Uses OpenStreetMap Nominatim API for geocoding

library(dplyr)
library(httr)
library(jsonlite)
library(readxl)
library(sf)

# Function to geocode a single affiliation
geocode_affiliation <- function(affiliation, delay = 1) {
  # Clean the affiliation string
  clean_affil <- gsub("University of|University|College|School|Institute|Department|Faculty", "", affiliation)
  clean_affil <- gsub("\\s+", " ", trimws(clean_affil))
  
  # Construct the query
  query <- URLencode(paste0(clean_affil, " university"))
  url <- paste0("https://nominatim.openstreetmap.org/search?q=", query, "&format=json&limit=1")
  
  # Add user agent to be polite to the API
  response <- GET(url, user_agent("EPSA2025_Geocoding/1.0"))
  
  # Check if request was successful
  if (status_code(response) == 200) {
    result <- fromJSON(rawToChar(response$content))
    
    if (length(result) > 0) {
      # Extract address components more carefully
      country <- NA
      state <- NA
      city <- NA
      
      # Try to extract address components safely
      if (!is.null(result$address) && length(result$address) > 0) {
        address <- result$address[[1]]  # Use [[1]] to get the first element
        if (is.list(address)) {
          country <- if (!is.null(address$country)) address$country else NA
          state <- if (!is.null(address$state)) address$state else NA
          city <- if (!is.null(address$city)) address$city else 
                  if (!is.null(address$town)) address$town else NA
        }
      }
      
      return(list(
        lat = as.numeric(result$lat[1]),
        lon = as.numeric(result$lon[1]),
        display_name = result$display_name[1],
        country = country,
        state = state,
        city = city
      ))
    }
  }
  
  # Return NA if no results or error
  return(list(
    lat = NA, lon = NA, display_name = NA, 
    country = NA, state = NA, city = NA
  ))
}

# Function to geocode all affiliations with smart caching
geocode_all_affiliations <- function(input_file = "epsa2025_affiliations_for_correction_coded.xlsx") {
  
  # Read affiliations
  if (!file.exists(input_file)) {
    stop("Input file not found. Please run the analysis script first.")
  }
  
  affiliations <- read_xlsx(input_file)
  
  # Filter for non-empty affiliations and use corrected ones
  affiliations <- affiliations %>%
    filter(!is.na(Affiliation) & Affiliation != "") %>%
    # Use corrected affiliation if available, otherwise use original
    mutate(affiliation_to_geocode = ifelse(!is.na(Corrected_Affiliation) & Corrected_Affiliation != "", 
                                           Corrected_Affiliation, Affiliation))
  
  # Recalculate frequencies based on corrected affiliations
  cat("Recalculating frequencies based on corrected affiliations...\n")
  frequency_recalc <- affiliations %>%
    group_by(affiliation_to_geocode) %>%
    summarise(
      total_frequency = sum(Frequency, na.rm = TRUE),
      original_affiliations = list(Affiliation),
      corrected_affiliations = list(Corrected_Affiliation)
    ) %>%
    arrange(desc(total_frequency))
  
  cat("Found", nrow(frequency_recalc), "unique affiliations after correction.\n")
  
  # Check existing cache
  cache_file <- "geocoded_affiliations.csv"
  existing_cache <- NULL
  if (file.exists(cache_file)) {
    cat("Loading existing cache...\n")
    existing_cache <- read.csv(cache_file, stringsAsFactors = FALSE)
  }
  
  # Initialize results
  results <- list()
  new_geocodes <- 0
  
  for (i in 1:nrow(frequency_recalc)) {
    affil_to_geocode <- frequency_recalc$affiliation_to_geocode[i]
    total_freq <- frequency_recalc$total_frequency[i]
    
    # Check if this affiliation is already in cache
    if (!is.null(existing_cache) && affil_to_geocode %in% existing_cache$Affiliation_Used) {
      cat(sprintf("Using cached result for %d/%d: %s (Frequency: %d)\n", 
                  i, nrow(frequency_recalc), affil_to_geocode, total_freq))
      
      # Get cached result
      cached_row <- existing_cache[existing_cache$Affiliation_Used == affil_to_geocode, ]
      results[[i]] <- data.frame(
        Affiliation = paste(unlist(frequency_recalc$original_affiliations[i]), collapse = "; "),
        Corrected_Affiliation = paste(unlist(frequency_recalc$corrected_affiliations[i]), collapse = "; "),
        Affiliation_Used = affil_to_geocode,
        Frequency = total_freq,
        lat = cached_row$lat,
        lon = cached_row$lon,
        display_name = cached_row$display_name,
        country = cached_row$country,
        state = cached_row$state,
        city = cached_row$city,
        stringsAsFactors = FALSE
      )
    } else {
      # Need to geocode this affiliation
      new_geocodes <- new_geocodes + 1
      cat(sprintf("Geocoding %d/%d: %s (Frequency: %d) [NEW]\n", 
                  i, nrow(frequency_recalc), affil_to_geocode, total_freq))
      
      # Geocode with delay to be polite to the API
      result <- geocode_affiliation(affil_to_geocode, delay = 1)
      
      results[[i]] <- data.frame(
        Affiliation = paste(unlist(frequency_recalc$original_affiliations[i]), collapse = "; "),
        Corrected_Affiliation = paste(unlist(frequency_recalc$corrected_affiliations[i]), collapse = "; "),
        Affiliation_Used = affil_to_geocode,
        Frequency = total_freq,
        lat = result$lat,
        lon = result$lon,
        display_name = result$display_name,
        country = result$country,
        state = result$state,
        city = result$city,
        stringsAsFactors = FALSE
      )
      
      # Be polite to the API - wait between requests
      Sys.sleep(1)
    }
  }
  
  # Combine results
  geocoded <- bind_rows(results)
  
  # Save to cache
  write.csv(geocoded, cache_file, row.names = FALSE)
  cat("Geocoding complete! Results saved to", cache_file, "\n")
  cat("New geocodes:", new_geocodes, "out of", nrow(frequency_recalc), "total affiliations.\n")
  
  return(geocoded)
}

# Function to create a static PNG map
create_static_map <- function(geocoded_data) {
  tryCatch({
    if (!require("ggplot2")) install.packages("ggplot2")
    if (!require("sf")) install.packages("sf")
    if (!require("rnaturalearth")) install.packages("rnaturalearth")
    if (!require("rnaturalearthdata")) install.packages("rnaturalearthdata")
    
    library(ggplot2)
    library(sf)
    library(rnaturalearth)
    library(rnaturalearthdata)
    
    # Filter out NA coordinates
    valid_coords <- geocoded_data %>%
      filter(!is.na(lat) & !is.na(lon))
    
    cat("Creating map with", nrow(valid_coords), "valid coordinates...\n")
    
    # Try to get world map data
    tryCatch({
      world <- ne_countries(scale = "medium", returnclass = "sf")
      cat("World map data loaded successfully\n")
      
      # Create the map with world background
      p <- ggplot() +
        geom_sf(data = world, fill = "lightgray", color = "white", size = 0.1) +
        geom_point(data = valid_coords, 
                   aes(x = lon, y = lat, size = Frequency), 
                   color = "red", alpha = 0.7) +
        scale_size_continuous(
          range = c(1, 5),
          name = "Number of\nPresentations"
        ) +
        labs(
          title = "Top 20 Affiliations at EPSA 2025",
          subtitle = "Size of dots indicates number of presentations",
          x = "Longitude",
          y = "Latitude"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          plot.subtitle = element_text(size = 12),
          axis.title = element_text(size = 10),
          legend.title = element_text(size = 10, face = "bold"),
          legend.text = element_text(size = 9),
          plot.background = element_rect(fill = "white"),
          panel.grid = element_line(color = "gray90")
        ) +
        coord_sf()
      
    }, error = function(e) {
      cat("World map failed, creating simple map...\n")
      # Fallback: simple map without world background
      p <<- ggplot(valid_coords, aes(x = lon, y = lat, size = Frequency)) +
        geom_point(color = "red", alpha = 0.7) +
        scale_size_continuous(
          range = c(1, 5),
          name = "Number of\nPresentations"
        ) +
        labs(
          title = "Top 20 Affiliations at EPSA 2025",
          subtitle = "Size of dots indicates number of presentations",
          x = "Longitude",
          y = "Latitude"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          plot.subtitle = element_text(size = 12),
          axis.title = element_text(size = 10),
          legend.title = element_text(size = 10, face = "bold"),
          legend.text = element_text(size = 9),
          plot.background = element_rect(fill = "white"),
          panel.grid = element_line(color = "gray90")
        )
    })
    
    # Save the map
    cat("Saving map to epsa2025_affiliations_map.png...\n")
    ggsave("epsa2025_affiliations_map.png", p, width = 12, height = 8, dpi = 300, bg = "white")
    cat("Static map saved successfully as epsa2025_affiliations_map.png\n")
    
    return(p)
    
  }, error = function(e) {
    cat("Error creating static map:", e$message, "\n")
    cat("Creating simple fallback map...\n")
    
    # Ultra-simple fallback
    library(ggplot2)
    valid_coords <- geocoded_data %>%
      filter(!is.na(lat) & !is.na(lon))
    
    p <- ggplot(valid_coords, aes(x = lon, y = lat, size = Frequency)) +
      geom_point(color = "red", alpha = 0.7) +
      labs(title = "Top 20 Affiliations at EPSA 2025") +
      theme_minimal()
    
    ggsave("epsa2025_affiliations_map.png", p, width = 10, height = 8, dpi = 300, bg = "white")
    cat("Simple fallback map saved\n")
    
    return(p)
  })
}

# Function to create a map visualization
create_affiliation_map <- function(geocoded_data) {
  if (!require("leaflet")) install.packages("leaflet")
  library(leaflet)
  
  # Filter out NA coordinates
  valid_coords <- geocoded_data %>%
    filter(!is.na(lat) & !is.na(lon))
  
  # Create map
  map <- leaflet(valid_coords) %>%
    addTiles() %>%
    addCircleMarkers(
      lng = ~lon,
      lat = ~lat,
      popup = ~paste(Affiliation, "<br>", "Presentations:", Frequency, "<br>", display_name),
      radius = ~sqrt(Frequency) * 3,  # Size based on frequency
      color = "red",
      fillOpacity = 0.7,
      weight = 2
    )
  
  return(map)
}

# Function to create a static PNG map for Europe only
create_europe_map <- function(geocoded_data) {
  tryCatch({
    if (!require("ggplot2")) install.packages("ggplot2")
    if (!require("sf")) install.packages("sf")
    if (!require("rnaturalearth")) install.packages("rnaturalearth")
    if (!require("rnaturalearthdata")) install.packages("rnaturalearthdata")
    
    library(ggplot2)
    library(sf)
    library(rnaturalearth)
    library(rnaturalearthdata)
    
    # Filter out NA coordinates
    valid_coords <- geocoded_data %>%
      filter(!is.na(lat) & !is.na(lon))
    
    cat("Creating Europe map with", nrow(valid_coords), "valid coordinates...\n")
    
    # Try to get world map data
    tryCatch({
      world <- ne_countries(scale = "medium", returnclass = "sf")
      cat("World map data loaded successfully\n")
      
      # Create the Europe-focused map
      p <- ggplot() +
        geom_sf(data = world, fill = "lightgray", color = "white", size = 0.1) +
        geom_point(data = valid_coords, 
                   aes(x = lon, y = lat, size = Frequency), 
                   color = "red", alpha = 0.7) +
        scale_size_continuous(
          range = c(1, 5),
          name = "Number of\nPresentations"
        ) +
        labs(
          title = "Top 20 Affiliations at EPSA 2025 - Europe",
          subtitle = "Size of dots indicates number of presentations",
          x = "Longitude",
          y = "Latitude"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          plot.subtitle = element_text(size = 12),
          axis.title = element_text(size = 10),
          legend.title = element_text(size = 10, face = "bold"),
          legend.text = element_text(size = 9),
          plot.background = element_rect(fill = "white"),
          panel.grid = element_line(color = "gray90")
        ) +
        # Zoom to Europe
        coord_sf(xlim = c(-10, 40), ylim = c(35, 70))
      
    }, error = function(e) {
      cat("World map failed, creating simple Europe map...\n")
      # Fallback: simple map without world background
      p <<- ggplot(valid_coords, aes(x = lon, y = lat, size = Frequency)) +
        geom_point(color = "red", alpha = 0.7) +
        scale_size_continuous(
          range = c(1, 5),
          name = "Number of\nPresentations"
        ) +
        labs(
          title = "Top 20 Affiliations at EPSA 2025 - Europe",
          subtitle = "Size of dots indicates number of presentations",
          x = "Longitude",
          y = "Latitude"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          plot.subtitle = element_text(size = 12),
          axis.title = element_text(size = 10),
          legend.title = element_text(size = 10, face = "bold"),
          legend.text = element_text(size = 9),
          plot.background = element_rect(fill = "white"),
          panel.grid = element_line(color = "gray90")
        ) +
        # Zoom to Europe
        coord_cartesian(xlim = c(-10, 40), ylim = c(35, 70))
    })
    
    # Save the map
    cat("Saving Europe map to epsa2025_affiliations_europe.png...\n")
    ggsave("epsa2025_affiliations_europe.png", p, width = 12, height = 8, dpi = 300, bg = "white")
    cat("Europe map saved successfully as epsa2025_affiliations_europe.png\n")
    
    return(p)
    
  }, error = function(e) {
    cat("Error creating Europe map:", e$message, "\n")
    cat("Creating simple fallback Europe map...\n")
    
    # Ultra-simple fallback
    library(ggplot2)
    valid_coords <- geocoded_data %>%
      filter(!is.na(lat) & !is.na(lon))
    
    p <- ggplot(valid_coords, aes(x = lon, y = lat, size = Frequency)) +
      geom_point(color = "red", alpha = 0.7) +
      labs(title = "Top 20 Affiliations at EPSA 2025 - Europe") +
      theme_minimal() +
      coord_cartesian(xlim = c(-10, 40), ylim = c(35, 70))
    
    ggsave("epsa2025_affiliations_europe.png", p, width = 10, height = 8, dpi = 300, bg = "white")
    cat("Simple fallback Europe map saved\n")
    
    return(p)
  })
}

# Function to save leaflet map as standalone HTML
save_leaflet_html <- function(geocoded_data) {
  if (!require("leaflet")) install.packages("leaflet")
  library(leaflet)
  
  # Filter out NA coordinates
  valid_coords <- geocoded_data %>%
    filter(!is.na(lat) & !is.na(lon))
  
  # Create map
  map <- leaflet(valid_coords) %>%
    addTiles() %>%
    addCircleMarkers(
      lng = ~lon,
      lat = ~lat,
      popup = ~paste("<b>", Affiliation_Used, "</b><br>", 
                     "Presentations:", Frequency, "<br>", 
                     display_name),
      radius = ~sqrt(Frequency) * 3,  # Size based on frequency
      color = "red",
      fillOpacity = 0.7,
      weight = 2
    ) %>%
    setView(lng = 10, lat = 50, zoom = 4)  # Center on Europe
  
  # Save as HTML
  library(htmlwidgets)
  saveWidget(map, "epsa2025_affiliations_map.html", selfcontained = FALSE)
  cat("Interactive map saved as epsa2025_affiliations_map.html\n")
  cat("You can upload this file to GitHub Pages or any web server.\n")
  
  return(map)
}

# Function to determine country from coordinates using shapefiles
get_country_from_coordinates <- function(lat, lon, world_sf) {
  if (is.na(lat) || is.na(lon)) return(NA)
  
  # Create a point from coordinates
  point <- st_point(c(lon, lat))
  point_sf <- st_sfc(point, crs = st_crs(world_sf))
  
  # Find which country polygon contains this point
  country_index <- st_intersects(point_sf, world_sf, sparse = FALSE)[1,]
  
  if (any(country_index)) {
    country_name <- world_sf$name[which(country_index)]
    return(country_name)
  }
  
  return(NA)
}

# Function to add countries to geocoded data using shapefiles
add_countries_from_coordinates <- function(geocoded_data) {
  cat("Adding countries based on coordinates using shapefiles...\n")
  
  # Load world shapefile data
  if (!require("rnaturalearth")) install.packages("rnaturalearth")
  if (!require("rnaturalearthdata")) install.packages("rnaturalearthdata")
  library(rnaturalearth)
  library(rnaturalearthdata)
  
  world_sf <- ne_countries(scale = "medium", returnclass = "sf")
  cat("World shapefile loaded successfully\n")
  
  # Add country column if it doesn't exist
  if (!"country_from_coords" %in% colnames(geocoded_data)) {
    geocoded_data$country_from_coords <- NA
  }
  
  # Process each row
  for (i in 1:nrow(geocoded_data)) {
    if (!is.na(geocoded_data$lat[i]) && !is.na(geocoded_data$lon[i])) {
      cat(sprintf("Processing %d/%d: %s\n", i, nrow(geocoded_data), geocoded_data$Affiliation[i]))
      
      country <- get_country_from_coordinates(geocoded_data$lat[i], geocoded_data$lon[i], world_sf)
      geocoded_data$country_from_coords[i] <- country
    }
  }
  
  # Save updated data
  write.csv(geocoded_data, "geocoded_affiliations.csv", row.names = FALSE)
  cat("Countries added and data saved.\n")
  
  return(geocoded_data)
}

# Function to create top countries bar plot
create_countries_bar_plot <- function(geocoded_data) {
  if (!require("ggplot2")) install.packages("ggplot2")
  library(ggplot2)
  
  # Get top 10 countries
  top_countries <- geocoded_data %>%
    filter(!is.na(country_from_coords)) %>%
    group_by(country_from_coords) %>%
    summarise(
      total_presentations = sum(Frequency, na.rm = TRUE),
      unique_institutions = n()
    ) %>%
    arrange(desc(total_presentations)) %>%
    head(10) %>%
    mutate(country_from_coords = factor(country_from_coords, levels = rev(country_from_coords)))
  
  # Create the bar plot
  p <- ggplot(top_countries, aes(x = total_presentations, y = country_from_coords)) +
    geom_col(fill = "steelblue", alpha = 0.8) +
    geom_text(aes(label = total_presentations), hjust = -0.5, size = 4, fontface = "bold") +
    labs(
      title = "Top 10 Countries at EPSA 2025",
      subtitle = "Total presentations by country",
      x = "Number of Presentations",
      y = "Country"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 10, face = "bold"),
      axis.text.x = element_text(size = 10),
      axis.title = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      plot.background = element_rect(fill = "white"),
      panel.grid.major.y = element_line(color = "gray90"),
      panel.grid.minor = element_blank()
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.1)))
  
  # Save the plot
  ggsave("top_countries_epsa2025.png", p, width = 12, height = 8, dpi = 300, bg = "white")
  cat("Top countries bar plot saved as top_countries_epsa2025.png\n")
  
  # Print the data
  cat("\n=== TOP 10 COUNTRIES ===\n")
  print(top_countries)
  
  return(p)
}

# Main execution
if (interactive()) {
  cat("=== EPSA 2025 Affiliation Geocoding ===\n")
  
  # Geocode affiliations
  geocoded <- geocode_all_affiliations()
  
  # Add countries based on coordinates
  geocoded <- add_countries_from_coordinates(geocoded)
  
  # Summary
  cat("\n=== GEOCODING SUMMARY ===\n")
  cat("Total affiliations:", nrow(geocoded), "\n")
  cat("Successfully geocoded:", sum(!is.na(geocoded$lat)), "\n")
  cat("Failed to geocode:", sum(is.na(geocoded$lat)), "\n")
  
  # Top countries
  cat("\n=== TOP COUNTRIES ===\n")
  country_counts <- geocoded %>%
    filter(!is.na(country)) %>%
    count(country, sort = TRUE) %>%
    head(10)
  print(country_counts)
  
  # Create map (optional)
  cat("\nCreating interactive map...\n")
  map <- create_affiliation_map(geocoded)
  print(map)
  
  # Create static PNG map
  cat("\nCreating static PNG map...\n")
  static_map <- create_static_map(geocoded)
  print(static_map)
  
  # Create Europe map
  cat("\nCreating Europe map...\n")
  europe_map <- create_europe_map(geocoded)
  print(europe_map)
  
  # Save interactive map as HTML
  cat("\nSaving interactive map as HTML...\n")
  html_map <- save_leaflet_html(geocoded)
  cat("HTML file created: epsa2025_affiliations_map.html\n")
  
  # Create top countries bar plot
  cat("\nCreating top countries bar plot...\n")
  create_countries_bar_plot(geocoded)
} 
