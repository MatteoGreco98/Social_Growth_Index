#!/usr/bin/env Rscript
################################################################################
# Script: 05_generate_visualizations.R
# Purpose: Generate maps and plots for SGI analysis
# Author: Social Growth Index Research Team
# Date: 2024
################################################################################

# Load required libraries
library(dplyr)
library(ggplot2)
library(sf)
library(tmap)
library(viridis)
library(scales)
library(tidyr)
library(gridExtra)

# Set working directory to project root
if (!exists("PROJECT_ROOT")) {
  PROJECT_ROOT <- here::here()
}

# Create output directories
dir.create(file.path(PROJECT_ROOT, "results/figures"), 
          recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(PROJECT_ROOT, "results/maps"), 
          recursive = TRUE, showWarnings = FALSE)

################################################################################
# 1. Load Data
################################################################################

load_sgi_data <- function() {
  cat("Loading SGI data...\n")
  
  data_file <- file.path(PROJECT_ROOT, "data/processed", "sgi_final.rds")
  boundaries_file <- file.path(PROJECT_ROOT, "data/processed", 
                              "municipalities_boundaries.gpkg")
  
  if (!file.exists(data_file)) {
    stop("SGI data not found. Please run 04_copeland_ranking.R first.")
  }
  
  sgi_data <- readRDS(data_file)
  
  # Load boundaries if available
  if (file.exists(boundaries_file)) {
    boundaries <- st_read(boundaries_file, quiet = TRUE)
    
    # Merge SGI data with boundaries
    sgi_spatial <- boundaries %>%
      left_join(
        sgi_data %>% st_drop_geometry(),
        by = "municipality_code"
      )
  } else {
    sgi_spatial <- sgi_data
  }
  
  cat(sprintf("Loaded %d observations\n", nrow(sgi_data)))
  
  return(list(
    data = sgi_data,
    spatial = sgi_spatial
  ))
}

################################################################################
# 2. Generate SGI Maps
################################################################################

create_sgi_map <- function(spatial_data, year = 2020, 
                          variable = "sgi", 
                          title = "Social Growth Index") {
  cat(sprintf("Creating map for %s (%d)...\n", variable, year))
  
  # Filter data for specific year
  year_data <- spatial_data %>%
    filter(year == !!year)
  
  if (nrow(year_data) == 0) {
    warning(sprintf("No data available for year %d", year))
    return(NULL)
  }
  
  # Create map using tmap
  map <- tm_shape(year_data) +
    tm_polygons(
      col = variable,
      palette = "viridis",
      style = "quantile",
      n = 5,
      title = title,
      border.alpha = 0.1,
      legend.hist = TRUE
    ) +
    tm_layout(
      title = sprintf("%s - %d", title, year),
      legend.outside = TRUE,
      legend.outside.position = "right",
      frame = FALSE
    ) +
    tm_compass(position = c("left", "bottom")) +
    tm_scale_bar(position = c("left", "bottom"))
  
  return(map)
}

generate_sgi_maps <- function(spatial_data, years = c(2010, 2015, 2020, 2022)) {
  cat("\nGenerating SGI maps...\n")
  
  maps_list <- list()
  
  for (year in years) {
    # SGI map
    sgi_map <- create_sgi_map(
      spatial_data, 
      year = year, 
      variable = "sgi",
      title = "Social Growth Index (SGI)"
    )
    
    if (!is.null(sgi_map)) {
      # Save map
      output_file <- file.path(PROJECT_ROOT, "results/maps", 
                              sprintf("sgi_map_%d.png", year))
      tmap_save(sgi_map, filename = output_file, 
               width = 3000, height = 2400, dpi = 300)
      cat(sprintf("  - Saved: %s\n", basename(output_file)))
      
      maps_list[[paste0("sgi_", year)]] <- sgi_map
    }
    
    # GDP per capita map
    gdp_map <- create_sgi_map(
      spatial_data, 
      year = year, 
      variable = "gdp_per_capita_municipal",
      title = "GDP per Capita"
    )
    
    if (!is.null(gdp_map)) {
      output_file <- file.path(PROJECT_ROOT, "results/maps", 
                              sprintf("gdp_per_capita_map_%d.png", year))
      tmap_save(gdp_map, filename = output_file, 
               width = 3000, height = 2400, dpi = 300)
    }
  }
  
  cat(sprintf("Generated %d maps\n", length(maps_list)))
  return(maps_list)
}

################################################################################
# 3. Generate Time Series Plots
################################################################################

plot_sgi_time_series <- function(sgi_data) {
  cat("\nCreating SGI time series plot...\n")
  
  # National average by year
  national_avg <- sgi_data %>%
    group_by(year) %>%
    summarise(
      mean_sgi = mean(sgi, na.rm = TRUE),
      median_sgi = median(sgi, na.rm = TRUE),
      q25_sgi = quantile(sgi, 0.25, na.rm = TRUE),
      q75_sgi = quantile(sgi, 0.75, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Create plot
  p <- ggplot(national_avg, aes(x = year)) +
    geom_ribbon(aes(ymin = q25_sgi, ymax = q75_sgi), 
               alpha = 0.2, fill = "steelblue") +
    geom_line(aes(y = mean_sgi, color = "Mean"), size = 1.2) +
    geom_line(aes(y = median_sgi, color = "Median"), size = 1.2) +
    geom_point(aes(y = mean_sgi), size = 3) +
    scale_color_manual(
      values = c("Mean" = "steelblue", "Median" = "darkorange"),
      name = ""
    ) +
    labs(
      title = "Social Growth Index Over Time",
      subtitle = "National average with interquartile range",
      x = "Year",
      y = "SGI"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      legend.position = "bottom"
    )
  
  # Save plot
  output_file <- file.path(PROJECT_ROOT, "results/figures", 
                          "sgi_time_series.png")
  ggsave(output_file, plot = p, width = 10, height = 6, dpi = 300)
  cat(sprintf("  - Saved: %s\n", basename(output_file)))
  
  return(p)
}

################################################################################
# 4. Generate Distribution Plots
################################################################################

plot_sgi_distribution <- function(sgi_data, year = 2020) {
  cat(sprintf("\nCreating SGI distribution plot (%d)...\n", year))
  
  year_data <- sgi_data %>%
    filter(year == !!year)
  
  if (nrow(year_data) == 0) {
    warning(sprintf("No data available for year %d", year))
    return(NULL)
  }
  
  # Create histogram
  p1 <- ggplot(year_data, aes(x = sgi)) +
    geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7, color = "black") +
    geom_vline(aes(xintercept = mean(sgi, na.rm = TRUE)), 
              color = "red", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = median(sgi, na.rm = TRUE)), 
              color = "orange", linetype = "dashed", size = 1) +
    labs(
      title = sprintf("SGI Distribution - %d", year),
      x = "Social Growth Index",
      y = "Number of Municipalities"
    ) +
    theme_minimal()
  
  # Create boxplot by category
  p2 <- ggplot(year_data, aes(x = sgi_category, y = sgi, fill = sgi_category)) +
    geom_boxplot(alpha = 0.7) +
    scale_fill_viridis_d() +
    labs(
      title = "SGI by Category",
      x = "Category",
      y = "Social Growth Index"
    ) +
    theme_minimal() +
    theme(legend.position = "none") +
    coord_flip()
  
  # Combine plots
  combined <- gridExtra::grid.arrange(p1, p2, ncol = 2)
  
  # Save plot
  output_file <- file.path(PROJECT_ROOT, "results/figures", 
                          sprintf("sgi_distribution_%d.png", year))
  ggsave(output_file, plot = combined, width = 14, height = 6, dpi = 300)
  cat(sprintf("  - Saved: %s\n", basename(output_file)))
  
  return(list(histogram = p1, boxplot = p2))
}

################################################################################
# 5. Generate Indicator Correlation Plot
################################################################################

plot_indicator_correlations <- function(sgi_data, year = 2020) {
  cat(sprintf("\nCreating correlation plot (%d)...\n", year))
  
  year_data <- sgi_data %>%
    filter(year == !!year) %>%
    select(
      `Pop Density` = population_density,
      `GDP per Capita` = gdp_per_capita_municipal,
      `GDP Density` = gdp_density_municipal,
      `SGI` = sgi
    )
  
  # Create correlation matrix
  cor_matrix <- cor(year_data, use = "complete.obs")
  
  # Create correlation plot
  library(corrplot)
  
  output_file <- file.path(PROJECT_ROOT, "results/figures", 
                          sprintf("indicator_correlations_%d.png", year))
  
  png(output_file, width = 2400, height = 2400, res = 300)
  corrplot(cor_matrix, 
          method = "color",
          type = "upper",
          addCoef.col = "black",
          tl.col = "black",
          tl.srt = 45,
          title = sprintf("Indicator Correlations - %d", year),
          mar = c(0, 0, 2, 0))
  dev.off()
  
  cat(sprintf("  - Saved: %s\n", basename(output_file)))
}

################################################################################
# 6. Generate Top Performers Comparison
################################################################################

plot_top_performers <- function(sgi_data, n = 20) {
  cat(sprintf("\nCreating top %d performers plot...\n", n))
  
  # Get top performers for latest year
  latest_year <- max(sgi_data$year, na.rm = TRUE)
  
  top_data <- sgi_data %>%
    filter(year == latest_year) %>%
    slice_max(order_by = sgi, n = n, with_ties = FALSE) %>%
    arrange(sgi) %>%
    mutate(municipality_name = factor(municipality_name, 
                                     levels = municipality_name))
  
  # Create bar plot
  p <- ggplot(top_data, aes(x = sgi, y = municipality_name, fill = sgi)) +
    geom_col() +
    scale_fill_viridis_c(option = "C") +
    labs(
      title = sprintf("Top %d Municipalities by SGI (%d)", n, latest_year),
      x = "Social Growth Index",
      y = ""
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 14, face = "bold")
    )
  
  # Save plot
  output_file <- file.path(PROJECT_ROOT, "results/figures", 
                          sprintf("top_%d_performers.png", n))
  ggsave(output_file, plot = p, width = 10, height = 8, dpi = 300)
  cat(sprintf("  - Saved: %s\n", basename(output_file)))
  
  return(p)
}

################################################################################
# 7. Generate Change Analysis Plot
################################################################################

plot_sgi_changes <- function(sgi_data) {
  cat("\nCreating SGI change analysis plot...\n")
  
  # Calculate changes from 2010 to 2022
  changes <- sgi_data %>%
    group_by(municipality_code, municipality_name) %>%
    filter(n() >= 2) %>%
    summarise(
      sgi_start = first(sgi, order_by = year),
      sgi_end = last(sgi, order_by = year),
      sgi_change = sgi_end - sgi_start,
      year_start = min(year),
      year_end = max(year),
      .groups = "drop"
    ) %>%
    arrange(-abs(sgi_change))
  
  # Get top gainers and losers
  top_changes <- bind_rows(
    changes %>% slice_max(order_by = sgi_change, n = 15) %>% mutate(type = "Gainers"),
    changes %>% slice_min(order_by = sgi_change, n = 15) %>% mutate(type = "Losers")
  ) %>%
    mutate(municipality_name = factor(municipality_name, 
                                     levels = municipality_name[order(sgi_change)]))
  
  # Create plot
  p <- ggplot(top_changes, aes(x = sgi_change, y = municipality_name, 
                               fill = type)) +
    geom_col() +
    scale_fill_manual(values = c("Gainers" = "forestgreen", "Losers" = "firebrick")) +
    labs(
      title = "Largest SGI Changes (2010-2022)",
      x = "Change in SGI",
      y = "",
      fill = ""
    ) +
    theme_minimal() +
    theme(
      legend.position = "top",
      plot.title = element_text(size = 14, face = "bold")
    )
  
  # Save plot
  output_file <- file.path(PROJECT_ROOT, "results/figures", 
                          "sgi_changes.png")
  ggsave(output_file, plot = p, width = 10, height = 10, dpi = 300)
  cat(sprintf("  - Saved: %s\n", basename(output_file)))
  
  return(p)
}

################################################################################
# 8. Main Execution
################################################################################

main <- function() {
  cat("Starting visualization generation...\n")
  cat(rep("=", 80), "\n", sep = "")
  
  # Load data
  loaded_data <- load_sgi_data()
  sgi_data <- loaded_data$data
  sgi_spatial <- loaded_data$spatial
  
  # Generate maps (if spatial data available)
  if ("sf" %in% class(sgi_spatial)) {
    generate_sgi_maps(sgi_spatial, years = c(2010, 2015, 2020, 2022))
  } else {
    cat("Spatial data not available, skipping map generation\n")
  }
  
  # Generate plots
  plot_sgi_time_series(sgi_data)
  plot_sgi_distribution(sgi_data, year = 2020)
  plot_indicator_correlations(sgi_data, year = 2020)
  plot_top_performers(sgi_data, n = 20)
  plot_sgi_changes(sgi_data)
  
  cat(rep("=", 80), "\n", sep = "")
  cat("Visualization generation completed!\n")
  cat("Results saved to:\n")
  cat("  - results/maps/\n")
  cat("  - results/figures/\n")
}

# Run main function if script is executed directly
if (sys.nframe() == 0) {
  main()
}
