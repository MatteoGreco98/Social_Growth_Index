#!/usr/bin/env Rscript
################################################################################
# Script: 03_compute_indicators.R
# Purpose: Compute standardized indicators for SGI analysis
# Author: Social Growth Index Research Team
# Date: 2024
################################################################################

# Load required libraries
library(dplyr)
library(tidyr)
library(readr)

# Set working directory to project root
if (!exists("PROJECT_ROOT")) {
  PROJECT_ROOT <- here::here()
}

################################################################################
# 1. Load Downscaled GDP Data
################################################################################

load_gdp_data <- function() {
  cat("Loading downscaled GDP data...\n")
  
  data_file <- file.path(PROJECT_ROOT, "data/processed", "gdp_downscaled.rds")
  
  if (!file.exists(data_file)) {
    stop("Downscaled GDP data not found. Please run 02_frk_gdp_downscaling.R first.")
  }
  
  data <- readRDS(data_file)
  cat(sprintf("Loaded %d observations\n", nrow(data)))
  
  return(data)
}

################################################################################
# 2. Standardize Indicators
################################################################################

standardize_indicator <- function(x, method = "zscore") {
  """
  Standardize indicator values
  
  Args:
    x: numeric vector to standardize
    method: standardization method ('zscore', 'minmax', 'rank')
  
  Returns:
    Standardized values
  """
  
  if (all(is.na(x))) {
    return(x)
  }
  
  if (method == "zscore") {
    # Z-score standardization: (x - mean) / sd
    return((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
    
  } else if (method == "minmax") {
    # Min-max normalization: (x - min) / (max - min)
    min_val <- min(x, na.rm = TRUE)
    max_val <- max(x, na.rm = TRUE)
    return((x - min_val) / (max_val - min_val))
    
  } else if (method == "rank") {
    # Rank-based normalization
    return(rank(x, na.last = "keep") / sum(!is.na(x)))
    
  } else {
    stop("Unknown standardization method")
  }
}

################################################################################
# 3. Compute Standardized Indicators
################################################################################

compute_standardized_indicators <- function(data, method = "zscore") {
  cat("\nComputing standardized indicators...\n")
  cat(sprintf("  Method: %s\n", method))
  
  # Select key indicators
  indicators_data <- data %>%
    st_drop_geometry() %>%
    select(
      year, municipality_code, municipality_name,
      population, area_km2,
      population_density,
      gdp_municipal_final,
      gdp_per_capita_municipal,
      gdp_density_municipal
    )
  
  # Standardize by year (to compare municipalities within same year)
  standardized_data <- indicators_data %>%
    group_by(year) %>%
    mutate(
      # Standardize three key indicators
      population_density_std = standardize_indicator(population_density, method),
      gdp_per_capita_std = standardize_indicator(gdp_per_capita_municipal, method),
      gdp_density_std = standardize_indicator(gdp_density_municipal, method),
      
      # Also compute percentile ranks for interpretation
      population_density_pct = rank(population_density, na.last = "keep") / 
                                sum(!is.na(population_density)),
      gdp_per_capita_pct = rank(gdp_per_capita_municipal, na.last = "keep") / 
                           sum(!is.na(gdp_per_capita_municipal)),
      gdp_density_pct = rank(gdp_density_municipal, na.last = "keep") / 
                        sum(!is.na(gdp_density_municipal))
    ) %>%
    ungroup()
  
  cat(sprintf("Standardized indicators computed: %d observations\n", 
              nrow(standardized_data)))
  
  return(standardized_data)
}

################################################################################
# 4. Calculate Temporal Changes
################################################################################

calculate_temporal_changes <- function(standardized_data) {
  cat("\nCalculating temporal changes...\n")
  
  # Calculate year-over-year changes
  temporal_data <- standardized_data %>%
    arrange(municipality_code, year) %>%
    group_by(municipality_code) %>%
    mutate(
      # Absolute changes
      population_change = population - lag(population),
      gdp_per_capita_change = gdp_per_capita_municipal - lag(gdp_per_capita_municipal),
      gdp_density_change = gdp_density_municipal - lag(gdp_density_municipal),
      
      # Percentage changes
      population_pct_change = (population - lag(population)) / lag(population) * 100,
      gdp_per_capita_pct_change = (gdp_per_capita_municipal - lag(gdp_per_capita_municipal)) / 
                                   lag(gdp_per_capita_municipal) * 100,
      
      # Cumulative changes from 2010 baseline
      years_from_baseline = year - min(year, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Calculate baseline (2010) values for comparison
  baseline_values <- temporal_data %>%
    filter(year == min(year, na.rm = TRUE)) %>%
    select(
      municipality_code,
      population_2010 = population,
      gdp_per_capita_2010 = gdp_per_capita_municipal,
      gdp_density_2010 = gdp_density_municipal
    )
  
  temporal_data <- temporal_data %>%
    left_join(baseline_values, by = "municipality_code") %>%
    mutate(
      # Cumulative changes from 2010
      population_total_change = (population - population_2010) / population_2010 * 100,
      gdp_per_capita_total_change = (gdp_per_capita_municipal - gdp_per_capita_2010) / 
                                     gdp_per_capita_2010 * 100
    )
  
  cat("Temporal changes calculated\n")
  return(temporal_data)
}

################################################################################
# 5. Compute Composite Indicators
################################################################################

compute_composite_indicators <- function(standardized_data) {
  cat("\nComputing composite indicators...\n")
  
  composite_data <- standardized_data %>%
    mutate(
      # Simple average of standardized indicators
      composite_simple = (population_density_std + gdp_per_capita_std + 
                         gdp_density_std) / 3,
      
      # Weighted average (can be adjusted based on research needs)
      # Default weights: GDP per capita (0.4), GDP density (0.3), Pop density (0.3)
      composite_weighted = 0.4 * gdp_per_capita_std + 
                          0.3 * gdp_density_std + 
                          0.3 * population_density_std,
      
      # Economic focus composite (GDP indicators only)
      composite_economic = (gdp_per_capita_std + gdp_density_std) / 2
    )
  
  # Compute percentile ranks for composite indicators
  composite_data <- composite_data %>%
    group_by(year) %>%
    mutate(
      composite_simple_pct = rank(composite_simple, na.last = "keep") / 
                            sum(!is.na(composite_simple)),
      composite_weighted_pct = rank(composite_weighted, na.last = "keep") / 
                              sum(!is.na(composite_weighted)),
      composite_economic_pct = rank(composite_economic, na.last = "keep") / 
                              sum(!is.na(composite_economic))
    ) %>%
    ungroup()
  
  cat("Composite indicators computed\n")
  return(composite_data)
}

################################################################################
# 6. Generate Summary Statistics
################################################################################

generate_summary_statistics <- function(indicators_data) {
  cat("\nGenerating summary statistics...\n")
  
  # National-level summary by year
  national_summary <- indicators_data %>%
    group_by(year) %>%
    summarise(
      n_municipalities = n(),
      
      # Population statistics
      total_population = sum(population, na.rm = TRUE),
      mean_population = mean(population, na.rm = TRUE),
      median_population = median(population, na.rm = TRUE),
      
      # GDP per capita statistics
      mean_gdp_per_capita = mean(gdp_per_capita_municipal, na.rm = TRUE),
      median_gdp_per_capita = median(gdp_per_capita_municipal, na.rm = TRUE),
      sd_gdp_per_capita = sd(gdp_per_capita_municipal, na.rm = TRUE),
      
      # GDP density statistics
      mean_gdp_density = mean(gdp_density_municipal, na.rm = TRUE),
      median_gdp_density = median(gdp_density_municipal, na.rm = TRUE),
      
      # Composite indicator statistics
      mean_composite = mean(composite_weighted, na.rm = TRUE),
      median_composite = median(composite_weighted, na.rm = TRUE),
      sd_composite = sd(composite_weighted, na.rm = TRUE),
      
      .groups = "drop"
    )
  
  # Regional statistics (by NUTS codes if available)
  if ("nuts3_code" %in% names(indicators_data)) {
    regional_summary <- indicators_data %>%
      group_by(year, nuts3_code) %>%
      summarise(
        n_municipalities = n(),
        total_population = sum(population, na.rm = TRUE),
        mean_gdp_per_capita = mean(gdp_per_capita_municipal, na.rm = TRUE),
        mean_composite = mean(composite_weighted, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    regional_summary <- NULL
  }
  
  return(list(
    national = national_summary,
    regional = regional_summary
  ))
}

################################################################################
# 7. Main Execution
################################################################################

main <- function() {
  cat("Starting indicator computation...\n")
  cat(rep("=", 80), "\n", sep = "")
  
  # Load data
  gdp_data <- load_gdp_data()
  
  # Compute standardized indicators
  standardized_data <- compute_standardized_indicators(
    data = gdp_data,
    method = "zscore"  # Can be changed to "minmax" or "rank"
  )
  
  # Calculate temporal changes
  temporal_data <- calculate_temporal_changes(standardized_data)
  
  # Compute composite indicators
  final_data <- compute_composite_indicators(temporal_data)
  
  # Generate summary statistics
  summary_stats <- generate_summary_statistics(final_data)
  
  # Save results
  cat("\nSaving results...\n")
  
  # Save full dataset
  output_file <- file.path(PROJECT_ROOT, "data/processed", 
                          "indicators_standardized.rds")
  saveRDS(final_data, output_file)
  cat(sprintf("  - Full dataset: %s\n", output_file))
  
  # Save as CSV
  write_csv(final_data, 
           file.path(PROJECT_ROOT, "data/processed", 
                    "indicators_standardized.csv"))
  cat("  - CSV export: data/processed/indicators_standardized.csv\n")
  
  # Save summary statistics
  write_csv(summary_stats$national,
           file.path(PROJECT_ROOT, "results/tables", 
                    "summary_statistics_national.csv"))
  cat("  - National summary: results/tables/summary_statistics_national.csv\n")
  
  if (!is.null(summary_stats$regional)) {
    write_csv(summary_stats$regional,
             file.path(PROJECT_ROOT, "results/tables", 
                      "summary_statistics_regional.csv"))
    cat("  - Regional summary: results/tables/summary_statistics_regional.csv\n")
  }
  
  # Print summary
  cat("\nSummary Statistics (National):\n")
  cat(rep("-", 80), "\n", sep = "")
  print(summary_stats$national)
  
  cat(rep("=", 80), "\n", sep = "")
  cat("Indicator computation completed!\n")
}

# Run main function if script is executed directly
if (sys.nframe() == 0) {
  main()
}
