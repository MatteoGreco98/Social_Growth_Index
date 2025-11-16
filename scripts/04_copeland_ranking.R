#!/usr/bin/env Rscript
################################################################################
# Script: 04_copeland_ranking.R
# Purpose: Apply Copeland ranking method for Social Growth Index
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
# 1. Load Standardized Indicators
################################################################################

load_indicators <- function() {
  cat("Loading standardized indicators...\n")
  
  data_file <- file.path(PROJECT_ROOT, "data/processed", 
                        "indicators_standardized.rds")
  
  if (!file.exists(data_file)) {
    stop("Standardized indicators not found. Please run 03_compute_indicators.R first.")
  }
  
  data <- readRDS(data_file)
  cat(sprintf("Loaded %d observations\n", nrow(data)))
  
  return(data)
}

################################################################################
# 2. Copeland Ranking Method
################################################################################

copeland_pairwise_comparison <- function(values1, values2) {
  """
  Perform pairwise comparison between two indicators
  
  Args:
    values1: numeric vector of indicator 1 values
    values2: numeric vector of indicator 2 values
  
  Returns:
    Vector of comparison results: 1 if indicator1 > indicator2, 
                                  -1 if indicator1 < indicator2,
                                  0 if equal
  """
  
  n <- length(values1)
  comparison <- numeric(n)
  
  for (i in 1:n) {
    if (is.na(values1[i]) || is.na(values2[i])) {
      comparison[i] <- 0
    } else if (values1[i] > values2[i]) {
      comparison[i] <- 1
    } else if (values1[i] < values2[i]) {
      comparison[i] <- -1
    } else {
      comparison[i] <- 0
    }
  }
  
  return(comparison)
}

compute_copeland_scores <- function(data_matrix) {
  """
  Compute Copeland scores for municipalities based on pairwise comparisons
  
  The Copeland method compares each municipality against all others
  across multiple indicators and counts wins minus losses.
  
  Args:
    data_matrix: matrix where rows are municipalities and columns are indicators
  
  Returns:
    Vector of Copeland scores
  """
  
  n <- nrow(data_matrix)
  copeland_scores <- numeric(n)
  
  # For each municipality
  for (i in 1:n) {
    wins <- 0
    losses <- 0
    
    # Compare with every other municipality
    for (j in 1:n) {
      if (i != j) {
        # Count indicators where i beats j
        comparisons <- data_matrix[i, ] > data_matrix[j, ]
        
        # Handle NAs
        comparisons <- comparisons[!is.na(comparisons)]
        
        if (length(comparisons) > 0) {
          wins_ij <- sum(comparisons)
          losses_ij <- sum(!comparisons)
          
          # If i wins on more indicators than j, count as win
          if (wins_ij > losses_ij) {
            wins <- wins + 1
          } else if (wins_ij < losses_ij) {
            losses <- losses + 1
          }
          # Ties don't count
        }
      }
    }
    
    # Copeland score = wins - losses
    copeland_scores[i] <- wins - losses
  }
  
  return(copeland_scores)
}

################################################################################
# 3. Apply Copeland Ranking
################################################################################

apply_copeland_ranking <- function(indicators_data, 
                                  indicators = c("gdp_per_capita_std", 
                                                "gdp_density_std", 
                                                "population_density_std")) {
  cat("\nApplying Copeland ranking...\n")
  cat(sprintf("  Using indicators: %s\n", paste(indicators, collapse = ", ")))
  
  # Process each year separately
  ranked_data <- indicators_data %>%
    group_by(year) %>%
    group_modify(~ {
      # Extract indicator matrix for this year
      indicator_matrix <- .x %>%
        select(all_of(indicators)) %>%
        as.matrix()
      
      # Compute Copeland scores
      copeland_scores <- compute_copeland_scores(indicator_matrix)
      
      # Add scores to data
      .x %>%
        mutate(
          copeland_score = copeland_scores,
          copeland_rank = rank(-copeland_score, ties.method = "min"),
          copeland_percentile = rank(copeland_score, na.last = "keep") / 
                               sum(!is.na(copeland_score))
        )
    }) %>%
    ungroup()
  
  cat(sprintf("Copeland ranking completed: %d observations\n", 
              nrow(ranked_data)))
  
  return(ranked_data)
}

################################################################################
# 4. Calculate Social Growth Index (SGI)
################################################################################

calculate_sgi <- function(ranked_data) {
  cat("\nCalculating Social Growth Index (SGI)...\n")
  
  # SGI is the normalized Copeland score (0-100 scale)
  sgi_data <- ranked_data %>%
    group_by(year) %>%
    mutate(
      # Normalize Copeland score to 0-100 scale
      copeland_min = min(copeland_score, na.rm = TRUE),
      copeland_max = max(copeland_score, na.rm = TRUE),
      
      # Social Growth Index (SGI)
      sgi = ((copeland_score - copeland_min) / 
            (copeland_max - copeland_min)) * 100,
      
      # SGI categories for interpretation
      sgi_category = case_when(
        sgi >= 80 ~ "Very High",
        sgi >= 60 ~ "High",
        sgi >= 40 ~ "Medium",
        sgi >= 20 ~ "Low",
        TRUE ~ "Very Low"
      ),
      
      # SGI rank
      sgi_rank = rank(-sgi, ties.method = "min")
    ) %>%
    select(-copeland_min, -copeland_max) %>%
    ungroup()
  
  cat("Social Growth Index calculated\n")
  return(sgi_data)
}

################################################################################
# 5. Identify Top and Bottom Performers
################################################################################

identify_performers <- function(sgi_data, n_top = 50, n_bottom = 50) {
  cat("\nIdentifying top and bottom performers...\n")
  
  # Top performers by year
  top_performers <- sgi_data %>%
    group_by(year) %>%
    slice_max(order_by = sgi, n = n_top, with_ties = FALSE) %>%
    arrange(year, -sgi) %>%
    ungroup()
  
  # Bottom performers by year
  bottom_performers <- sgi_data %>%
    group_by(year) %>%
    slice_min(order_by = sgi, n = n_bottom, with_ties = FALSE) %>%
    arrange(year, sgi) %>%
    ungroup()
  
  # Consistent top performers (in top quartile for most years)
  consistent_top <- sgi_data %>%
    group_by(municipality_code, municipality_name) %>%
    summarise(
      n_years = n(),
      avg_sgi = mean(sgi, na.rm = TRUE),
      median_sgi = median(sgi, na.rm = TRUE),
      n_top_quartile = sum(sgi_category %in% c("Very High", "High")),
      prop_top_quartile = n_top_quartile / n_years,
      .groups = "drop"
    ) %>%
    filter(prop_top_quartile >= 0.75) %>%
    arrange(-avg_sgi)
  
  # Most improved municipalities
  sgi_changes <- sgi_data %>%
    arrange(municipality_code, year) %>%
    group_by(municipality_code, municipality_name) %>%
    filter(n() >= 2) %>%
    summarise(
      sgi_2010 = first(sgi, order_by = year),
      sgi_2022 = last(sgi, order_by = year),
      sgi_change = sgi_2022 - sgi_2010,
      years_covered = n(),
      .groups = "drop"
    ) %>%
    arrange(-sgi_change)
  
  most_improved <- sgi_changes %>%
    slice_max(order_by = sgi_change, n = n_top, with_ties = FALSE)
  
  most_declined <- sgi_changes %>%
    slice_min(order_by = sgi_change, n = n_bottom, with_ties = FALSE)
  
  cat(sprintf("  - Top %d performers identified per year\n", n_top))
  cat(sprintf("  - Bottom %d performers identified per year\n", n_bottom))
  cat(sprintf("  - %d consistent top performers\n", nrow(consistent_top)))
  cat(sprintf("  - %d most improved municipalities\n", nrow(most_improved)))
  
  return(list(
    top = top_performers,
    bottom = bottom_performers,
    consistent_top = consistent_top,
    most_improved = most_improved,
    most_declined = most_declined
  ))
}

################################################################################
# 6. Generate SGI Summary Statistics
################################################################################

generate_sgi_statistics <- function(sgi_data) {
  cat("\nGenerating SGI summary statistics...\n")
  
  # Annual statistics
  annual_stats <- sgi_data %>%
    group_by(year) %>%
    summarise(
      n_municipalities = n(),
      mean_sgi = mean(sgi, na.rm = TRUE),
      median_sgi = median(sgi, na.rm = TRUE),
      sd_sgi = sd(sgi, na.rm = TRUE),
      min_sgi = min(sgi, na.rm = TRUE),
      max_sgi = max(sgi, na.rm = TRUE),
      
      # Category distribution
      n_very_high = sum(sgi_category == "Very High"),
      n_high = sum(sgi_category == "High"),
      n_medium = sum(sgi_category == "Medium"),
      n_low = sum(sgi_category == "Low"),
      n_very_low = sum(sgi_category == "Very Low"),
      
      .groups = "drop"
    )
  
  # Overall statistics
  overall_stats <- sgi_data %>%
    summarise(
      n_observations = n(),
      n_municipalities = n_distinct(municipality_code),
      n_years = n_distinct(year),
      mean_sgi = mean(sgi, na.rm = TRUE),
      median_sgi = median(sgi, na.rm = TRUE),
      sd_sgi = sd(sgi, na.rm = TRUE)
    )
  
  return(list(
    annual = annual_stats,
    overall = overall_stats
  ))
}

################################################################################
# 7. Main Execution
################################################################################

main <- function() {
  cat("Starting Copeland ranking and SGI calculation...\n")
  cat(rep("=", 80), "\n", sep = "")
  
  # Load indicators
  indicators_data <- load_indicators()
  
  # Apply Copeland ranking
  ranked_data <- apply_copeland_ranking(
    indicators_data,
    indicators = c("gdp_per_capita_std", "gdp_density_std", "population_density_std")
  )
  
  # Calculate Social Growth Index
  sgi_data <- calculate_sgi(ranked_data)
  
  # Identify performers
  performers <- identify_performers(sgi_data, n_top = 50, n_bottom = 50)
  
  # Generate statistics
  statistics <- generate_sgi_statistics(sgi_data)
  
  # Save results
  cat("\nSaving results...\n")
  
  # Save full SGI dataset
  output_file <- file.path(PROJECT_ROOT, "data/processed", "sgi_final.rds")
  saveRDS(sgi_data, output_file)
  cat(sprintf("  - Full SGI dataset: %s\n", output_file))
  
  # Save as CSV
  write_csv(sgi_data %>% select(-contains("_std")),
           file.path(PROJECT_ROOT, "data/processed", "sgi_final.csv"))
  cat("  - CSV export: data/processed/sgi_final.csv\n")
  
  # Save performer lists
  write_csv(performers$top,
           file.path(PROJECT_ROOT, "results/tables", "top_performers.csv"))
  write_csv(performers$bottom,
           file.path(PROJECT_ROOT, "results/tables", "bottom_performers.csv"))
  write_csv(performers$consistent_top,
           file.path(PROJECT_ROOT, "results/tables", "consistent_top_performers.csv"))
  write_csv(performers$most_improved,
           file.path(PROJECT_ROOT, "results/tables", "most_improved.csv"))
  write_csv(performers$most_declined,
           file.path(PROJECT_ROOT, "results/tables", "most_declined.csv"))
  cat("  - Performer lists saved to results/tables/\n")
  
  # Save statistics
  write_csv(statistics$annual,
           file.path(PROJECT_ROOT, "results/tables", "sgi_annual_statistics.csv"))
  write_csv(statistics$overall,
           file.path(PROJECT_ROOT, "results/tables", "sgi_overall_statistics.csv"))
  cat("  - Statistics saved to results/tables/\n")
  
  # Print summary
  cat("\nSGI Summary Statistics:\n")
  cat(rep("-", 80), "\n", sep = "")
  print(statistics$annual)
  
  cat("\n")
  print(statistics$overall)
  
  cat(rep("=", 80), "\n", sep = "")
  cat("Copeland ranking and SGI calculation completed!\n")
}

# Run main function if script is executed directly
if (sys.nframe() == 0) {
  main()
}
