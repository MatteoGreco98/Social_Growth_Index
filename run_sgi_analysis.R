#!/usr/bin/env Rscript
################################################################################
# Script: run_sgi_analysis.R
# Purpose: Main pipeline for Social Growth Index analysis
# Author: Social Growth Index Research Team
# Date: 2024
################################################################################

# Load required libraries
library(here)

# Set project root
PROJECT_ROOT <- here::here()

cat("\n")
cat(rep("=", 80), "\n", sep = "")
cat("SOCIAL GROWTH INDEX (SGI) ANALYSIS PIPELINE\n")
cat("Measuring Socioeconomic Resilience at the Municipal Level in Italy\n")
cat("Period: 2010-2022\n")
cat(rep("=", 80), "\n", sep = "")
cat("\n")

# Check if data exists
data_exists <- file.exists(file.path(PROJECT_ROOT, "data/raw", 
                                     "popolazione_comuni_2020.csv"))

if (!data_exists) {
  cat("WARNING: Raw data files not found!\n")
  cat("Please download the required data files following instructions in:\n")
  cat("  data/raw/README.md\n\n")
  cat("The pipeline will continue but may fail if data is missing.\n\n")
  Sys.sleep(3)
}

################################################################################
# Pipeline Steps
################################################################################

run_pipeline <- function(skip_steps = NULL) {
  """
  Run the complete SGI analysis pipeline
  
  Args:
    skip_steps: vector of step numbers to skip (e.g., c(1, 2))
  """
  
  steps <- list(
    list(
      number = 1,
      name = "Data Loading and Cleaning",
      script = "scripts/01_load_and_clean_data.R"
    ),
    list(
      number = 2,
      name = "FRK-based GDP Downscaling",
      script = "scripts/02_frk_gdp_downscaling.R"
    ),
    list(
      number = 3,
      name = "Indicator Computation",
      script = "scripts/03_compute_indicators.R"
    ),
    list(
      number = 4,
      name = "Copeland Ranking and SGI Calculation",
      script = "scripts/04_copeland_ranking.R"
    ),
    list(
      number = 5,
      name = "Visualization Generation",
      script = "scripts/05_generate_visualizations.R"
    )
  )
  
  # Track timing
  total_start_time <- Sys.time()
  step_times <- list()
  
  for (step in steps) {
    # Check if step should be skipped
    if (!is.null(skip_steps) && step$number %in% skip_steps) {
      cat(sprintf("SKIPPING Step %d: %s\n\n", step$number, step$name))
      next
    }
    
    cat("\n")
    cat(rep("-", 80), "\n", sep = "")
    cat(sprintf("Step %d: %s\n", step$number, step$name))
    cat(rep("-", 80), "\n", sep = "")
    
    step_start_time <- Sys.time()
    
    # Run the script
    tryCatch({
      script_path <- file.path(PROJECT_ROOT, step$script)
      
      if (!file.exists(script_path)) {
        stop(sprintf("Script not found: %s", step$script))
      }
      
      source(script_path)
      
      step_end_time <- Sys.time()
      step_duration <- difftime(step_end_time, step_start_time, units = "secs")
      step_times[[step$name]] <- step_duration
      
      cat("\n")
      cat(sprintf("Step %d completed in %.1f seconds\n", 
                 step$number, as.numeric(step_duration)))
      
    }, error = function(e) {
      cat("\n")
      cat(sprintf("ERROR in Step %d: %s\n", step$number, step$name))
      cat(sprintf("Error message: %s\n", e$message))
      cat("\n")
      
      # Ask user if they want to continue
      cat("Do you want to continue with remaining steps? (y/n): ")
      response <- readline()
      
      if (tolower(response) != "y") {
        stop("Pipeline aborted by user")
      }
    })
  }
  
  total_end_time <- Sys.time()
  total_duration <- difftime(total_end_time, total_start_time, units = "mins")
  
  # Print summary
  cat("\n")
  cat(rep("=", 80), "\n", sep = "")
  cat("PIPELINE EXECUTION SUMMARY\n")
  cat(rep("=", 80), "\n", sep = "")
  
  for (name in names(step_times)) {
    cat(sprintf("  %s: %.1f seconds\n", name, as.numeric(step_times[[name]])))
  }
  
  cat(rep("-", 80), "\n", sep = "")
  cat(sprintf("Total execution time: %.1f minutes\n", as.numeric(total_duration)))
  cat(rep("=", 80), "\n", sep = "")
  cat("\n")
}

################################################################################
# Command Line Interface
################################################################################

parse_arguments <- function() {
  """Parse command line arguments"""
  
  args <- commandArgs(trailingOnly = TRUE)
  
  config <- list(
    skip_steps = NULL,
    help = FALSE
  )
  
  if (length(args) > 0) {
    for (i in seq_along(args)) {
      if (args[i] == "--help" || args[i] == "-h") {
        config$help <- TRUE
      } else if (args[i] == "--skip") {
        if (i < length(args)) {
          skip_str <- args[i + 1]
          config$skip_steps <- as.numeric(unlist(strsplit(skip_str, ",")))
        }
      }
    }
  }
  
  return(config)
}

print_help <- function() {
  cat("\n")
  cat("USAGE: Rscript run_sgi_analysis.R [OPTIONS]\n")
  cat("\n")
  cat("OPTIONS:\n")
  cat("  --help, -h        Show this help message\n")
  cat("  --skip STEPS      Skip specified steps (comma-separated numbers)\n")
  cat("                    Example: --skip 1,2\n")
  cat("\n")
  cat("PIPELINE STEPS:\n")
  cat("  1. Data Loading and Cleaning\n")
  cat("  2. FRK-based GDP Downscaling\n")
  cat("  3. Indicator Computation\n")
  cat("  4. Copeland Ranking and SGI Calculation\n")
  cat("  5. Visualization Generation\n")
  cat("\n")
  cat("EXAMPLES:\n")
  cat("  # Run complete pipeline\n")
  cat("  Rscript run_sgi_analysis.R\n")
  cat("\n")
  cat("  # Skip data loading (if already done)\n")
  cat("  Rscript run_sgi_analysis.R --skip 1\n")
  cat("\n")
  cat("  # Skip visualization generation\n")
  cat("  Rscript run_sgi_analysis.R --skip 5\n")
  cat("\n")
}

################################################################################
# Main Execution
################################################################################

main <- function() {
  # Parse arguments
  config <- parse_arguments()
  
  # Show help if requested
  if (config$help) {
    print_help()
    return(invisible(NULL))
  }
  
  # Run pipeline
  cat("Starting SGI analysis pipeline...\n")
  
  if (!is.null(config$skip_steps)) {
    cat(sprintf("Skipping steps: %s\n", 
               paste(config$skip_steps, collapse = ", ")))
  }
  
  run_pipeline(skip_steps = config$skip_steps)
  
  cat("\n")
  cat(rep("=", 80), "\n", sep = "")
  cat("SGI ANALYSIS COMPLETED SUCCESSFULLY!\n")
  cat(rep("=", 80), "\n", sep = "")
  cat("\n")
  cat("Results are available in:\n")
  cat("  - data/processed/    (processed datasets)\n")
  cat("  - results/tables/    (summary tables and statistics)\n")
  cat("  - results/figures/   (plots and charts)\n")
  cat("  - results/maps/      (spatial visualizations)\n")
  cat("\n")
  cat("Key output files:\n")
  cat("  - data/processed/sgi_final.csv\n")
  cat("  - results/tables/sgi_annual_statistics.csv\n")
  cat("  - results/tables/top_performers.csv\n")
  cat("  - results/figures/sgi_time_series.png\n")
  cat("  - results/maps/sgi_map_2020.png\n")
  cat("\n")
}

# Run main function if script is executed directly
if (!interactive()) {
  main()
}
