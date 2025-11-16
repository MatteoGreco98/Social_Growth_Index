#!/usr/bin/env Rscript
################################################################################
# Script: 01_load_and_clean_data.R
# Purpose: Load and clean ISTAT/Eurostat data for SGI analysis
# Author: Social Growth Index Research Team
# Date: 2024
################################################################################

# Load required libraries
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(sf)
library(stringr)

# Set working directory to project root
if (!exists("PROJECT_ROOT")) {
  PROJECT_ROOT <- here::here()
}

# Create output directory if it doesn't exist
dir.create(file.path(PROJECT_ROOT, "data/processed"), 
           recursive = TRUE, showWarnings = FALSE)

################################################################################
# 1. Load Municipal Population Data (ISTAT)
################################################################################

load_population_data <- function(years = 2010:2022) {
  cat("Loading population data...\n")
  
  pop_list <- list()
  
  for (year in years) {
    file_path <- file.path(PROJECT_ROOT, "data/raw", 
                          paste0("popolazione_comuni_", year, ".csv"))
    
    if (file.exists(file_path)) {
      cat(sprintf("  - Loading %d data\n", year))
      
      # Read data with appropriate encoding
      pop_data <- tryCatch({
        read_csv(file_path, 
                locale = locale(encoding = "UTF-8"),
                show_col_types = FALSE) %>%
          mutate(year = year)
      }, error = function(e) {
        # Try alternative encoding
        read_csv2(file_path, 
                 locale = locale(encoding = "latin1"),
                 show_col_types = FALSE) %>%
          mutate(year = year)
      })
      
      # Standardize column names
      pop_data <- pop_data %>%
        rename_with(tolower) %>%
        rename(
          municipality_code = matches("^(pro_com|codice|cod_istat)"),
          municipality_name = matches("^(comune|denominazione)"),
          population = matches("^(popolazione|pop|totale)")
        )
      
      pop_list[[as.character(year)]] <- pop_data
    } else {
      warning(sprintf("Population data file not found for year %d", year))
    }
  }
  
  # Combine all years
  if (length(pop_list) > 0) {
    population_data <- bind_rows(pop_list) %>%
      select(year, municipality_code, municipality_name, population) %>%
      mutate(
        municipality_code = as.character(municipality_code),
        population = as.numeric(population)
      ) %>%
      filter(!is.na(population), population > 0)
    
    cat(sprintf("Loaded population data: %d observations\n", nrow(population_data)))
    return(population_data)
  } else {
    warning("No population data files found!")
    return(NULL)
  }
}

################################################################################
# 2. Load Regional GDP Data (Eurostat)
################################################################################

load_gdp_data <- function() {
  cat("\nLoading regional GDP data...\n")
  
  file_path <- file.path(PROJECT_ROOT, "data/raw", 
                        "gdp_regional_NUTS2_NUTS3.csv")
  
  if (!file.exists(file_path)) {
    warning("GDP data file not found!")
    return(NULL)
  }
  
  # Read GDP data
  gdp_data <- tryCatch({
    read_csv(file_path, 
            locale = locale(encoding = "UTF-8"),
            show_col_types = FALSE)
  }, error = function(e) {
    read_csv2(file_path, 
             locale = locale(encoding = "latin1"),
             show_col_types = FALSE)
  })
  
  # Standardize column names
  gdp_data <- gdp_data %>%
    rename_with(tolower) %>%
    rename(
      nuts_code = matches("^(geo|nuts|codice)"),
      year = matches("^(time|anno|year)"),
      gdp = matches("^(gdp|pil|value)")
    )
  
  # Clean and filter
  gdp_data <- gdp_data %>%
    mutate(
      nuts_code = as.character(nuts_code),
      year = as.integer(year),
      gdp = as.numeric(gdp),
      nuts_level = case_when(
        nchar(nuts_code) == 3 ~ "NUTS1",
        nchar(nuts_code) == 4 ~ "NUTS2",
        nchar(nuts_code) == 5 ~ "NUTS3",
        TRUE ~ "Other"
      )
    ) %>%
    filter(!is.na(gdp), gdp > 0, nuts_level %in% c("NUTS2", "NUTS3"))
  
  cat(sprintf("Loaded GDP data: %d observations\n", nrow(gdp_data)))
  return(gdp_data)
}

################################################################################
# 3. Load Municipal Boundaries (Shapefile)
################################################################################

load_boundaries <- function() {
  cat("\nLoading municipal boundaries...\n")
  
  shp_path <- file.path(PROJECT_ROOT, "data/raw/boundaries", 
                       "Com01012022_g_WGS84.shp")
  
  if (!file.exists(shp_path)) {
    warning("Shapefile not found!")
    return(NULL)
  }
  
  # Read shapefile
  boundaries <- st_read(shp_path, quiet = TRUE) %>%
    st_transform(4326)  # Ensure WGS84
  
  # Standardize column names
  boundaries <- boundaries %>%
    rename_with(tolower) %>%
    rename(
      municipality_code = matches("^(pro_com|codice|cod_istat)"),
      municipality_name = matches("^(comune|denominazione)")
    ) %>%
    mutate(
      municipality_code = as.character(municipality_code),
      area_km2 = as.numeric(st_area(.)) / 1e6  # Convert to km²
    )
  
  cat(sprintf("Loaded boundaries: %d municipalities\n", nrow(boundaries)))
  return(boundaries)
}

################################################################################
# 4. Create Municipality-NUTS Mapping
################################################################################

create_municipality_nuts_mapping <- function(boundaries, gdp_data) {
  cat("\nCreating municipality-NUTS mapping...\n")
  
  if (is.null(boundaries) || is.null(gdp_data)) {
    warning("Cannot create mapping: missing data")
    return(NULL)
  }
  
  # Extract NUTS codes from municipality codes
  # Italian municipality codes: first 3 digits = province (NUTS3 related)
  municipality_nuts <- boundaries %>%
    st_drop_geometry() %>%
    select(municipality_code, municipality_name) %>%
    mutate(
      province_code = substr(municipality_code, 1, 3),
      # Map province codes to NUTS3 codes (simplified)
      nuts3_code = paste0("IT", substr(municipality_code, 1, 1), 
                         substr(municipality_code, 2, 3))
    ) %>%
    distinct()
  
  cat(sprintf("Created mapping for %d municipalities\n", 
              nrow(municipality_nuts)))
  return(municipality_nuts)
}

################################################################################
# 5. Clean and Merge All Data
################################################################################

clean_and_merge_data <- function(population_data, gdp_data, boundaries, 
                                municipality_nuts) {
  cat("\nMerging all datasets...\n")
  
  # Merge population with municipality info
  merged_data <- population_data %>%
    left_join(
      boundaries %>% 
        st_drop_geometry() %>% 
        select(municipality_code, area_km2),
      by = "municipality_code"
    )
  
  # Add NUTS codes
  if (!is.null(municipality_nuts)) {
    merged_data <- merged_data %>%
      left_join(
        municipality_nuts %>% 
          select(municipality_code, nuts3_code),
        by = "municipality_code"
      )
  }
  
  # Add GDP data (at NUTS3 level for downscaling)
  if (!is.null(gdp_data)) {
    gdp_nuts3 <- gdp_data %>%
      filter(nuts_level == "NUTS3") %>%
      select(nuts3_code = nuts_code, year, gdp_nuts3 = gdp)
    
    merged_data <- merged_data %>%
      left_join(gdp_nuts3, by = c("nuts3_code", "year"))
  }
  
  # Calculate basic indicators
  merged_data <- merged_data %>%
    mutate(
      population_density = population / area_km2
    ) %>%
    filter(!is.na(area_km2), area_km2 > 0)
  
  cat(sprintf("Merged data: %d observations\n", nrow(merged_data)))
  return(merged_data)
}

################################################################################
# 6. Main Execution
################################################################################

main <- function() {
  cat("Starting data loading and cleaning...\n")
  cat(rep("=", 80), "\n", sep = "")
  
  # Load all data sources
  population_data <- load_population_data(2010:2022)
  gdp_data <- load_gdp_data()
  boundaries <- load_boundaries()
  
  # Create mappings
  municipality_nuts <- create_municipality_nuts_mapping(boundaries, gdp_data)
  
  # Merge and clean
  if (!is.null(population_data)) {
    cleaned_data <- clean_and_merge_data(
      population_data, gdp_data, boundaries, municipality_nuts
    )
    
    # Save cleaned data
    output_file <- file.path(PROJECT_ROOT, "data/processed", 
                            "municipalities_cleaned.rds")
    saveRDS(cleaned_data, output_file)
    cat(sprintf("\nCleaned data saved to: %s\n", output_file))
    
    # Save as CSV for inspection
    write_csv(cleaned_data, 
             file.path(PROJECT_ROOT, "data/processed", 
                      "municipalities_cleaned.csv"))
    
    # Save boundaries separately
    if (!is.null(boundaries)) {
      st_write(boundaries, 
              file.path(PROJECT_ROOT, "data/processed", 
                       "municipalities_boundaries.gpkg"),
              delete_dsn = TRUE, quiet = TRUE)
      cat("Boundaries saved to: data/processed/municipalities_boundaries.gpkg\n")
    }
  }
  
  cat(rep("=", 80), "\n", sep = "")
  cat("Data loading and cleaning completed!\n")
}

# Run main function if script is executed directly
if (sys.nframe() == 0) {
  main()
}
