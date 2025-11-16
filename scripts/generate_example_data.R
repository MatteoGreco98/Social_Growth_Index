#!/usr/bin/env Rscript
################################################################################
# Script: generate_example_data.R
# Purpose: Generate synthetic example data for testing the SGI pipeline
# Author: Social Growth Index Research Team
# Date: 2024
# Note: This is for testing purposes only - real data should be downloaded
################################################################################

library(dplyr)
library(readr)
library(sf)

# Set seed for reproducibility
set.seed(42)

# Set project root
if (!exists("PROJECT_ROOT")) {
  PROJECT_ROOT <- here::here()
}

cat("Generating example data for SGI pipeline testing...\n")
cat("Note: This is synthetic data - for real analysis, download actual data.\n\n")

################################################################################
# Generate Example Municipality Data
################################################################################

n_municipalities <- 100  # Small sample for testing
years <- 2010:2022

cat("Generating municipality population data...\n")

# Create base municipality list
municipalities <- data.frame(
  municipality_code = sprintf("%06d", 1001:(1000 + n_municipalities)),
  municipality_name = paste("Municipality", 1:n_municipalities),
  region = sample(c("North", "Center", "South"), n_municipalities, replace = TRUE)
)

# Generate population data for each year
for (year in years) {
  pop_data <- municipalities %>%
    mutate(
      # Base population with some variation
      population = round(rnorm(n_municipalities, mean = 10000, sd = 5000)),
      # Add year trend
      population = pmax(1000, population + (year - 2010) * rnorm(n_municipalities, mean = 50, sd = 100)),
      year = year
    ) %>%
    select(municipality_code, municipality_name, population, year)
  
  # Save to CSV
  output_file <- file.path(PROJECT_ROOT, "data/raw", 
                          paste0("popolazione_comuni_", year, ".csv"))
  write_csv(pop_data, output_file)
  cat(sprintf("  - Created: popolazione_comuni_%d.csv\n", year))
}

################################################################################
# Generate Example GDP Data (NUTS3 level)
################################################################################

cat("\nGenerating regional GDP data...\n")

# Create NUTS3 codes based on municipality regions
nuts3_regions <- data.frame(
  nuts3_code = c("ITA11", "ITA12", "ITA13"),
  region_name = c("North", "Center", "South")
)

# Generate GDP data
gdp_data <- expand.grid(
  nuts3_code = nuts3_regions$nuts3_code,
  year = years
) %>%
  left_join(nuts3_regions, by = "nuts3_code") %>%
  mutate(
    # Base GDP in millions of euros
    gdp = case_when(
      region_name == "North" ~ rnorm(n(), mean = 50000, sd = 10000),
      region_name == "Center" ~ rnorm(n(), mean = 35000, sd = 7000),
      region_name == "South" ~ rnorm(n(), mean = 25000, sd = 5000)
    ),
    # Add year trend
    gdp = gdp * (1 + (year - 2010) * 0.02),
    gdp = pmax(1000, gdp),
    nuts_level = "NUTS3"
  ) %>%
  select(nuts3_code, year, gdp, nuts_level)

# Save GDP data
output_file <- file.path(PROJECT_ROOT, "data/raw", "gdp_regional_NUTS2_NUTS3.csv")
write_csv(gdp_data, output_file)
cat("  - Created: gdp_regional_NUTS2_NUTS3.csv\n")

################################################################################
# Generate Example Spatial Boundaries
################################################################################

cat("\nGenerating municipal boundaries...\n")

# Create simple polygon boundaries (grid)
n_cols <- 10
n_rows <- ceiling(n_municipalities / n_cols)

boundaries_list <- list()

for (i in 1:n_municipalities) {
  row <- (i - 1) %/% n_cols
  col <- (i - 1) %% n_cols
  
  # Create a simple square polygon
  lon_min <- 8 + col * 0.5
  lon_max <- lon_min + 0.4
  lat_min <- 40 + row * 0.5
  lat_max <- lat_min + 0.4
  
  polygon <- st_polygon(list(matrix(c(
    lon_min, lat_min,
    lon_max, lat_min,
    lon_max, lat_max,
    lon_min, lat_max,
    lon_min, lat_min
  ), ncol = 2, byrow = TRUE)))
  
  boundaries_list[[i]] <- st_sfc(polygon, crs = 4326)
}

# Combine into sf object
boundaries <- st_sf(
  municipality_code = municipalities$municipality_code,
  municipality_name = municipalities$municipality_name,
  geometry = do.call(c, boundaries_list)
)

# Calculate areas
boundaries <- boundaries %>%
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6)

# Create boundaries directory
dir.create(file.path(PROJECT_ROOT, "data/raw/boundaries"), 
          recursive = TRUE, showWarnings = FALSE)

# Save as GeoPackage (simpler than shapefile)
output_file <- file.path(PROJECT_ROOT, "data/raw/boundaries", 
                        "Com01012022_g_WGS84.gpkg")
st_write(boundaries, output_file, delete_dsn = TRUE, quiet = TRUE)
cat("  - Created: boundaries/Com01012022_g_WGS84.gpkg\n")

# Also create a simple shapefile
shp_file <- file.path(PROJECT_ROOT, "data/raw/boundaries", 
                     "Com01012022_g_WGS84.shp")
st_write(boundaries, shp_file, delete_dsn = TRUE, quiet = TRUE)
cat("  - Created: boundaries/Com01012022_g_WGS84.shp\n")

################################################################################
# Summary
################################################################################

cat("\n")
cat(rep("=", 80), "\n", sep = "")
cat("Example data generation completed!\n")
cat(rep("=", 80), "\n", sep = "")
cat("\nGenerated files:\n")
cat(sprintf("  - %d population CSV files (2010-2022)\n", length(years)))
cat("  - 1 GDP data file (NUTS3 level)\n")
cat("  - 1 boundary file (GeoPackage and Shapefile)\n")
cat("\nTotal municipalities: ", n_municipalities, "\n")
cat("Years covered: ", min(years), "-", max(years), "\n")
cat("\n")
cat("You can now run the SGI analysis pipeline:\n")
cat("  Rscript run_sgi_analysis.R\n")
cat("\n")
cat("IMPORTANT: This is synthetic data for testing only!\n")
cat("For real analysis, download actual data from ISTAT and Eurostat.\n")
cat(rep("=", 80), "\n", sep = "")
