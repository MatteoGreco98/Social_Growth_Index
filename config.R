# Configuration file for SGI Analysis
# Edit values below to customize analysis parameters

################################################################################
# Data Configuration
################################################################################

# Years to analyze
YEARS_START <- 2010
YEARS_END <- 2022

# Data file paths (relative to project root)
DATA_RAW_DIR <- "data/raw"
DATA_PROCESSED_DIR <- "data/processed"
RESULTS_DIR <- "results"

################################################################################
# FRK Downscaling Parameters
################################################################################

# Spatial resolution for downscaling (degrees)
FRK_RESOLUTION <- 2

# Weight for population vs density in GDP allocation
POPULATION_WEIGHT <- 0.7
DENSITY_WEIGHT <- 0.3

# Blend factor for smoothing (original vs smoothed)
ORIGINAL_BLEND <- 0.6
SMOOTHED_BLEND <- 0.4

# Distance threshold for spatial smoothing (degrees)
DISTANCE_THRESHOLD <- 0.5

################################################################################
# Indicator Standardization
################################################################################

# Standardization method: "zscore", "minmax", or "rank"
STANDARDIZATION_METHOD <- "zscore"

# Composite indicator weights
WEIGHT_GDP_PER_CAPITA <- 0.4
WEIGHT_GDP_DENSITY <- 0.3
WEIGHT_POP_DENSITY <- 0.3

################################################################################
# Copeland Ranking
################################################################################

# Indicators to use in Copeland ranking
COPELAND_INDICATORS <- c(
  "gdp_per_capita_std",
  "gdp_density_std", 
  "population_density_std"
)

################################################################################
# Visualization Parameters
################################################################################

# Years to create maps for
MAP_YEARS <- c(2010, 2015, 2020, 2022)

# Number of top/bottom performers to show
N_TOP_PERFORMERS <- 50
N_BOTTOM_PERFORMERS <- 50

# Figure dimensions (width, height in inches)
FIGURE_WIDTH <- 10
FIGURE_HEIGHT <- 6

# Map dimensions (width, height in pixels)
MAP_WIDTH <- 3000
MAP_HEIGHT <- 2400

# DPI for outputs
OUTPUT_DPI <- 300

# Color palette for maps
MAP_PALETTE <- "viridis"

################################################################################
# Performance Thresholds
################################################################################

# SGI category thresholds
SGI_VERY_HIGH_THRESHOLD <- 80
SGI_HIGH_THRESHOLD <- 60
SGI_MEDIUM_THRESHOLD <- 40
SGI_LOW_THRESHOLD <- 20

################################################################################
# Output Options
################################################################################

# Save intermediate results
SAVE_INTERMEDIATE <- TRUE

# Generate all visualizations
GENERATE_MAPS <- TRUE
GENERATE_PLOTS <- TRUE

# Export formats
EXPORT_CSV <- TRUE
EXPORT_RDS <- TRUE
