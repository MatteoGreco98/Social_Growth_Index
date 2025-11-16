# The Social Growth Index (SGI)

**Measuring Socioeconomic Resilience at the Municipal Level in Italy (2010-2022)**

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R Version](https://img.shields.io/badge/R-%E2%89%A5%204.0.0-blue)](https://www.r-project.org/)

## Overview

The Social Growth Index (SGI) is a composite measure of socioeconomic resilience for Italian municipalities. It integrates:

- **FRK-based spatial downscaling** of GDP from NUTS-2/NUTS-3 to municipal level
- **Copeland ranking** over three key indicators:
  - GDP density (economic productivity per km²)
  - GDP per capita (economic welfare)
  - Population density (urbanization and agglomeration)

This repository provides a complete, reproducible R-based research codebase for computing the SGI for approximately 7,900 Italian municipalities over the period 2010-2022.

## Key Features

- ✅ **Data Integration**: Load and clean ISTAT (Italian) and Eurostat data
- ✅ **Spatial Downscaling**: FRK-based GDP disaggregation from regional to municipal level
- ✅ **Standardized Indicators**: Z-score normalization and temporal analysis
- ✅ **Copeland Ranking**: Multi-criteria decision analysis for composite index
- ✅ **Rich Visualizations**: Maps, time series, distributions, and comparative analyses
- ✅ **Full Reproducibility**: Automated pipeline with comprehensive documentation

## Repository Structure

```
.
├── run_sgi_analysis.R          # Main pipeline script
├── DESCRIPTION                 # R package dependencies
├── README.md                   # This file
├── data/
│   ├── raw/                    # Raw data files (not in git)
│   │   └── README.md           # Data download instructions
│   └── processed/              # Processed datasets (generated)
├── scripts/
│   ├── 01_load_and_clean_data.R          # Data loading and cleaning
│   ├── 02_frk_gdp_downscaling.R          # FRK-based GDP downscaling
│   ├── 03_compute_indicators.R           # Standardized indicators
│   ├── 04_copeland_ranking.R             # Copeland ranking and SGI
│   └── 05_generate_visualizations.R      # Maps and plots
├── results/
│   ├── figures/                # Plots and charts (generated)
│   ├── maps/                   # Spatial visualizations (generated)
│   └── tables/                 # Summary statistics (generated)
└── docs/                       # Additional documentation

```

## Prerequisites

### Software Requirements

- **R** (≥ 4.0.0)
- **RStudio** (recommended but optional)
- Required R packages (installed automatically):
  - Data manipulation: `dplyr`, `tidyr`, `readr`, `readxl`
  - Spatial analysis: `sf`, `FRK`, `sp`, `Matrix`
  - Visualization: `ggplot2`, `tmap`, `viridis`, `corrplot`
  - Utilities: `here`, `httr`, `jsonlite`, `scales`, `knitr`

### Data Requirements

Raw data must be downloaded from official sources (see `data/raw/README.md`):

1. **ISTAT**: Municipal population data (2010-2022)
2. **Eurostat**: Regional GDP data (NUTS-2/NUTS-3)
3. **ISTAT**: Italian municipal boundaries (shapefiles)

## Installation

### 1. Clone the Repository

```bash
git clone https://github.com/MatteoGreco98/The-Social-Growth-Index-Measuring-Socioeconomic-Resilience-at-the-Municipal-Level-in-Italy.git
cd The-Social-Growth-Index-Measuring-Socioeconomic-Resilience-at-the-Municipal-Level-in-Italy
```

### 2. Install R Dependencies

Open R or RStudio in the project directory and run:

```r
# Install required packages
install.packages(c(
  "dplyr", "tidyr", "readr", "readxl",
  "sf", "FRK", "sp", "Matrix",
  "ggplot2", "tmap", "viridis", "corrplot", "scales",
  "here", "httr", "jsonlite", "knitr"
))
```

Alternatively, use the DESCRIPTION file:

```r
# Install devtools if needed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install dependencies from DESCRIPTION
devtools::install_deps()
```

### 3. Download Data

Follow the detailed instructions in `data/raw/README.md` to download:

- ISTAT population data for each year (2010-2022)
- Eurostat regional GDP data
- ISTAT municipal boundary shapefiles

Place all downloaded files in the `data/raw/` directory as specified.

## Usage

### Quick Start

Run the complete analysis pipeline:

```bash
# From terminal/command line
Rscript run_sgi_analysis.R
```

Or from R/RStudio:

```r
source("run_sgi_analysis.R")
```

### Step-by-Step Execution

Run individual analysis steps:

```r
# Set project root
library(here)
PROJECT_ROOT <- here::here()

# Step 1: Load and clean data
source("scripts/01_load_and_clean_data.R")

# Step 2: FRK-based GDP downscaling
source("scripts/02_frk_gdp_downscaling.R")

# Step 3: Compute standardized indicators
source("scripts/03_compute_indicators.R")

# Step 4: Apply Copeland ranking
source("scripts/04_copeland_ranking.R")

# Step 5: Generate visualizations
source("scripts/05_generate_visualizations.R")
```

### Pipeline Options

Skip specific steps (e.g., if data is already processed):

```bash
# Skip data loading step
Rscript run_sgi_analysis.R --skip 1

# Skip multiple steps
Rscript run_sgi_analysis.R --skip 1,2

# Show help
Rscript run_sgi_analysis.R --help
```

## Methodology

### 1. Data Loading and Cleaning

- Loads municipal population data from ISTAT (annual, 2010-2022)
- Loads regional GDP data from Eurostat (NUTS-2/NUTS-3)
- Loads municipal boundary shapefiles
- Standardizes municipality codes and handles missing values
- Creates municipality-NUTS mapping

### 2. FRK-Based GDP Downscaling

Fixed Rank Kriging (FRK) approach for spatial downscaling:

1. **Population-weighted allocation**: Distributes NUTS-3 GDP to municipalities proportionally to population
2. **Density adjustment**: Incorporates population density patterns
3. **Spatial smoothing**: Applies FRK-inspired local averaging with distance-based weights
4. **Conservation constraint**: Ensures total GDP at NUTS-3 level is preserved
5. **Outputs**: Municipal-level GDP, GDP per capita, GDP density

### 3. Indicator Standardization

- **Z-score standardization**: `(x - mean) / sd` for each year
- **Three key indicators**:
  - Population density (inhabitants/km²)
  - GDP per capita (€/inhabitant)
  - GDP density (€/km²)
- **Temporal analysis**: Year-over-year and cumulative changes from 2010 baseline
- **Composite measures**: Simple and weighted averages

### 4. Copeland Ranking

Multi-criteria decision analysis:

1. **Pairwise comparisons**: Each municipality compared against all others
2. **Indicator-wise wins**: Count indicators where municipality i > municipality j
3. **Copeland score**: Total wins minus total losses across all comparisons
4. **SGI calculation**: Normalize Copeland scores to 0-100 scale
5. **Categories**: Very Low (<20), Low (20-40), Medium (40-60), High (60-80), Very High (≥80)

### 5. Visualization

- **Spatial maps**: Choropleth maps of SGI and indicators (tmap)
- **Time series**: National trends with confidence intervals
- **Distributions**: Histograms and boxplots by category
- **Correlations**: Indicator correlation matrices
- **Rankings**: Top/bottom performers and changes over time

## Outputs

### Processed Data

- `data/processed/municipalities_cleaned.rds`: Clean municipal data
- `data/processed/gdp_downscaled.rds`: Downscaled GDP estimates
- `data/processed/indicators_standardized.rds`: Standardized indicators
- `data/processed/sgi_final.csv`: **Final SGI dataset** (main output)

### Tables

- `results/tables/sgi_annual_statistics.csv`: Annual SGI summary
- `results/tables/top_performers.csv`: Top 50 municipalities by year
- `results/tables/bottom_performers.csv`: Bottom 50 municipalities
- `results/tables/most_improved.csv`: Municipalities with largest SGI gains
- `results/tables/consistent_top_performers.csv`: Persistently high performers

### Visualizations

- `results/figures/sgi_time_series.png`: National SGI trends
- `results/figures/sgi_distribution_YYYY.png`: SGI distributions
- `results/figures/indicator_correlations_YYYY.png`: Correlation matrices
- `results/figures/top_20_performers.png`: Top performers bar chart
- `results/figures/sgi_changes.png`: Largest changes 2010-2022
- `results/maps/sgi_map_YYYY.png`: Spatial distribution maps

## Interpreting Results

### SGI Score Interpretation

- **80-100 (Very High)**: Exceptional socioeconomic resilience
- **60-80 (High)**: Strong performance across indicators
- **40-60 (Medium)**: Moderate/average resilience
- **20-40 (Low)**: Below-average performance
- **0-20 (Very Low)**: Significant challenges

### Key Insights

The SGI reveals:

- Spatial patterns of economic development across Italy
- Temporal dynamics of municipal resilience (2010-2022)
- Municipalities with sustained growth vs. decline
- Regional disparities in socioeconomic outcomes

## Customization

### Modify Indicator Weights

Edit `scripts/03_compute_indicators.R`:

```r
# Default: GDP per capita (0.4), GDP density (0.3), Pop density (0.3)
composite_weighted = 0.4 * gdp_per_capita_std + 
                    0.3 * gdp_density_std + 
                    0.3 * population_density_std
```

### Change Standardization Method

Edit `scripts/03_compute_indicators.R`:

```r
# Options: "zscore", "minmax", "rank"
standardized_data <- compute_standardized_indicators(
  data = gdp_data,
  method = "zscore"  # Change to "minmax" or "rank"
)
```

### Adjust FRK Parameters

Edit `scripts/02_frk_gdp_downscaling.R`:

```r
# Blend factor between population and density weights
gdp_municipal = total_gdp * (0.7 * pop_weight + 0.3 * density_weight)

# Smoothing blend factor
gdp_municipal_final = 0.6 * gdp_municipal + 0.4 * gdp_municipal_smoothed
```

## Troubleshooting

### Common Issues

**Issue**: "Cleaned data not found"
- **Solution**: Run `01_load_and_clean_data.R` first or check data files exist

**Issue**: "Shapefile not found"
- **Solution**: Download municipal boundaries from ISTAT (see `data/raw/README.md`)

**Issue**: Package installation errors
- **Solution**: Update R to ≥4.0.0 and install dependencies individually

**Issue**: Memory errors with large datasets
- **Solution**: Process years sequentially or increase R memory limit

### Getting Help

- Check script comments and function documentation
- Review error messages carefully
- Ensure all data files are in correct locations
- Verify R and package versions

## Citation

If you use this code or methodology in your research, please cite:

```bibtex
@software{sgi2024,
  author = {Social Growth Index Research Team},
  title = {The Social Growth Index: Measuring Socioeconomic Resilience at the Municipal Level in Italy},
  year = {2024},
  url = {https://github.com/MatteoGreco98/The-Social-Growth-Index-Measuring-Socioeconomic-Resilience-at-the-Municipal-Level-in-Italy}
}
```

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- **ISTAT**: Italian National Institute of Statistics (data source)
- **Eurostat**: European Statistical Office (GDP data)
- **FRK Package**: Fixed Rank Kriging methodology
- **R Community**: Open-source tools and packages

## Contact

For questions, suggestions, or collaborations:

- Open an issue on GitHub
- Contact: [repository maintainer]

## Version History

- **v1.0.0** (2024): Initial release with complete pipeline
  - Data loading and cleaning
  - FRK-based GDP downscaling
  - Copeland ranking methodology
  - Comprehensive visualizations
  - Full reproducibility

---

**Note**: This research codebase is designed for academic and research purposes. Users are responsible for complying with data source terms of use and citing appropriately.
