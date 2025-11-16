# Changelog

All notable changes to the Social Growth Index (SGI) project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0] - 2024

### Added

**Core Functionality**
- Complete R-based research codebase for Social Growth Index analysis
- Data loading and cleaning pipeline for ISTAT/Eurostat data
- FRK-based spatial downscaling of GDP from NUTS-3 to municipal level
- Standardized indicator computation (GDP density, GDP per capita, population density)
- Copeland ranking algorithm for multi-criteria composite index
- Comprehensive visualization suite (maps, time series, distributions, correlations)

**Scripts**
- `scripts/01_load_and_clean_data.R` - Data integration and preprocessing
- `scripts/02_frk_gdp_downscaling.R` - Fixed Rank Kriging implementation
- `scripts/03_compute_indicators.R` - Indicator standardization and temporal analysis
- `scripts/04_copeland_ranking.R` - Copeland method and SGI calculation
- `scripts/05_generate_visualizations.R` - Maps and plots generation
- `scripts/generate_example_data.R` - Synthetic test data generator
- `run_sgi_analysis.R` - Main pipeline orchestrator with CLI

**Configuration & Management**
- `config.R` - Centralized parameter configuration
- `DESCRIPTION` - R package dependencies specification
- `.gitignore` - Data and output exclusions

**Documentation**
- `README.md` - Comprehensive project documentation
- `QUICKSTART.md` - 5-minute quick start guide
- `docs/METHODOLOGY.md` - Detailed technical methodology
- `data/raw/README.md` - Data download instructions
- `CONTRIBUTING.md` - Contribution guidelines
- `LICENSE` - MIT License
- `CHANGELOG.md` - This file

**Project Structure**
- Organized directory structure: `data/`, `scripts/`, `results/`, `docs/`
- Separation of raw and processed data
- Automated output organization (figures, maps, tables)

### Features

**Data Integration**
- Loads annual municipal population data from ISTAT (2010-2022)
- Integrates regional GDP data from Eurostat (NUTS-2/NUTS-3)
- Processes Italian municipal boundary shapefiles
- Handles missing values and standardizes municipality codes
- Creates municipality-NUTS mapping

**FRK-Based GDP Downscaling**
- Population-weighted GDP allocation
- Density-based spatial adjustment
- Distance-weighted spatial smoothing
- Mass conservation at NUTS-3 level
- Produces municipal-level GDP, GDP per capita, and GDP density

**Indicator Standardization**
- Z-score standardization (configurable to min-max or rank-based)
- Year-over-year change analysis
- Cumulative changes from 2010 baseline
- Composite indicator calculation with configurable weights
- Percentile rank computation

**Copeland Ranking**
- Pairwise comparison across all municipalities
- Multi-criteria decision analysis
- Copeland score calculation (wins - losses)
- SGI normalization to 0-100 scale
- Five-tier categorization (Very Low to Very High)

**Visualizations**
- Choropleth maps of SGI and indicators (using tmap)
- Time series plots with confidence intervals
- Distribution histograms and boxplots
- Indicator correlation matrices
- Top performer comparisons
- Change analysis plots (2010-2022)

**Analysis Outputs**
- Complete SGI dataset (CSV and RDS formats)
- Top/bottom 50 performers by year
- Consistent top performers identification
- Most improved/declined municipalities
- Annual and overall summary statistics
- Regional aggregations

**Reproducibility**
- Automated pipeline from raw data to results
- Configurable parameters for sensitivity analysis
- Example data generation for testing
- Comprehensive documentation
- Clear data requirements and sourcing

**Usability**
- Command-line interface with options
- Modular script design (run individually or as pipeline)
- Progress reporting and error handling
- Summary statistics output
- Execution time tracking

### Technical Details

**Algorithms Implemented**
- Fixed Rank Kriging (FRK) spatial downscaling
- Copeland tournament ranking
- Z-score/min-max/rank standardization methods
- Distance-weighted spatial smoothing
- Pairwise comparison logic

**Dependencies**
- Data manipulation: dplyr, tidyr, readr, readxl
- Spatial analysis: sf, FRK, sp, Matrix
- Visualization: ggplot2, tmap, viridis, corrplot, scales
- Utilities: here, httr, jsonlite, knitr

**Performance**
- Processes ~7,900 municipalities × 13 years
- Typical execution time: 10-20 minutes
- Memory requirements: 2-4 GB RAM
- Scalable to larger datasets

**Validation**
- Mass conservation in GDP downscaling
- Consistency checks across years
- Temporal trend validation
- Spatial pattern verification

### Known Limitations

- GDP downscaling relies on population/density proxies (inherent uncertainty)
- Copeland ranking is ordinal (doesn't capture magnitude of differences)
- Missing data handling may affect some municipalities
- Shapefile changes over time not fully addressed
- No formal uncertainty quantification (future work)

### Future Enhancements (Planned)

- Unit tests for all core functions
- Confidence intervals for downscaled GDP
- Alternative ranking methods (Borda, Kemeny)
- Interactive visualizations (Shiny app)
- Time series clustering analysis
- Regional comparative analysis tools
- Automated data fetching from APIs
- Parallel processing for large datasets
- More sophisticated FRK implementation
- Robustness checks and sensitivity analysis automation

---

## Version History

### [1.0.0] - 2024-01-XX
- Initial release
- Complete SGI pipeline implementation
- Full documentation and reproducibility

---

## How to Use This Changelog

This changelog documents:
- **Added**: New features
- **Changed**: Changes to existing functionality
- **Deprecated**: Soon-to-be removed features
- **Removed**: Removed features
- **Fixed**: Bug fixes
- **Security**: Security vulnerability fixes

For each version, the most recent changes are listed first.

---

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for how to suggest changes or report issues.

## References

For methodology details, see [docs/METHODOLOGY.md](docs/METHODOLOGY.md).
