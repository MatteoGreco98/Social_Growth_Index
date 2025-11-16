# Quick Start Guide - Social Growth Index (SGI)

This guide will help you get started with the SGI analysis in 5 minutes.

## Prerequisites

- R (≥ 4.0.0) installed
- Basic familiarity with R and command line

## Option 1: Test with Example Data (Fastest)

Perfect for testing the pipeline without downloading large datasets.

### Step 1: Clone Repository

```bash
git clone https://github.com/MatteoGreco98/The-Social-Growth-Index-Measuring-Socioeconomic-Resilience-at-the-Municipal-Level-in-Italy.git
cd The-Social-Growth-Index-Measuring-Socioeconomic-Resilience-at-the-Municipal-Level-in-Italy
```

### Step 2: Install Dependencies

Open R in the project directory:

```r
# Install required packages
install.packages(c(
  "dplyr", "tidyr", "readr", "sf", "FRK", 
  "ggplot2", "tmap", "viridis", "here"
))
```

### Step 3: Generate Example Data

```bash
Rscript scripts/generate_example_data.R
```

This creates synthetic data for 100 municipalities (2010-2022).

### Step 4: Run Analysis

```bash
Rscript run_sgi_analysis.R
```

### Step 5: View Results

Check the outputs:

```bash
# View final SGI data
head data/processed/sgi_final.csv

# View top performers
head results/tables/top_performers.csv

# Open visualizations
ls results/figures/
ls results/maps/
```

**Estimated time**: ~2-5 minutes

---

## Option 2: Full Analysis with Real Data

For actual research with official ISTAT/Eurostat data.

### Step 1: Clone Repository

Same as Option 1.

### Step 2: Install Dependencies

Same as Option 1.

### Step 3: Download Data

Follow detailed instructions in `data/raw/README.md`:

1. **ISTAT Population Data** (2010-2022)
   - Visit: http://dati.istat.it/
   - Download annual municipal population files
   - Save as: `data/raw/popolazione_comuni_YYYY.csv`

2. **Eurostat GDP Data**
   - Visit: https://ec.europa.eu/eurostat/data/database
   - Download regional GDP (NUTS-2/NUTS-3)
   - Save as: `data/raw/gdp_regional_NUTS2_NUTS3.csv`

3. **Municipal Boundaries**
   - Visit: https://www.istat.it/it/archivio/222527
   - Download shapefiles
   - Save to: `data/raw/boundaries/`

### Step 4: Run Analysis

```bash
Rscript run_sgi_analysis.R
```

### Step 5: Explore Results

Results will be in:
- `data/processed/sgi_final.csv` - Complete SGI dataset
- `results/tables/` - Summary statistics and rankings
- `results/figures/` - Plots and charts
- `results/maps/` - Spatial visualizations

**Estimated time**: ~10-20 minutes (depending on data download)

---

## Understanding the Output

### Key Files

1. **sgi_final.csv**: Main output with SGI scores for all municipalities and years
   - Columns: municipality_code, municipality_name, year, sgi, sgi_rank, sgi_category

2. **top_performers.csv**: Top 50 municipalities by year
3. **most_improved.csv**: Municipalities with largest SGI gains (2010-2022)
4. **sgi_time_series.png**: National SGI trends over time
5. **sgi_map_2020.png**: Spatial distribution of SGI

### Interpreting SGI Scores

- **80-100**: Very High resilience
- **60-80**: High resilience  
- **40-60**: Medium resilience
- **20-40**: Low resilience
- **0-20**: Very Low resilience

---

## Common Workflows

### Run Specific Steps Only

```bash
# Skip data loading (if already done)
Rscript run_sgi_analysis.R --skip 1

# Skip visualization
Rscript run_sgi_analysis.R --skip 5
```

### Run Individual Scripts

```r
# In R console
PROJECT_ROOT <- here::here()

# Run step by step
source("scripts/01_load_and_clean_data.R")
source("scripts/02_frk_gdp_downscaling.R")
source("scripts/03_compute_indicators.R")
source("scripts/04_copeland_ranking.R")
source("scripts/05_generate_visualizations.R")
```

### Customize Parameters

Edit `config.R` to change:
- Indicator weights
- Standardization method
- FRK parameters
- Visualization options

---

## Troubleshooting

**Problem**: Package installation fails

```r
# Try installing one at a time
install.packages("dplyr")
install.packages("sf")  # May need system dependencies
```

**Problem**: "Data not found" errors

- Check files are in `data/raw/` directory
- Verify file names match expected format
- Run `generate_example_data.R` for testing

**Problem**: Memory errors

- Process fewer years
- Use data.table for large files
- Increase R memory limit

**Problem**: Spatial operations fail

```r
# Install system dependencies (Ubuntu/Debian)
sudo apt-get install libgdal-dev libgeos-dev libproj-dev
```

---

## Next Steps

1. **Explore Results**: Open CSV files and visualizations
2. **Read Documentation**: See `docs/METHODOLOGY.md` for details
3. **Customize Analysis**: Modify weights in `config.R`
4. **Extend Pipeline**: Add new indicators or visualizations
5. **Share Results**: Export tables and figures for publication

---

## Getting Help

- Check the main README.md for detailed documentation
- Review script comments for function descriptions
- Open an issue on GitHub for questions
- Consult `docs/METHODOLOGY.md` for technical details

---

## Example R Session

```r
# Load results
library(dplyr)
library(ggplot2)

# Read final SGI data
sgi <- read.csv("data/processed/sgi_final.csv")

# Quick exploration
summary(sgi$sgi)
table(sgi$sgi_category)

# Top 10 municipalities in 2022
top_2022 <- sgi %>%
  filter(year == 2022) %>%
  arrange(desc(sgi)) %>%
  head(10)

print(top_2022)

# Plot SGI distribution
ggplot(sgi %>% filter(year == 2022), aes(x = sgi)) +
  geom_histogram(bins = 30) +
  theme_minimal() +
  labs(title = "SGI Distribution 2022")
```

---

**That's it!** You're ready to analyze socioeconomic resilience in Italian municipalities.

For more details, see the full README.md and documentation in `docs/`.
