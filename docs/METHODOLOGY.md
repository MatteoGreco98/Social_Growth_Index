# Social Growth Index (SGI) - Technical Documentation

## Methodology Details

### 1. Fixed Rank Kriging (FRK) for GDP Downscaling

#### Background
GDP data is typically available only at aggregated regional levels (NUTS-2 or NUTS-3). To measure socioeconomic resilience at the municipal level, we need to disaggregate (downscale) this GDP to individual municipalities.

#### FRK Approach
Fixed Rank Kriging is a spatial statistical method that:
- Uses known values at coarse resolution (NUTS-3 GDP)
- Incorporates covariates (population, population density)
- Applies spatial smoothing based on geographic proximity
- Preserves mass conservation (total GDP matches NUTS-3 totals)

#### Implementation Steps

1. **Population-weighted allocation**:
   ```
   pop_weight[i] = population[i] / total_population[NUTS3]
   gdp_base[i] = total_gdp[NUTS3] * pop_weight[i]
   ```

2. **Density adjustment**:
   ```
   density_factor[i] = population_density[i] / mean(population_density[NUTS3])
   density_weight[i] = density_factor[i] / sum(density_factor[NUTS3])
   ```

3. **Combined allocation**:
   ```
   gdp_municipal[i] = total_gdp[NUTS3] * (α * pop_weight[i] + β * density_weight[i])
   where α = 0.7, β = 0.3 (configurable)
   ```

4. **Spatial smoothing**:
   ```
   For each municipality i:
     For each neighbor j within distance threshold:
       weight[j] = 1 / (distance[i,j] + 0.01)
     weights = weights / sum(weights)
     gdp_smoothed[i] = sum(weights * gdp_per_capita * population)
   ```

5. **Blending and normalization**:
   ```
   gdp_blended[i] = γ * gdp_municipal[i] + δ * gdp_smoothed[i]
   where γ = 0.6, δ = 0.4 (configurable)
   
   normalization_factor = total_gdp[NUTS3] / sum(gdp_blended[NUTS3])
   gdp_final[i] = gdp_blended[i] * normalization_factor
   ```

#### Validation
The method ensures:
- Sum of municipal GDP equals NUTS-3 GDP (mass conservation)
- Spatial patterns reflect both population and density
- Smooth transitions between neighboring municipalities
- Realistic per capita values

### 2. Indicator Standardization

#### Purpose
Make indicators comparable across different scales and units.

#### Methods Available

**Z-score Standardization** (default):
```
z[i] = (x[i] - mean(x)) / sd(x)
```
- Advantages: Preserves relative distances, handles outliers
- Disadvantages: Not bounded, sensitive to extreme values

**Min-Max Normalization**:
```
normalized[i] = (x[i] - min(x)) / (max(x) - min(x))
```
- Advantages: Bounded [0, 1], easy interpretation
- Disadvantages: Sensitive to outliers

**Rank-based**:
```
rank_normalized[i] = rank(x[i]) / n
```
- Advantages: Robust to outliers, non-parametric
- Disadvantages: Loses information about magnitudes

#### Temporal Considerations
Standardization is performed **within each year** to enable:
- Cross-sectional comparison (municipalities in same year)
- Identification of relative positions
- Temporal tracking of rank changes

### 3. Copeland Ranking Method

#### Background
Copeland's method is a tournament-based voting system that performs pairwise comparisons across multiple criteria.

#### Algorithm

1. **Pairwise comparison**:
   ```
   For each pair of municipalities (i, j):
     For each indicator k:
       If indicator_k[i] > indicator_k[j]: win[k] = 1
       Else: loss[k] = 1
     
     If sum(wins) > sum(losses):
       copeland_wins[i] += 1
       copeland_losses[j] += 1
   ```

2. **Copeland score**:
   ```
   copeland_score[i] = copeland_wins[i] - copeland_losses[i]
   ```

3. **Normalization to SGI**:
   ```
   SGI[i] = (copeland_score[i] - min_score) / (max_score - min_score) * 100
   ```

#### Properties
- **Condorcet winner**: Municipality that beats all others will rank first
- **Multi-criteria**: Incorporates multiple indicators simultaneously
- **Ordinal**: Based on rankings, not absolute values
- **Robust**: Less sensitive to outliers than weighted averages

#### Advantages over Simple Averaging
- Handles conflicting indicators better
- More robust to scale differences
- Reflects competitive performance
- Natural interpretation (wins vs losses)

### 4. Indicator Selection

#### Three Key Indicators

**GDP Density** (€/km²):
- Measures: Economic productivity per unit area
- Interpretation: Concentration of economic activity
- High values: Urban centers, industrial zones
- Low values: Rural, agricultural areas

**GDP per Capita** (€/inhabitant):
- Measures: Economic welfare, standard of living
- Interpretation: Average economic output per person
- High values: Wealthy municipalities, productivity
- Low values: Economic challenges, unemployment

**Population Density** (inhabitants/km²):
- Measures: Urbanization, agglomeration
- Interpretation: Demographic concentration
- High values: Cities, metropolitan areas
- Low values: Rural, mountain areas

#### Rationale
These three indicators capture:
1. **Economic productivity** (GDP density)
2. **Economic welfare** (GDP per capita)
3. **Demographic dynamics** (population density)

Together they measure **socioeconomic resilience**: the capacity of municipalities to sustain economic and demographic vitality.

### 5. Temporal Analysis

#### Year-over-Year Changes
```
change[i,t] = value[i,t] - value[i,t-1]
pct_change[i,t] = (value[i,t] - value[i,t-1]) / value[i,t-1] * 100
```

#### Cumulative Changes from Baseline
```
total_change[i,t] = (value[i,t] - value[i,2010]) / value[i,2010] * 100
```

#### Growth Trajectories
Municipalities classified by temporal patterns:
- **Consistent top performers**: High SGI in ≥75% of years
- **Most improved**: Largest positive SGI change 2010-2022
- **Most declined**: Largest negative SGI change 2010-2022
- **Volatile**: High variance in year-to-year SGI

### 6. Spatial Analysis

#### Regional Aggregation
Results can be aggregated to:
- NUTS-3 (provinces)
- NUTS-2 (regions)
- Metropolitan areas
- Custom geographic groupings

#### Spatial Autocorrelation
High SGI municipalities tend to cluster:
- Northern industrial areas
- Major metropolitan regions
- Tourist destinations

Low SGI municipalities cluster in:
- Southern rural areas
- Mountain regions
- Peripheral areas

### 7. Uncertainty and Limitations

#### Data Limitations
- GDP data at NUTS-3 level (downscaling introduces uncertainty)
- Population data quality varies
- Missing data for some municipalities/years
- Administrative boundary changes over time

#### Methodological Limitations
- FRK assumes smooth spatial patterns
- Copeland method is ordinal (doesn't capture magnitude)
- Equal weighting of indicators (can be adjusted)
- Temporal independence assumption

#### Recommendations
- Sensitivity analysis on weights
- Validation against independent data sources
- Confidence intervals for downscaled GDP
- Robustness checks with alternative methods

## Computational Details

### Performance
- Processing time: ~5-15 minutes (depending on hardware)
- Memory requirements: ~2-4 GB RAM
- Parallelization: Not implemented (can be added)

### Software Dependencies
- R (≥4.0.0)
- FRK package for spatial statistics
- sf package for spatial data
- tidyverse for data manipulation
- tmap for mapping

### Reproducibility
All analysis is deterministic (no random seeds needed) except:
- FRK spatial smoothing (uses numerical optimization)
- Minor variations possible across R versions

## References

### Methodology
- Copeland, A. H. (1951). "A 'reasonable' social welfare function"
- Cressie, N. & Johannesson, G. (2008). "Fixed rank kriging for very large spatial data sets"

### Data Sources
- ISTAT: http://dati.istat.it/
- Eurostat: https://ec.europa.eu/eurostat/

### Related Work
- Regional GDP downscaling methods
- Composite indicators in social science
- Multi-criteria decision analysis

---

For questions or clarifications, refer to code comments or open an issue on GitHub.
