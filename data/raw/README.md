# Data Directory

This directory contains raw data files for the Social Growth Index analysis.

## Required Data Sources

### 1. ISTAT Data (Italian National Institute of Statistics)
- **Municipal population data**: Annual population by municipality (2010-2022)
  - Source: http://dati.istat.it/
  - File: `popolazione_comuni_YYYY.csv` (one per year)
  - Variables: PRO_COM_T (municipal code), population

- **Municipal demographic indicators**
  - Source: http://dati.istat.it/
  - File: `indicatori_demografici_comuni_YYYY.csv`
  - Variables: demographic indicators at municipal level

### 2. Eurostat Data
- **Regional GDP (NUTS-2/NUTS-3)**
  - Source: https://ec.europa.eu/eurostat/data/database
  - Dataset: nama_10r_2gdp, nama_10r_3gdp
  - File: `gdp_regional_NUTS2_NUTS3.csv`
  - Variables: TIME_PERIOD, geo (NUTS code), GDP values

### 3. Geographic Data
- **Italian municipal boundaries**
  - Source: ISTAT (Limiti amministrativi)
  - File: `Com01012022_g_WGS84.shp` (shapefile with associated files)
  - Format: Shapefile or GeoJSON

## Data Download Instructions

1. **ISTAT Population Data**:
   ```
   Visit: http://dati.istat.it/
   Navigate to: Popolazione > Popolazione residente al 1° gennaio
   Select: Years 2010-2022, all municipalities
   Export as CSV
   ```

2. **Eurostat GDP Data**:
   ```
   Visit: https://ec.europa.eu/eurostat/data/database
   Navigate to: Economy and finance > National accounts > GDP
   Select: Regional GDP (nama_10r_2gdp, nama_10r_3gdp)
   Filter: Italy, Years 2010-2022
   Export as CSV
   ```

3. **Geographic Boundaries**:
   ```
   Visit: https://www.istat.it/it/archivio/222527
   Download: Limiti amministrativi 2022
   Extract: Municipal boundary shapefiles
   ```

## File Structure

After downloading, organize files as:
```
data/raw/
  ├── popolazione_comuni_2010.csv
  ├── popolazione_comuni_2011.csv
  ├── ...
  ├── popolazione_comuni_2022.csv
  ├── gdp_regional_NUTS2_NUTS3.csv
  └── boundaries/
      ├── Com01012022_g_WGS84.shp
      ├── Com01012022_g_WGS84.shx
      ├── Com01012022_g_WGS84.dbf
      └── Com01012022_g_WGS84.prj
```

## Data Preprocessing

The `scripts/01_load_and_clean_data.R` script will:
- Load all raw data files
- Standardize municipality codes
- Merge datasets by municipality and year
- Handle missing values
- Save cleaned data to `data/processed/`

## Notes

- All data files are listed in `.gitignore` due to size and licensing
- Users must download data independently following instructions above
- Data coverage: 2010-2022, all Italian municipalities (~7,900 municipalities)
- Coordinate Reference System: WGS84 (EPSG:4326)
