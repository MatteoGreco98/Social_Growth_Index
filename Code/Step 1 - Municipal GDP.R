# Estimation of Municipal GDP using FRK

# ============================================================
# FRK MULTILEVEL GAMMA-LOG TEMPLATE
# ============================================================
# This script implements a multi-level FRK model (Gamma with log-link)
# using:
#  - BAUs on a regular grid over a selected domain (Italian regions in the original use case)
#  - areal observations at multiple levels (e.g. provinces + regions)
#  - auxiliary covariates at municipal and regional level
#
# The script is written to be reusable in a GitHub repository:
#  - All user-specific paths are removed.
#  - All dataset-specific pieces are grouped in the "USER CONFIG" section.
#  - Logging is done in a generic way.
#
# Adapt the USER CONFIG section to your own data structure.
# ============================================================

# -------------------- Libraries --------------------
library(FRK)
library(sp)
library(sf)
library(dplyr)
library(readxl)
library(rlang)
library(corrplot)

# ============================================================
# USER CONFIG: paths, filenames, basic parameters
# ============================================================

# Root directory where data are stored (change as needed)
data_dir    <- "data"          # TODO: set your data folder
results_dir <- "results"       # TODO: set your results folder root

# Example subfolders (you can adapt to your own layout)
base_dir    <- file.path(data_dir, "FRK_data")      # TODO
RESULTS_DIR <- file.path(results_dir, "FRK_Results")# TODO

if (!dir.exists(RESULTS_DIR)) {
  dir.create(RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)
}

# Main model parameters
YEAR     <- 2022              # TODO: year to analyse
REGIONI  <- 1:20              # TODO: selected region codes (e.g. 1:20 for all Italian regions)
EPSG_UTM <- 32632             # UTM32N (metric CRS)
cell_km  <- 40                # BAU grid resolution (km)
nres_use <- 2                 # number of resolutions for basis functions

# Filenames (relative to base_dir). Adapt to your own data.
file_comuni_rds     <- "Dataset_GRINS_Comunale_1.rds"        # TODO
file_provinciale_rds<- "Dataset_GRINS_Provinciale_1.rds"     # TODO
file_regionale_rds  <- "Dataset_GRINS_Regionale_1.rds"       # TODO
file_gdp_prov_gpkg  <- "GDP_costant_2015_all_years.gpkg"     # TODO
file_gdp_reg_gpkg   <- "GDP_NUTS2_IT_real2015_with_Trentino_fixed.gpkg" # TODO

file_mef_gpkg       <- "MEF2022_sh.gpkg"                     # TODO
file_sh_prov        <- "ProvCM01012023"                      # TODO
file_sh_com         <- "Com01012023"                         # TODO
file_sh_reg         <- "Reg01012023"                         # TODO

# Note: sh_com21 is the municipal shapefile aligned with MEF register (e.g. 2021)
file_sh_com21       <- "Com01012021_MEFAlias.gpkg"           # TODO: put your MEF-aligned municipal shapefile

message("Results will be saved in: ", RESULTS_DIR)
message("Base directory is: ", base_dir)
message("Cell size (km): ", cell_km)
message("Number of resolutions (nres_use): ", nres_use)
message("Year: ", YEAR)
message("Regions (codes): ", paste(REGIONI, collapse = ", "))
message("UTM EPSG: ", EPSG_UTM)

# ============================================================
# UTILITIES: logging + saving helpers
# ============================================================

.ts      <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")
.logline <- function(msg) sprintf("[%s] %s", .ts(), msg)

log_step <- function(msg) {
  line <- .logline(msg)
  message(line)
  write(line, file = file.path(RESULTS_DIR, "run.log"), append = TRUE)
}

save_rds  <- function(obj, name) {
  saveRDS(obj, file.path(RESULTS_DIR, paste0(name, ".rds")))
}

save_csv  <- function(df,  name) {
  utils::write.csv(df, file.path(RESULTS_DIR, paste0(name, ".csv")), row.names = FALSE)
}

save_gpkg <- function(x,   name, layer = name) {
  sf::st_write(
    x,
    dsn         = file.path(RESULTS_DIR, paste0(name, ".gpkg")),
    layer       = layer,
    delete_dsn  = FALSE,
    delete_layer= TRUE,
    quiet       = TRUE
  )
}

# ============================================================
# GEOMETRY & GRID UTILITIES (sf-based)
# ============================================================

.align_crs <- function(x, y) {
  stopifnot(inherits(x, "sf"), inherits(y, "sf"))
  if (is.na(st_crs(x)) || is.na(st_crs(y))) return(y)
  if (st_crs(x) != st_crs(y)) y <- st_transform(y, st_crs(x))
  y
}

# 1) BBox from CRS and extents
GetBBox <- function(crs, xmin = 0, xmax = 1, ymin = 0, ymax = 1) {
  the_crs <- tryCatch(st_crs(crs), error = function(e) NA)
  if (is.na(the_crs)) {
    stop("GetBBox: invalid CRS. Pass an EPSG code or an sf-compatible crs object.")
  }
  bb <- st_bbox(c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax), crs = the_crs)
  st_as_sfc(bb)
}

# 2) Regular grid from bbox (xnum/ynum)
GetSpatialGrid_FromBBox <- function(this_bbox, xnum = 100, ynum = 100) {
  stopifnot(inherits(this_bbox, "sfc") || inherits(this_bbox, "sf"))
  if (inherits(this_bbox, "sf")) this_bbox <- st_geometry(this_bbox)
  gr <- st_make_grid(this_bbox, n = c(xnum, ynum), what = "polygons", square = TRUE)
  grd <- st_sf(grid_id = seq_along(gr), geometry = gr)
  st_crs(grd) <- st_crs(this_bbox)
  grd
}

# 2b) Regular grid from cell size (in meters)
GetSpatialGrid_FromCell <- function(geometry, cell_m) {
  g <- if (inherits(geometry, "sf")) st_geometry(geometry) else geometry
  stopifnot(inherits(g, "sfc"))
  bb <- st_bbox(g)
  xnum <- max(1L, ceiling((bb["xmax"] - bb["xmin"]) / cell_m))
  ynum <- max(1L, ceiling((bb["ymax"] - bb["ymin"]) / cell_m))
  GetSpatialGrid_FromBBox(st_as_sfc(bb), xnum = xnum, ynum = ynum)
}

# 3) BBox from geometry
GetBBox_FromGeometry <- function(geometry) {
  stopifnot(inherits(geometry, "sfc") || inherits(geometry, "sf"))
  if (inherits(geometry, "sf")) geometry <- st_geometry(geometry)
  st_as_sfc(st_bbox(geometry))
}

# 4) Generic GetGrid wrapper
GetGrid <- function(bbox = NULL,
                    geometry = NULL,
                    xnum = NULL, ynum = NULL,
                    cell_m = NULL,
                    mask_geometry = NULL,
                    mask_mode = c("centroid", "polygon"),
                    keep_empty = FALSE,
                    make_valid = TRUE) {
  mask_mode <- match.arg(mask_mode)
  if (is.null(bbox) && is.null(geometry)) {
    stop("GetGrid: specify 'bbox' or 'geometry'.")
  }
  if (!is.null(geometry)) {
    bbox <- GetBBox_FromGeometry(geometry)
  }
  
  grid <- if (!is.null(cell_m)) {
    GetSpatialGrid_FromCell(bbox, cell_m = cell_m)
  } else {
    if (is.null(xnum) || is.null(ynum))
      stop("GetGrid: provide (xnum, ynum) or 'cell_m'.")
    GetSpatialGrid_FromBBox(bbox, xnum = xnum, ynum = ynum)
  }
  
  if (!is.null(mask_geometry)) {
    if (inherits(mask_geometry, "sfc")) mask_geometry <- st_sf(geom = mask_geometry)
    if (make_valid) mask_geometry <- st_make_valid(mask_geometry)
    mask_geometry <- .align_crs(grid, mask_geometry)
    
    if (mask_mode == "polygon") {
      out <- suppressWarnings(st_intersection(grid, st_geometry(mask_geometry)))
      if (!keep_empty) {
        out <- out %>% dplyr::filter(!st_is_empty(geometry))
      }
      grid <- out
    } else {
      pts <- st_point_on_surface(st_geometry(grid))
      hit <- st_intersects(st_sf(geom = pts), mask_geometry, sparse = TRUE)
      keep <- lengths(hit) > 0
      if (!keep_empty) grid <- grid[keep, ]
    }
  }
  grid
}

# ============================================================
# COVARIATE PROJECTION UTILITIES
# ============================================================

# Project municipal covariates onto BAUs
push_communal_covariates <- function(baus, sh_com, cov_tab, cov_cols,
                                     sh_code = "PRO_COM", data_code = "codice_comune",
                                     prefix = NULL, impute_zero = TRUE) {
  sh_com <- st_transform(sh_com, st_crs(baus)) |> st_make_valid()
  pts    <- st_point_on_surface(st_geometry(baus))
  
  idx <- st_join(st_sf(geom = pts), sh_com |> dplyr::select(all_of(sh_code)), left = TRUE)
  baus[[sh_code]] <- idx[[sh_code]]
  
  na_id <- which(is.na(baus[[sh_code]]))
  if (length(na_id)) {
    nn <- st_nearest_feature(st_sf(geom = pts[na_id]), sh_com)
    baus[[sh_code]][na_id] <- sh_com[[sh_code]][nn]
  }
  
  cov_tab2 <- cov_tab |>
    dplyr::select(all_of(c(data_code, cov_cols))) |>
    dplyr::distinct(.data[[data_code]], .keep_all = TRUE)
  
  baus <- baus |>
    dplyr::left_join(cov_tab2, by = setNames(data_code, sh_code))
  
  if (!is.null(prefix)) {
    for (nm in cov_cols) names(baus)[names(baus) == nm] <- paste0(prefix, nm)
    cov_cols <- paste0(prefix, cov_cols)
  }
  if (impute_zero) for (nm in cov_cols) baus[[nm]][is.na(baus[[nm]])] <- 0
  
  list(baus = baus, added_cols = cov_cols)
}

# Project regional covariates onto BAUs
push_regional_covariates <- function(baus, sh_reg, reg_tab, reg_cols,
                                     sh_code = "COD_REG", tab_code = "COD_REG",
                                     prefix = NULL, impute_zero = TRUE) {
  sh_reg <- st_transform(sh_reg, st_crs(baus)) |> st_make_valid()
  pts    <- st_point_on_surface(st_geometry(baus))
  
  idx <- st_join(st_sf(geom = pts), sh_reg |> dplyr::select(all_of(sh_code)), left = TRUE)
  baus[[sh_code]] <- idx[[sh_code]]
  
  na_id <- which(is.na(baus[[sh_code]]))
  if (length(na_id)) {
    nn <- st_nearest_feature(st_sf(geom = pts[na_id]), sh_reg)
    baus[[sh_code]][na_id] <- sh_reg[[sh_code]][nn]
  }
  
  reg_tab2 <- reg_tab |> dplyr::select(all_of(c(tab_code, reg_cols)))
  baus <- baus |> dplyr::left_join(reg_tab2, by = setNames(tab_code, sh_code))
  
  if (!is.null(prefix)) {
    for (nm in reg_cols) names(baus)[names(baus) == nm] <- paste0(prefix, nm)
    reg_cols <- paste0(prefix, reg_cols)
  }
  if (impute_zero) for (nm in reg_cols) baus[[nm]][is.na(baus[[nm]])] <- 0
  
  list(baus = baus, added_cols = reg_cols)
}

# ============================================================
# STEP 1 — DATA LOADING
# ============================================================

# RDS datasets (municipal, provincial, regional)
Comuni      <- readRDS(file.path(base_dir, file_comuni_rds))
Provinciale <- readRDS(file.path(base_dir, file_provinciale_rds))
Regionale   <- readRDS(file.path(base_dir, file_regionale_rds))

# GDP at provincial level (multi-year)
GDP_costant_2015 <- st_read(file.path(base_dir, file_gdp_prov_gpkg), quiet = TRUE)

# Standardise column names (dataset-specific)
colnames(GDP_costant_2015)[colnames(GDP_costant_2015) == "time"]        <- "anno"
colnames(GDP_costant_2015)[colnames(GDP_costant_2015) == "gdp_real_15"] <- "gdp_const_p_2015"
colnames(GDP_costant_2015)[colnames(GDP_costant_2015) == "geom"]        <- "geometry"
st_geometry(GDP_costant_2015) <- "geometry"

# GDP at regional level (for areal observations)
Regioni_GDP <- st_read(file.path(base_dir, file_gdp_reg_gpkg), quiet = TRUE)

# MEF dataset (municipal tax data, original use case)
mef20YY_sh <- st_read(file.path(base_dir, file_mef_gpkg), quiet = TRUE)

# TODO: this block is dataset-specific.
#       Keep it here if you share the same MEF structure,
#       otherwise adapt/remove and build your own covariates.
mef20YY_sh <- mef20YY_sh |>
  dplyr::rename(
    anno_imposta          = "Anno.di.imposta",
    codice_catastale      = "Codice.catastale",
    codice_istat_comune   = "Codice.Istat.Comune",
    denominazione_comune  = "Denominazione.Comune",
    codice_istat_regione  = "Codice.Istat.Regione",
    n_contribuenti        = "Numero.contribuenti",
    redd_fabbricati_freq  = "Reddito.da.fabbricati...Frequenza",
    redd_fabbricati_euro  = "Reddito.da.fabbricati...Ammontare.in.euro",
    redd_lavdip_freq      = "Reddito.da.lavoro.dipendente.e.assimilati...Frequenza",
    redd_lavdip_euro      = "Reddito.da.lavoro.dipendente.e.assimilati...Ammontare.in.euro",
    redd_pensione_freq    = "Reddito.da.pensione...Frequenza",
    redd_pensione_euro    = "Reddito.da.pensione...Ammontare.in.euro",
    redd_autonomo_freq    = "Reddito.da.lavoro.autonomo..comprensivo.dei.valori.nulli....Frequenza",
    redd_autonomo_euro    = "Reddito.da.lavoro.autonomo..comprensivo.dei.valori.nulli....Ammontare.in.euro",
    redd_imprend_ord_freq = "Reddito.di.spettanza.dell.imprenditore.in.contabilita..ordinaria...comprensivo.dei.valori.nulli....Frequenza",
    redd_imprend_ord_euro = "Reddito.di.spettanza.dell.imprenditore.in.contabilita..ordinaria...comprensivo.dei.valori.nulli....Ammontare.in.euro",
    redd_imprend_semp_freq= "Reddito.di.spettanza.dell.imprenditore.in.contabilita..semplificata..comprensivo.dei.valori.nulli....Frequenza",
    redd_imprend_semp_euro= "Reddito.di.spettanza.dell.imprenditore.in.contabilita..semplificata..comprensivo.dei.valori.nulli....Ammontare.in.euro",
    redd_partecip_freq    = "Reddito.da.partecipazione...comprensivo.dei.valori.nulli....Frequenza",
    redd_partecip_euro    = "Reddito.da.partecipazione...comprensivo.dei.valori.nulli....Ammontare.in.euro",
    redd_imponibile_freq  = "Reddito.imponibile...Frequenza",
    redd_imponibile_euro  = "Reddito.imponibile...Ammontare.in.euro",
    imposta_netta_freq    = "Imposta.netta...Frequenza",
    imposta_netta_euro    = "Imposta.netta...Ammontare.in.euro",
    tratt_freq            = "Trattamento.spettante...Frequenza",
    tratt_euro            = "Trattamento.spettante...Ammontare.in.euro",
    redd_impon_add_freq   = "Reddito.imponibile.addizionale...Frequenza",
    redd_impon_add_euro   = "Reddito.imponibile.addizionale...Ammontare.in.euro",
    add_regionale_freq    = "Addizionale.regionale.dovuta...Frequenza",
    add_regionale_euro    = "Addizionale.regionale.dovuta...Ammontare.in.euro",
    add_comunale_freq     = "Addizionale.comunale.dovuta...Frequenza",
    add_comunale_euro     = "Addizionale.comunale.dovuta...Ammontare.in.euro",
    redd_zero_freq        = "Reddito.complessivo.minore.o.uguale.a.zero.euro...Frequenza",
    redd_zero_euro        = "Reddito.complessivo.minore.o.uguale.a.zero.euro...Ammontare.in.euro",
    redd_0_10k_freq       = "Reddito.complessivo.da.0.a.10000.euro...Frequenza",
    redd_0_10k_euro       = "Reddito.complessivo.da.0.a.10000.euro...Ammontare.in.euro",
    redd_10_15k_freq      = "Reddito.complessivo.da.10000.a.15000.euro...Frequenza",
    redd_10_15k_euro      = "Reddito.complessivo.da.10000.a.15000.euro...Ammontare.in.euro",
    redd_15_26k_freq      = "Reddito.complessivo.da.15000.a.26000.euro...Frequenza",
    redd_15_26k_euro      = "Reddito.complessivo.da.15000.a.26000.euro...Ammontare.in.euro",
    redd_26_55k_freq      = "Reddito.complessivo.da.26000.a.55000.euro...Frequenza",
    redd_26_55k_euro      = "Reddito.complessivo.da.26000.a.55000.euro...Ammontare.in.euro",
    redd_55_75k_freq      = "Reddito.complessivo.da.55000.a.75000.euro...Frequenza",
    redd_55_75k_euro      = "Reddito.complessivo.da.55000.a.75000.euro...Ammontare.in.euro",
    redd_75_120k_freq     = "Reddito.complessivo.da.75000.a.120000.euro...Frequenza",
    redd_75_120k_euro     = "Reddito.complessivo.da.75000.a.120000.euro...Ammontare.in.euro",
    redd_oltre_120k_freq  = "Reddito.complessivo.oltre.120000.euro...Frequenza",
    redd_oltre_120k_euro  = "Reddito.complessivo.oltre.120000.euro...Ammontare.in.euro",
    Reddito_totale        = "Redditto_totale",
    Reddito_pc            = "Reddito_pc",
    geometry              = "geom"
  )

mef_cols_renamed <- c(
  "anno_imposta","codice_catastale","codice_istat_comune","denominazione_comune",
  "codice_istat_regione","n_contribuenti","redd_fabbricati_freq","redd_fabbricati_euro",
  "redd_lavdip_freq","redd_lavdip_euro","redd_pensione_freq","redd_pensione_euro",
  "redd_autonomo_freq","redd_autonomo_euro","redd_imprend_ord_freq","redd_imprend_ord_euro",
  "redd_imprend_semp_freq","redd_imprend_semp_euro","redd_partecip_freq","redd_partecip_euro",
  "redd_imponibile_freq","redd_imponibile_euro","imposta_netta_freq","imposta_netta_euro",
  "tratt_freq","tratt_euro","redd_impon_add_freq","redd_impon_add_euro",
  "add_regionale_freq","add_regionale_euro","add_comunale_freq","add_comunale_euro",
  "redd_zero_freq","redd_zero_euro","redd_0_10k_freq","redd_0_10k_euro",
  "redd_10_15k_freq","redd_10_15k_euro","redd_15_26k_freq","redd_15_26k_euro",
  "redd_26_55k_freq","redd_26_55k_euro","redd_55_75k_freq","redd_55_75k_euro",
  "redd_75_120k_freq","redd_75_120k_euro","redd_oltre_120k_freq","redd_oltre_120k_euro",
  "Reddito_totale","Reddito_pc"
)

mef20YY <- mef20YY_sh %>%
  dplyr::select(all_of(mef_cols_renamed)) %>%
  as.data.frame()

# Shapefiles
sh_prov  <- st_read(file.path(base_dir, file_sh_prov), quiet = TRUE)
sh_com   <- st_read(file.path(base_dir, file_sh_com),  quiet = TRUE)
sh_reg   <- st_read(file.path(base_dir, file_sh_reg),  quiet = TRUE)
sh_com21 <- st_read(file.path(base_dir, file_sh_com21),quiet = TRUE)

log_step("Data loaded: municipal/provincial/regional datasets, GDP, MEF, and shapefiles.")

# ============================================================
# STEP 2 — FILTER BY REGION/YEAR AND BUILD ATTRIBUTE TABLES
# ============================================================

# Municipal level
Comuni_REG_0 <- Comuni |>
  dplyr::filter(anno == YEAR, COD_REG %in% REGIONI) |>
  dplyr::select(
    codice_comune, stringa_codici, anno,
    nome_provincia, nome_regione, nome_comune,
    Popolazione, COD_REG
  )

sh_com_REG_0 <- sh_com |> dplyr::filter(COD_REG %in% REGIONI)
Comuni_REG_sh <- merge(sh_com_REG_0, Comuni_REG_0, by.x = "PRO_COM", by.y = "codice_comune")
Comuni_REG_sh <- Comuni_REG_sh %>%
  dplyr::select(-COD_REG.y)
colnames(Comuni_REG_sh)[colnames(Comuni_REG_sh) == "COD_REG.x"] <- "COD_REG"

# Provincial GDP (€/year, constant prices)
GDP_costant_2015_clear <- GDP_costant_2015 %>%
  dplyr::filter(anno == YEAR, COD_REG %in% REGIONI) %>%
  dplyr::select(DEN_UTS, anno, COD_PROV, COD_REG, SIGLA, gdp_const_p_2015, geometry)

Provinciale_REG_sh <- GDP_costant_2015_clear

# Regional additional covariates
Regionie_REG <- Regionale |>
  dplyr::filter(anno == YEAR, COD_REG %in% REGIONI) |>
  dplyr::select(stringa, nome_regione, anno, COD_REG,
                R.D_spesa_totale_economia, R.D_addetti_totale_economia)
sh_reg_REG <- sh_reg |> dplyr::filter(COD_REG %in% REGIONI)
Regionie_REG_sh <- merge(sh_reg_REG, Regionie_REG, by = "COD_REG")

# MEF, filtered by selected regions
mef_data <- mef20YY
mef_data[is.na(mef_data)] <- 0
mef_data$codice_comune <- mef_data$codice_istat_comune

mef_data_REG <- mef_data %>%
  dplyr::filter(codice_istat_regione %in% REGIONI) %>%
  dplyr::select(
    anno_imposta, codice_istat_comune, denominazione_comune,
    codice_istat_regione, n_contribuenti, Reddito_totale, Reddito_pc
  )

sh_com21_REG <- sh_com21 |> dplyr::filter(COD_REG %in% REGIONI)
mef_data_REG_sh <- merge(sh_com21_REG, mef_data_REG,
                         by.x = "PRO_COM", by.y = "codice_istat_comune")

log_step("Regional filters applied and municipal/MEF shapes aligned.")

# Attribute tables (clean) for covariates
sh_reg_master <- sh_reg   |> dplyr::filter(COD_REG %in% REGIONI)
sh_com_MEFCOV <- sh_com21 |> dplyr::filter(COD_REG %in% REGIONI)
sh_com_COMCOV <- sh_com21 |> dplyr::filter(COD_REG %in% REGIONI)
sh_prov_REG   <- sh_prov  |> dplyr::filter(COD_REG %in% REGIONI)

mef_data_REG_tab <- mef_data |>
  dplyr::filter(anno_imposta == YEAR,
                codice_istat_regione %in% REGIONI) |>
  dplyr::transmute(
    codice_comune       = as.integer(codice_istat_comune),
    mef_Reddito_pc      = as.numeric(Reddito_pc),
    mef_Redditto_totale = as.numeric(Reddito_totale)
  )

Comuni_REG_tab <- Comuni |>
  dplyr::filter(anno == YEAR, COD_REG %in% REGIONI) |>
  dplyr::transmute(
    codice_comune,
    com_Popolazione = as.numeric(Popolazione)
  )

Regionale_REG_tab <- Regionale |>
  dplyr::filter(anno == YEAR, COD_REG %in% REGIONI) |>
  dplyr::transmute(
    COD_REG,
    reg_Spesa   = as.numeric(R.D_spesa_totale_economia),
    reg_Addetti = as.numeric(R.D_addetti_totale_economia)
  )

log_step("Attribute tables for each dataset (MEF/municipal/regional) prepared.")

# ============================================================
# STEP 3 — BAUs (DOMAIN GRID) AND WEIGHTS
# ============================================================

# Unified domain (union of selected regions) in metric CRS
domain_utm <- st_union(st_make_valid(sh_reg_master)) |> st_transform(EPSG_UTM)

bb      <- st_bbox(domain_utm)
dx_m    <- bb["xmax"] - bb["xmin"]
dy_m    <- bb["ymax"] - bb["ymin"]
cell_m  <- cell_km * 1000
xnum    <- max(1L, ceiling(dx_m / cell_m))
ynum    <- max(1L, ceiling(dy_m / cell_m))

BAUs <- st_make_grid(domain_utm, n = c(xnum, ynum),
                     what = "polygons", square = TRUE) |>
  st_sf(geometry = _) |>
  st_intersection(domain_utm)

BAUs$fs <- 1

inter_bau_dom <- suppressWarnings(
  st_intersection(
    BAUs |> dplyr::mutate(idx = dplyr::row_number()) |> dplyr::select(idx),
    domain_utm
  )
)

wts_tbl <- inter_bau_dom |>
  dplyr::mutate(area_in = as.numeric(st_area(geometry))) |>
  st_drop_geometry() |>
  dplyr::group_by(idx) |>
  dplyr::summarise(wts = sum(area_in, na.rm = TRUE), .groups = "drop")

wts_vec <- numeric(nrow(BAUs))
wts_vec[wts_tbl$idx] <- wts_tbl$wts
BAUs$wts <- wts_vec

log_step(sprintf("BAUs created: N=%d; total area (km^2)=%.2f",
                 nrow(BAUs),
                 sum(as.numeric(st_area(BAUs)))/1e6))

# ============================================================
# STEP 4 — PROJECT COVARIATES ONTO BAUs
# ============================================================

baus_cov <- BAUs

# MEF covariates
mef_cols <- c("mef_Reddito_pc", "mef_Redditto_totale")
tmp <- push_communal_covariates(
  baus_cov, sh_com_MEFCOV, mef_data_REG_tab, mef_cols,
  sh_code    = "PRO_COM",
  data_code  = "codice_comune",
  prefix     = NULL,
  impute_zero = TRUE
)
baus_cov <- tmp$baus
cols_mef <- tmp$added_cols

# Municipal population
com_cols <- c("com_Popolazione")
tmp <- push_communal_covariates(
  baus_cov, sh_com_COMCOV, Comuni_REG_tab, com_cols,
  sh_code    = "PRO_COM",
  data_code  = "codice_comune",
  prefix     = NULL,
  impute_zero = TRUE
)
baus_cov <- tmp$baus
cols_com <- tmp$added_cols

# Regional covariates
reg_cols <- c("reg_Spesa", "reg_Addetti")
tmp <- push_regional_covariates(
  baus_cov, sh_reg_REG, Regionale_REG_tab, reg_cols,
  sh_code    = "COD_REG",
  tab_code   = "COD_REG",
  prefix     = NULL,
  impute_zero = TRUE
)
baus_cov <- tmp$baus
cols_reg <- tmp$added_cols

log_step("Covariates projected onto BAUs (MEF + municipal + regional).")

# Rename covariates as Covariate_1..k and build legend
all_cov_cols <- c(cols_com, cols_mef, cols_reg)

cov_legend <- data.frame(
  Covariate = paste0("Covariate_", seq_along(all_cov_cols)),
  Original  = all_cov_cols,
  stringsAsFactors = FALSE
)

for (i in seq_along(all_cov_cols)) {
  baus_cov[[paste0("Covariate_", i)]] <- baus_cov[[all_cov_cols[i]]]
}

grid_BAUs_allcov <- baus_cov

attr_df  <- sf::st_drop_geometry(grid_BAUs_allcov)
cov_cols <- grep("^Covariate_", names(attr_df), value = TRUE)
if (length(cov_cols) == 0) stop("No 'Covariate_*' columns created.")
if (anyNA(attr_df[, cov_cols, drop = FALSE])) {
  message("Note: NA values present in covariates — consider additional imputation/filtering.")
}

log_step(sprintf("Covariate_1..k created. k=%d", length(cov_cols)))

# ============================================================
# STEP 5 — COVARIATE CORRELATION & SELECTION
# ============================================================

cov_data <- attr_df[, cov_cols, drop = FALSE]
cor_mat  <- cor(cov_data, use = "pairwise.complete.obs", method = "pearson")

# Replace Covariate_* labels with original names
colnames(cor_mat) <- cov_legend$Original
rownames(cor_mat) <- cov_legend$Original

print(round(cor_mat, 3))
log_step("Covariate correlation matrix computed.")

high_corr <- which(abs(cor_mat) > 0.9 & abs(cor_mat) < 1, arr.ind = TRUE)
if (nrow(high_corr) > 0) {
  cat("\nHighly correlated pairs (|r| > 0.9):\n")
  print(apply(high_corr, 1, function(idx) paste(colnames(cor_mat)[idx], collapse = " vs ")))
} else {
  cat("\nNo correlations > 0.9 found.\n")
}

# Simple greedy drop function
drop_high_corr <- function(cor_mat, thr = 0.90) {
  C <- abs(cor_mat)
  diag(C) <- 0
  keep <- colnames(C)
  while (length(keep) > 1 && max(C[keep, keep, drop = FALSE]) > thr) {
    cm <- C[keep, keep, drop = FALSE]
    diag(cm) <- 0
    ij <- which(cm == max(cm), arr.ind = TRUE)[1, ]
    i  <- rownames(cm)[ij[1]]
    j  <- colnames(cm)[ij[2]]
    mi <- mean(cm[i, keep, drop = TRUE])
    mj <- mean(cm[j, keep, drop = TRUE])
    drop_var <- if (mi >= mj) i else j
    keep <- setdiff(keep, drop_var)
  }
  keep
}

# Manual selection (optional): use original names
keep_manual <- c("com_Popolazione", "mef_Reddito_pc")  # TODO: edit or set to NULL

corr_threshold <- 0.90
keep_auto <- drop_high_corr(cor_mat, thr = corr_threshold)

orig_names <- cov_legend$Original

if (!is.null(keep_manual)) {
  selected_original <- intersect(keep_manual, orig_names)
} else {
  selected_original <- intersect(keep_auto, orig_names)
}

if (length(selected_original) == 0) {
  stop("Covariate selection yielded 0 variables. Check 'keep_manual' or 'corr_threshold'.")
}

cat("Selected covariates (original names):\n")
print(selected_original)

# Rebuild grid with selected covariates only
grid_BAUs_allcov_filtered <- grid_BAUs_allcov
old_cov_mask <- grepl("^Covariate_", names(grid_BAUs_allcov_filtered))
grid_BAUs_allcov_filtered <- grid_BAUs_allcov_filtered[, !old_cov_mask]

for (i in seq_along(selected_original)) {
  src_name <- selected_original[i]
  if (!src_name %in% names(grid_BAUs_allcov_filtered)) {
    stop("Missing column in dataset: ", src_name)
  }
  grid_BAUs_allcov_filtered[[paste0("Covariate_", i)]] <-
    grid_BAUs_allcov_filtered[[src_name]]
}

cov_legend_filtered <- data.frame(
  Covariate = paste0("Covariate_", seq_along(selected_original)),
  Original  = selected_original,
  stringsAsFactors = FALSE
)

attr_df_f   <- sf::st_drop_geometry(grid_BAUs_allcov_filtered)
cov_cols_f  <- grep("^Covariate_", names(attr_df_f), value = TRUE)
na_counts_f <- colSums(is.na(attr_df_f[, cov_cols_f, drop = FALSE]))
cat("\nNew Covariate_* created:\n")
print(cov_legend_filtered)
cat("\nNA count per selected covariate:\n")
print(na_counts_f)

cor_mat_sel <- cor(attr_df_f[, cov_cols_f, drop = FALSE],
                   use = "pairwise.complete.obs")
colnames(cor_mat_sel) <- cov_legend_filtered$Original
rownames(cor_mat_sel) <- cov_legend_filtered$Original
cat("\nCorrelations among selected covariates:\n")
print(round(cor_mat_sel, 3))

log_step(sprintf(
  "Covariate selection completed: k=%d -> %s",
  nrow(cov_legend_filtered),
  paste(cov_legend_filtered$Original, collapse = ", ")
))

grid_BAUs_allcov <- grid_BAUs_allcov_filtered
cov_legend       <- cov_legend_filtered

# ============================================================
# STEP 6 — BAUs -> SpatialPixelsDataFrame + standardisation
# ============================================================

baus_sf   <- grid_BAUs_allcov |> sf::st_transform(EPSG_UTM)
baus_ctr  <- sf::st_centroid(sf::st_geometry(baus_sf))
baus_sp   <- methods::as(sf::st_sf(geom = baus_ctr), "Spatial")

cov_mask  <- grepl("^Covariate_", names(baus_sf))
baus_data <- sf::st_drop_geometry(baus_sf)[, c("fs", names(baus_sf)[cov_mask]), drop = FALSE]

baus_spdf <- sp::SpatialPixelsDataFrame(
  sp::coordinates(baus_sp),
  data      = as.data.frame(baus_data),
  tolerance = 0.999
)
sp::proj4string(baus_spdf) <- sp::CRS(sf::st_crs(baus_sf)$wkt)

baus_spdf@data$wts <- sf::st_drop_geometry(baus_sf)$wts
baus_spdf@data$fs  <- sf::st_drop_geometry(baus_sf)$fs

# Clean weights and fs, z-score covariates
wts <- baus_spdf@data$wts
wts[!is.finite(wts) | wts < 0] <- 0
stopifnot(any(wts > 0))
baus_spdf@data$wts <- wts / max(wts)

if (!"fs" %in% names(baus_spdf@data)) baus_spdf@data$fs <- 1
fs <- baus_spdf@data$fs
fs[!is.finite(fs) | fs <= 0] <- median(fs[is.finite(fs) & fs > 0], na.rm = TRUE)
baus_spdf@data$fs <- fs

covars <- intersect(grep("^Covariate_", names(baus_spdf@data), value = TRUE),
                    names(baus_spdf@data))
for (cn in covars) {
  v <- baus_spdf@data[[cn]]
  v[!is.finite(v)] <- median(v[is.finite(v)], na.rm = TRUE)
  baus_spdf@data[[cn]] <- as.numeric(scale(v))
}

summary(baus_spdf)

# ============================================================
# STEP 7 — AREAL OBSERVATIONS (REGIONS + PROVINCES)
# ============================================================

# Normalise GDP regional table
colnames(Regioni_GDP)[colnames(Regioni_GDP) == "time"]           <- "anno"
colnames(Regioni_GDP)[colnames(Regioni_GDP) == "gdp_clv15_meur"] <- "gdp_const_p_2015"
colnames(Regioni_GDP)[colnames(Regioni_GDP) == "geom"]           <- "geometry"
if (inherits(Regioni_GDP, "sf") && attr(Regioni_GDP, "sf_column") != "geometry") {
  sf::st_geometry(Regioni_GDP) <- "geometry"
}

Provinciale_REG_sh_1 <- Provinciale_REG_sh %>%
  dplyr::select(COD_PROV, anno, gdp_const_p_2015)

Regioni_GDP_1 <- Regioni_GDP %>%
  dplyr::filter(anno == YEAR) %>%
  dplyr::select(COD_PROV, anno, gdp_const_p_2015, geometry)

prep_obs <- function(sf_obj) {
  sf_obj %>%
    st_make_valid() %>%
    st_transform(EPSG_UTM) %>%
    dplyr::mutate(
      area_km2 = as.numeric(st_area(.)) / 1e6,
      value    = gdp_const_p_2015 / pmax(area_km2, 1e-6)
    ) %>%
    dplyr::filter(
      !st_is_empty(geometry),
      is.finite(value), !is.na(value), value > 0
    ) %>%
    dplyr::select(value)
}

obs_reg_sf  <- prep_obs(Regioni_GDP_1)
obs_prov_sf <- prep_obs(Provinciale_REG_sh_1)

baus_sf_tmp <- sf::st_as_sf(baus_spdf)
ctr         <- sf::st_centroid(baus_sf_tmp)

keep_with_hits <- function(obs_sf) {
  hits <- sf::st_intersects(obs_sf, ctr, sparse = TRUE)
  obs_sf[lengths(hits) > 0, ]
}

obs_reg_sf  <- keep_with_hits(obs_reg_sf)
obs_prov_sf <- keep_with_hits(obs_prov_sf)

obs_reg_sp  <- methods::as(obs_reg_sf,  "Spatial")
obs_prov_sp <- methods::as(obs_prov_sf, "Spatial")

# ============================================================
# STEP 8 — BASIS FUNCTIONS (MULTI-RESOLUTION)
# ============================================================

set.seed(123)

if (sp::proj4string(obs_reg_sp) != sp::proj4string(obs_prov_sp)) {
  obs_prov_sp <- sp::spTransform(obs_prov_sp, sp::CRS(sp::proj4string(obs_reg_sp)))
}

obs_reg_sp  <- sp::spChFIDs(obs_reg_sp,  paste0("reg_",  seq_len(nrow(obs_reg_sp))))
obs_prov_sp <- sp::spChFIDs(obs_prov_sp, paste0("prov_", seq_len(nrow(obs_prov_sp))))

obs_all_sp <- rbind(obs_reg_sp, obs_prov_sp)

B <- FRK::auto_basis(
  manifold = FRK::plane(),
  data     = obs_all_sp,
  nres     = nres_use,
  regular  = 0,
  type     = "Gaussian"
)

# (Optional) visual check:
# FRK::show_basis(B); plot(obs_reg_sp, add = TRUE)

# ============================================================
# STEP 9 — FRK MODEL (Gamma-link = log)
# ============================================================

form_FRK <- as.formula(paste("value ~", paste(covars, collapse = " + ")))
log_step("FRK formula and basis constructed.")

baus_poly_utm <- sf::st_transform(grid_BAUs_allcov, EPSG_UTM) |>
  dplyr::mutate(idx = dplyr::row_number())

inter_bau_dom <- suppressWarnings(
  sf::st_intersection(baus_poly_utm[, "idx"], sf::st_make_valid(domain_utm))
)
wts_tbl <- inter_bau_dom |>
  dplyr::mutate(w_area = as.numeric(sf::st_area(geometry))) |>
  sf::st_drop_geometry() |>
  dplyr::group_by(idx) |>
  dplyr::summarise(wts = sum(w_area, na.rm = TRUE), .groups = "drop")

wts_vec <- rep(0, nrow(baus_poly_utm))
wts_vec[wts_tbl$idx] <- wts_tbl$wts
baus_spdf@data$wts <- wts_vec
baus_spdf@data$wts[!is.finite(baus_spdf@data$wts) | baus_spdf@data$wts < 0] <- 0
baus_spdf@data$wts <- baus_spdf@data$wts / max(baus_spdf@data$wts)

stopifnot(all(is.finite(baus_spdf@data$wts)), any(baus_spdf@data$wts > 0))
stopifnot(all(is.finite(obs_reg_sf$value)),  nrow(obs_reg_sf) > 0)
stopifnot(all(is.finite(obs_prov_sf$value)), nrow(obs_prov_sf) > 0)
for (cn in covars) stopifnot(all(is.finite(baus_spdf@data[[cn]])))

S <- FRK::SRE(
  f        = form_FRK,
  data     = list(obs_prov_sp, obs_reg_sp),
  BAUs     = baus_spdf,
  basis    = B,
  response = "gamma",
  link     = "log",
  K_type   = "precision",
  average_in_BAU = TRUE,
  normalise_wts  = TRUE
)

S <- FRK::SRE.fit(S, method = "TMB", taper = 3)
log_step("FRK SRE model (Gamma-log) fitted with TMB.")

# ============================================================
# STEP 10 — PREDICTIONS & AGGREGATIONS
# ============================================================

CRS_M            <- EPSG_UTM
VAL_COL_DEFAULT  <- "mu"

get_pred_df <- function(obj) {
  if (is.list(obj)) {
    if ("BAUs" %in% names(obj)) obj <- obj[["BAUs"]] else obj <- obj[[1L]]
  }
  if (inherits(obj, "Spatial")) return(obj@data)
  if (inherits(obj, "sf"))      return(sf::st_drop_geometry(obj))
  if (is.data.frame(obj))       return(obj)
  abort(paste("Unknown prediction format:", paste(class(obj), collapse = ", ")))
}

pick_col <- function(df, candidates, exclude = character()) {
  nn  <- names(df)
  hit <- setdiff(intersect(candidates, nn), exclude)
  if (!length(hit))
    abort(paste0("None of {", paste(candidates, collapse = ", "),
                 "} found in: ", paste(nn, collapse = ", ")))
  df[[hit[1L]]]
}

build_pred_sf <- function(grid_bau_sf, S,
                          crs_m = CRS_M,
                          mu_candidates = c("p_Z","mean","mu","response","Mean","pred"),
                          drop_cols = c("RMSPE","RMSPE_Z")) {
  pred_resp <- predict(S, obs_fs = FALSE, type = "response")
  pred_link <- predict(S, obs_fs = FALSE, type = "link")      # kept for completeness
  
  df_resp <- get_pred_df(pred_resp)
  mu_hat  <- pick_col(df_resp, mu_candidates, exclude = drop_cols)
  se_hat  <- if ("RMSPE_Z" %in% names(df_resp)) df_resp[["RMSPE_Z"]] else NULL
  q5      <- if ("Z_percentile_5"  %in% names(df_resp)) df_resp[["Z_percentile_5"]]  else NULL
  q95     <- if ("Z_percentile_95" %in% names(df_resp)) df_resp[["Z_percentile_95"]] else NULL
  
  pred_sf <- sf::st_transform(grid_bau_sf, crs_m)
  pred_sf$mu <- as.numeric(mu_hat)
  if (!is.null(se_hat)) pred_sf$se  <- as.numeric(se_hat)
  if (!is.null(q5))     pred_sf$q5  <- as.numeric(q5)
  if (!is.null(q95))    pred_sf$q95 <- as.numeric(q95)
  pred_sf
}

agg_weighted_mean <- function(polys_sf, fine_sf, value_col = VAL_COL_DEFAULT,
                              id_col = NULL, crs_m = CRS_M) {
  stopifnot(inherits(polys_sf, "sf"), inherits(fine_sf, "sf"))
  geom_pol  <- attr(polys_sf, "sf_column")
  geom_fine <- attr(fine_sf,  "sf_column")
  
  polys_sf <- polys_sf |> st_make_valid() |> st_transform(crs_m)
  fine_sf  <- fine_sf  |> st_make_valid() |> st_transform(crs_m)
  
  if (is.null(id_col) || !(id_col %in% names(polys_sf))) {
    id_col <- "..id"
    polys_sf[[id_col]] <- seq_len(nrow(polys_sf))
  }
  
  polys_key <- polys_sf |> dplyr::select(all_of(c(id_col, geom_pol)))
  fine_val  <- fine_sf  |> dplyr::select(all_of(c(value_col, geom_fine)))
  
  inter <- suppressWarnings(sf::st_intersection(fine_val, polys_key))
  if (nrow(inter) == 0) {
    return(polys_sf |>
             dplyr::mutate(density_hat = NA_real_) |>
             dplyr::select(all_of(c(id_col, geom_pol, "density_hat"))))
  }
  
  inter$w_area <- as.numeric(sf::st_area(inter))
  
  res_attr <- inter |>
    dplyr::group_by(.data[[id_col]]) |>
    dplyr::summarise(
      num = sum(.data[[value_col]] * w_area, na.rm = TRUE),
      den = sum(w_area, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(density_hat = ifelse(den > 0, num / den, NA_real_)) |>
    dplyr::select(all_of(c(id_col, "density_hat"))) |>
    sf::st_drop_geometry()
  
  polys_sf |>
    dplyr::left_join(res_attr, by = id_col) |>
    dplyr::select(all_of(c(id_col, geom_pol, "density_hat")))
}

to_absolute <- function(polys_sf, density_col = "density_hat", crs_m = CRS_M) {
  stopifnot(inherits(polys_sf, "sf"))
  if (!density_col %in% names(polys_sf)) {
    stop("to_absolute(): column '", density_col, "' not found.")
  }
  polys_sf <- sf::st_transform(polys_sf, crs_m)
  areas_m2 <- as.numeric(sf::st_area(sf::st_geometry(polys_sf)))
  polys_sf$area_m2   <- areas_m2
  polys_sf$value_hat <- polys_sf[[density_col]] * polys_sf$area_m2
  polys_sf
}

eval_metrics <- function(pred_abs_sf, obs_tab,
                         by = "COD_PROV",
                         obs_col = "gdp_const_p_2015") {
  stopifnot(all(c(by, obs_col) %in% names(obs_tab)))
  res <- pred_abs_sf |>
    sf::st_drop_geometry() |>
    dplyr::left_join(obs_tab, by = by)
  
  yhat <- res[["value_hat"]]
  y    <- res[[obs_col]]
  
  mae  <- mean(abs(yhat - y), na.rm = TRUE)
  rmse <- sqrt(mean((yhat - y)^2, na.rm = TRUE))
  r2   <- if (stats::sd(yhat, na.rm = TRUE) > 0 &&
              stats::sd(y,    na.rm = TRUE) > 0) {
    stats::cor(yhat, y, use = "complete.obs")^2
  } else {
    NA_real_
  }
  
  list(
    MAE      = mae,
    RMSE     = rmse,
    R2       = r2,
    sum_pred = sum(yhat, na.rm = TRUE),
    sum_obs  = sum(y,    na.rm = TRUE)
  )
}

# Calibration helpers
calibrate_global <- function(pred_bau_sf, alpha) {
  pred_bau_sf |> dplyr::mutate(mu_cal = .data[[VAL_COL_DEFAULT]] * alpha)
}

alpha_from_totals <- function(pred_abs_sf, obs_tab,
                              by = "COD_PROV",
                              obs_col = "gdp_const_p_2015") {
  res <- pred_abs_sf |>
    sf::st_drop_geometry() |>
    dplyr::left_join(obs_tab, by = by)
  sum_obs <- sum(res[[obs_col]], na.rm = TRUE)
  sum_hat <- sum(res$value_hat,   na.rm = TRUE)
  if (is.finite(sum_hat) && sum_hat > 0) sum_obs / sum_hat else 1
}

calibrate_by_province <- function(pred_bau_sf, prov_sf, pred_abs_prov_sf, obs_tab,
                                  by = "COD_PROV", obs_col = "gdp_const_p_2015",
                                  crs_m = CRS_M) {
  prov_alphas <- pred_abs_prov_sf |>
    sf::st_drop_geometry() |>
    dplyr::select({{by}}, pred_prov = value_hat) |>
    dplyr::left_join(obs_tab, by = by) |>
    dplyr::mutate(alpha_prov = ifelse(
      is.finite(pred_prov) & pred_prov > 0,
      .data[[obs_col]] / pred_prov, 1
    )) |>
    dplyr::select(all_of(by), alpha_prov)
  
  prov_alpha_sf <- prov_sf |>
    st_make_valid() |> st_transform(crs_m) |>
    dplyr::select(all_of(by), geometry) |>
    dplyr::left_join(prov_alphas, by = by)
  
  fine_sf <- pred_bau_sf |>
    st_make_valid() |> st_transform(crs_m) |>
    dplyr::select(!!VAL_COL_DEFAULT, geometry)
  
  inter_fp <- suppressWarnings(st_intersection(fine_sf, prov_alpha_sf))
  if (nrow(inter_fp) == 0) abort("Empty BAU–province intersection (CRS/validity issue).")
  
  inter_fp$mu_cal <- inter_fp[[VAL_COL_DEFAULT]] * inter_fp$alpha_prov
  inter_fp |> dplyr::select(mu_cal, geometry)
}

# Predictions at BAU level
pred_sf <- build_pred_sf(grid_BAUs_allcov, S, crs_m = CRS_M)
log_step("BAU-level predictions computed.")

# Aggregation to provinces and municipalities
prov_pred_density <- agg_weighted_mean(Provinciale_REG_sh, pred_sf,
                                       "mu", "COD_PROV", CRS_M)
com_pred_density  <- agg_weighted_mean(dplyr::filter(sh_com, COD_REG %in% REGIONI),
                                       pred_sf, "mu", "PRO_COM", CRS_M)

prov_pred_abs <- to_absolute(prov_pred_density, "density_hat", CRS_M)
com_pred_abs  <- to_absolute(com_pred_density,  "density_hat", CRS_M)

prov_obs_tab <- Provinciale_REG_sh |>
  dplyr::select(COD_PROV, gdp_const_p_2015) |>
  sf::st_drop_geometry()

m_raw <- eval_metrics(prov_pred_abs, prov_obs_tab,
                      by = "COD_PROV",
                      obs_col = "gdp_const_p_2015")
print(m_raw)
log_step("Provincial/municipal aggregations and raw metrics computed.")