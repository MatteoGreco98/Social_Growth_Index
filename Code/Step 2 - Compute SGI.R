# Funzione Copeland

library(sf)
library(dplyr)
library(purrr)

GDP_Municipalities_10_22 <- st_read('Social_Growth_Index/Output/GDP_Municipalities_10_22.gpkg')

copeland_with_rank_2 <- function(data,
                                 id_cols = c("codice_comune", "anno")) {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Il pacchetto 'dplyr' è necessario.")
  }
  if (!all(id_cols %in% names(data))) {
    stop("Le colonne identificative indicate non esistono nel dataset.")
  }
  
  # 1) separo criteri numerici
  crit <- data |>
    dplyr::select(-dplyr::all_of(id_cols)) |>
    dplyr::select(where(is.numeric))
  
  ids  <- data |>
    dplyr::select(dplyr::all_of(id_cols))
  
  n <- nrow(crit)
  if (n == 0 || ncol(crit) == 0)
    stop("Il dataset non contiene righe o colonne numeriche utili.")
  
  # 2) rank decrescente per ogni criterio (valore alto = "meglio")
  ranks_mat <- apply(-as.matrix(crit), 2, rank, ties.method = "average")
  
  # 3) Copeland score (vittorie - sconfitte)
  copeland <- rowSums((n + 1) - 2 * ranks_mat)
  
  # 4) output finale:
  #    - si ordina in modo CRESCENTE per Copeland.Score
  #    - Rank 1 = Copeland più basso (peggiore)
  #    - Rank max = Copeland più alto (migliore)
  #    - Decile 10 = migliori, Quartile 4 = migliori
  dplyr::bind_cols(ids,
                   Copeland.Score = copeland) |>
    dplyr::arrange(Copeland.Score) |>               # <–– cambia qui (era desc)
    dplyr::mutate(
      Rank     = dplyr::row_number(),
      Quartile = dplyr::ntile(Rank, 4),
      Decile   = dplyr::ntile(Rank, 10)
    )
}

df_base <- GDP_Municipalities_10_22

vars_crit <- c("GDP_pro_capite", "GDP_per_km2_euro", "dens_pop_ha")

anni <- sort(unique(df_base$anno))

copeland_by_year <- map_dfr(
  anni,
  function(a) {
    df_a <- df_base %>%
      filter(anno == a) %>%
      select(codice_comune, anno, all_of(vars_crit)) %>%
      filter(if_all(all_of(vars_crit), is.finite))
    
    if (nrow(df_a) == 0) return(tibble())
    
    copeland_with_rank_2(
      data    = df_a,
      id_cols = c("codice_comune", "anno")
    )
  }
)

copeland_by_year %>% 
  group_by(anno) %>% 
  slice_min(Rank, n = 10)


