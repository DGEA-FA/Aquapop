#' Résumé des métadonnées d'un inventaire
#'
#' @param data_lac Un data.frame filtré sur un lac/type de pêche/année, contenant les données du feuillet "Lac".
#' @param data_station Un data.frame correspondant au feuillet "Stations", déjà filtré sur le même jeu.
#'
#' @return Un tableau (data.frame) avec une ligne par type de variable et une colonne par type de pêche.
#' @export
generate_recapitulatif_inventaire <- function(data_lac, data_station) {
  
  # Informations issues du feuillet "Lac"
  info_lac <- data_lac %>% reframe(
    "Type de pêche" = unique(typ_pech),
    "No de lac" = unique(no_lac),
    "Nom du lac" = unique(nom_lac),
    "Superficie du lac (ha)" = unique(superficie_ha),
    "Année(s) de l’inventaire (aaaa)" = toString(sort(unique(annee)))
  )
  
  # Dates de début/fin d’inventaire
  date_info <- data_station %>% reframe(
    "Date de début de l’inventaire (aaaa-mm-jj)" = {
      if (all(is.na(date_pose))) "Aucune donnée disponible" else min(date_pose, na.rm = TRUE)
    },
    "Date de fin de l’inventaire (aaaa-mm-jj)" = {
      if (all(is.na(date_leve))) "Aucune donnée disponible" else max(date_leve, na.rm = TRUE)
    }
  )
  
  # Fonction auxiliaire pour compter avec filtre
  count_filtered <- function(df, col, val) {
    df %>% filter(.data[[col]] == val) %>% summarise(n = n()) %>% pull(n)
  }
  
  # Comptages des stations
  count_info <- tibble::tibble(
    "N stations aléatoires" = count_filtered(data_station, "st_hasard", "O"),
    "N stations dirigées"   = count_filtered(data_station, "st_hasard", "N"),
    "N stations valides"    = count_filtered(data_station, "st_valide", "O"),
    "N stations invalides"  = count_filtered(data_station, "st_valide", "N"),
    "N stations total"      = nrow(data_station)
  )
  
  # Construction finale du tableau : rotation pour affichage vertical
  recap <- bind_cols(info_lac, date_info, count_info)
  recap[-1] %>% t() %>% as.data.frame() %>% setNames(recap[, 1]) %>%
    tibble::rownames_to_column("Type de pêche")
}
