#' Générer un tableau récapitulatif d’un inventaire ichtyologique
#'
#' Cette fonction produit un tableau synthèse des métadonnées d’un inventaire ichtyologique
#' réalisé sur un lac donné, pour un type de pêche et une ou plusieurs années.
#'
#' @inheritParams count_filtered
#'
#' @return Un `data.frame` structuré avec une ligne par type d'information
#' et une colonne par type de pêche. La première colonne est nommée `"Type de pêche"`.
#'
#' @export
#'
#' @examples
#' # Exemple fictif :
#' recap <- generate_recapitulatif_inventaire(data_lac = lac_filtre, data_station = stations_filtrees)
#' print(recap)
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

#' Compter les lignes correspondant à une valeur spécifique dans une colonne
#'
#' Fonction utilitaire interne utilisée pour compter le nombre d’occurrences
#' d’une valeur donnée dans une colonne d’un `data.frame`.
#'
#' @param df Un `data.frame`
#' @param col Le nom de la colonne à tester (chaîne de caractères)
#' @param val La valeur à compter dans cette colonne
#'
#' @return Un entier représentant le nombre de lignes correspondant
#' @keywords internal
count_filtered <- function(df, col, val) {
  df %>%
    dplyr::filter(.data[[col]] == val) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::pull(n)
}

