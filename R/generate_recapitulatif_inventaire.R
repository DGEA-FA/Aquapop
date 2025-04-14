#' Générer un tableau récapitulatif d’un inventaire ichtyologique
#'
#' Cette fonction produit un tableau synthèse des métadonnées d’un inventaire ichtyologique
#' réalisé sur un lac donné, pour un type de pêche et une ou plusieurs années. Le tableau inclut :
#' - le nom et le numéro du lac, la superficie, les années couvertes ;
#' - les dates de début et de fin d’inventaire ;
#' - le nombre de stations selon leur type (aléatoire, dirigée) et leur statut (valide, invalide).
#'
#' Le tableau retourné est structuré verticalement (type d'information en ligne, type de pêche en colonne),
#' ce qui le rend adapté à une présentation en en-tête de rapport ou d’onglet introductif d’une application.
#'
#' @param data_lac Un `data.frame` contenant les métadonnées du lac (feuillet "Lac"),
#' déjà filtré sur un seul lac, un type de pêche et une ou plusieurs années. Doit inclure les colonnes
#' `typ_pech`, `no_lac`, `nom_lac`, `superficie_ha` et `annee`.
#'
#' @param data_station Un `data.frame` contenant les données de stations (feuillet "Stations"),
#' filtré sur les mêmes critères. Doit inclure les colonnes `date_pose`, `date_leve`, `st_hasard`, `st_valide`.
#'
#' @return Un tableau (`data.frame`) structuré avec une ligne par type d'information et
#' une colonne par type de pêche. La première colonne est nommée `"Type de pêche"`.
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
