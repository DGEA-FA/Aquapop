#' Générer un tableau récapitulatif d’un inventaire ichtyologique
#'
#' Cette fonction produit un tableau synthèse des métadonnées d’un inventaire ichtyologique
#' réalisé sur un lac donné, pour un type de pêche et une ou plusieurs années.
#'
#' @param data_lac Un `data.frame` contenant les métadonnées du lac (feuillet "Lac"),
#'                 déjà filtré sur un seul lac, un type de pêche et une ou plusieurs années.
#' @param data_station Un `data.frame` contenant les données des stations (feuillet "Station"),
#'                     déjà filtré de manière cohérente avec `data_lac`.
#'
#' @return Un `data.frame` structuré avec une ligne par type d'information
#'         et une colonne par type de pêche. La première colonne est `"Type de pêche"`.
#'         
#' @importFrom tibble rownames_to_column tibble
#' @importFrom stats setNames
#' @importFrom dplyr reframe bind_cols n summarise filter pull
#' @export
generate_recapitulatif_inventaire <- function(data_lac, data_station) {
  
  # --- Informations générales sur le lac ---
  info_lac <- data_lac |> reframe(
    `Type de pêche` = unique(typ_pech),
    `No de lac` = unique(no_lac),
    `Nom du lac` = unique(nom_lac),
    `Superficie du lac (ha)` = unique(superficie_ha),
    `Année(s) de l’inventaire (aaaa)` = toString(sort(unique(annee)))
  )
  
  # --- Dates de l’inventaire ---
  date_info <- data_station |> reframe(
    `Date de début de l’inventaire (aaaa-mm-jj)` = {
      if (all(is.na(date_pose))) "Aucune donnée disponible" else min(date_pose, na.rm = TRUE)
    },
    `Date de fin de l’inventaire (aaaa-mm-jj)` = {
      if (all(is.na(date_leve))) "Aucune donnée disponible" else max(date_leve, na.rm = TRUE)
    }
  )
  
  # --- Comptage des stations ---
  count_info <- tibble(
    `N stations aléatoires` = generate_recapitulatif_compter_valeurs(data_station, "st_hasard", "O"),
    `N stations dirigées`   = generate_recapitulatif_compter_valeurs(data_station, "st_hasard", "N"),
    `N stations valides`    = generate_recapitulatif_compter_valeurs(data_station, "st_valide", "O"),
    `N stations invalides`  = generate_recapitulatif_compter_valeurs(data_station, "st_valide", "N"),
    `N stations total`      = nrow(data_station)
  )
  
  # --- Fusion et transformation du tableau pour affichage vertical ---
  tableau_recapitulatif <- bind_cols(info_lac, date_info, count_info)
  tableau_recapitulatif[-1] |>
    t() |>
    as.data.frame() |>
    setNames(tableau_recapitulatif[[1]]) |>
    rownames_to_column("Type de pêche")
}

#' Compter les occurrences d’une valeur dans une colonne
#'
#' Fonction utilitaire interne utilisée dans le tableau récapitulatif.
#'
#' @param df Un `data.frame` contenant les données à filtrer
#' @param col Le nom de la colonne (chaîne de caractères)
#' @param val La valeur à compter dans cette colonne
#'
#' @return Un entier représentant le nombre de lignes correspondantes
#' @keywords internal
generate_recapitulatif_compter_valeurs <- function(df, col, val) {
  df |>
    filter(.data[[col]] == val) |>
    summarise(n = n()) |>
    pull(n)
}

