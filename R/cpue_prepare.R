#' Préparer les données agrégées de CPUE par station
#'
#' Cette fonction prépare un tableau de CPUE par station à partir des spécimens observés
#' et des captures enregistrées. Elle permet de calculer soit la CPUE totale, soit la CPUE
#' limitée aux femelles, selon l'argument `group`.
#'
#' Elle suppose que les objets `specimen` et `capture` sont déjà filtrés en amont pour ne
#' contenir que les stations valides et au hasard (via `get_analysis_data()`).
#'
#' @param capture Un `data.frame` de captures, avec au minimum `no_station`, `nb_capture`, `nb_pese`.
#' @param specimen Un `data.frame` de spécimens, déjà filtré pour l’espèce et les stations valides/hasard.
#' @param group Une chaîne `"tous"` (par défaut) ou `"femelles"`, indiquant le groupe à analyser.
#'
#' @return Un `data.frame` avec les colonnes :
#' \describe{
#'   \item{no_station}{Identifiant de la station}
#'   \item{CPUE}{Nombre de spécimens observés à cette station}
#'   \item{Group}{Libellé du groupe analysé : `"Tous"` ou `"Femelles"`}
#' }
#'
#' @export
cpue_prepare <- function(capture, specimen, group = c("tous", "femelles")) {
  group <- match.arg(group)
  
  # Appliquer le filtre si on veut seulement les femelles
  if (group == "femelles") {
    specimen <- dplyr::filter(specimen, sexe == "F")
    group_label <- "Femelles"
  } else {
    group_label <- "Tous"
  }
  
  # Joindre les spécimens aux captures (right_join pour inclure toutes les stations)
  alldata <- dplyr::right_join(specimen, capture, by = "no_station") |>
    dplyr::filter(!is.na(no_station))
  
  # Calcul du nombre d’individus par station
  cpue_summary <- alldata |>
    dplyr::group_by(no_station) |>
    dplyr::summarise(
      CPUE = dplyr::n(),
      Group = group_label,
      .groups = "drop"
    )
  
  return(cpue_summary)
}
