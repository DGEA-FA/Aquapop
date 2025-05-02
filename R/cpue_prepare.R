#' Préparer les données agrégées de CPUE par station
#'
#' Cette fonction prépare un tableau de CPUE (captures par unité d'effort) à l’échelle de la station, 
#' à partir des données de captures et des spécimens observés. Elle permet de calculer soit la CPUE totale,
#' soit la CPUE restreinte aux femelles, selon l’argument `group`.
#'
#' Elle suppose que les objets `capture` et `specimen` sont déjà filtrés pour ne contenir que les stations valides et aléatoires 
#' (par exemple via `get_analysis_data()`).
#'
#' @param capture Un `data.frame` de captures, contenant au minimum les colonnes `no_station`, `nb_capture`, `nb_pese`.
#' @param specimen Un `data.frame` de spécimens, déjà filtré pour l’espèce et les stations pertinentes.
#' @param group Une chaîne de caractères, `"tous"` (valeur par défaut) ou `"femelles"`, indiquant le groupe à analyser.
#'
#' @return Un `data.frame` contenant les colonnes suivantes :
#' \describe{
#'   \item{no_station}{Identifiant de la station}
#'   \item{CPUE}{Nombre de spécimens observés à la station, divisé par l’effort (station)}
#'   \item{Group}{Libellé du groupe analysé : `"Tous"` ou `"Femelles"`}
#' }
#'
#' @importFrom dplyr filter group_by summarise right_join n
#'
#' @export
cpue_prepare <- function(capture, specimen, group = c("tous", "femelles")) {
  group <- match.arg(group)
  
  # Appliquer le filtre si on veut seulement les femelles
  if (group == "femelles") {
    specimen <- filter(specimen, sexe == "F")
    group_label <- "Femelles"
  } else {
    group_label <- "Tous"
  }
  
  # Joindre les spécimens aux captures (right_join pour inclure toutes les stations)
  alldata <- right_join(specimen, capture, by = "no_station") |>
    filter(!is.na(no_station))
  
  # Calcul du nombre d’individus par station
  cpue_summary <- alldata |>
    group_by(no_station) |>
    summarise(
      CPUE = n(),
      Group = group_label,
      .groups = "drop"
    )
  
  return(cpue_summary)
}
