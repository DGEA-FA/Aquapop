# ==== Comparer les captures et les spécimens pour la CPUE ====

#' Comparer les captures et le nombre de spécimens des stations valides et au hasard
#'
#' Cette fonction compare le nombre de captures indiqué dans la table `capture`
#' et le nombre d'individus présents dans la table  `specimen` (stations valides et au hasard).
#' 
#' @param capture Un `data.frame` contenant au moins la colonnes `nb_capture`.
#' @param specimen Un `data.frame` contenant tous les spécimens de l'espèce cible, filtré pour les 
#' stations valides et au hasard seulement.
#'
#' @return Une liste contenant `success`, `message`, `data` et `flextable`.
#'
#' @export
#' @importFrom checkmate assert_data_frame assert_names
#' @importFrom dplyr count full_join group_by mutate select summarise case_when
#' @importFrom tidyr replace_na
#' @importFrom flextable flextable autofit set_header_labels
cpue_compare_capture_specimen <- function(capture, specimen) {
  assert_data_frame(capture, min.rows = 1)
  assert_data_frame(specimen, min.cols = 1)
  
  assert_names(names(capture), must.include = "nb_capture")

  capture_total <- sum(capture$nb_capture, na.rm = TRUE)
  nb_specimen_total <- nrow(specimen)
  ecart <- nb_specimen_total - capture_total
  
  data <- data.frame(
    capture_total,
    nb_specimen_total
  )
  
  
  message <- if (ecart != 0) {
    "Des écarts sont présents entre la table Récolte et la table Spécimens."
  } else {
    "Aucun écart détecté entre la table Récolte et la table Spécimens."
  }
  

  ft <- data |>
    flextable() |>
    set_header_labels(
      capture_total   = "Récolte",
      nb_specimen_total  = "Nb spécimens"
    ) |>
    style_flextable_aquapop()
  
  list(
    success = TRUE,
    message = message,
    data = data,
    flextable = ft
  )
}