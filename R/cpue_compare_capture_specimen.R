# ==== Comparer les captures et les spécimens pour la CPUE ====

#' Comparer le nombre de captures et de spécimens par station
#'
#' Cette fonction compare, pour chaque station, le nombre de captures indiqué
#' dans la table `capture` et le nombre d'individus présents dans la table
#' `specimen`.
#'
#' @param capture Un `data.frame` contenant au moins les colonnes `no_station`
#'   et `nb_capture`.
#' @param specimen Un `data.frame` contenant au moins la colonne `no_station`.
#'
#' @return Une liste contenant `success`, `message`, `data` et `flextable`.
#'
#' @export
#' @importFrom checkmate assert_data_frame assert_names
#' @importFrom dplyr count full_join group_by mutate select summarise case_when
#' @importFrom tidyr replace_na
#' @importFrom flextable flextable autofit
cpue_compare_capture_specimen <- function(capture, specimen) {
  assert_data_frame(capture, min.rows = 1)
  assert_data_frame(specimen, min.cols = 1)
  
  assert_names(names(capture), must.include = c("no_station", "nb_capture"))
  assert_names(names(specimen), must.include = "no_station")
  
  capture_par_station <- capture |>
    group_by(no_station) |>
    summarise(
      nb_capture_recolte = sum(nb_capture, na.rm = TRUE),
      .groups = "drop"
    )
  
  specimen_par_station <- specimen |>
    count(no_station, name = "nb_specimens")
  
  data <- capture_par_station |>
    full_join(specimen_par_station, by = "no_station") |>
    mutate(
      nb_capture_recolte = replace_na(nb_capture_recolte, 0),
      nb_specimens = replace_na(nb_specimens, 0L),
      ecart = nb_specimens - nb_capture_recolte,
      interpretation = case_when(
        ecart == 0 ~ "Aucun écart",
        ecart < 0 ~ "Capture supérieure aux spécimens",
        ecart > 0 ~ "Spécimens supérieurs à la capture"
      )
    ) |>
    select(
      no_station,
      nb_capture_recolte,
      nb_specimens,
      ecart,
      interpretation
    )
  
  message <- if (any(data$ecart != 0)) {
    "Des écarts sont présents entre la table Récolte et la table Spécimens."
  } else {
    "Aucun écart détecté entre la table Récolte et la table Spécimens."
  }
  
  ft <- data |>
    flextable() |>
    set_header_labels(
      no_station   = "No station",
      nb_capture_recolte = "N récolte",
      nb_specimens  = "N spécimens",
      ecart     = "Écart",
      interpretation   = "Interprétation"
    ) |>
    style_flextable_aquapop()
  
  list(
    success = TRUE,
    message = message,
    data = data,
    flextable = ft
  )
}