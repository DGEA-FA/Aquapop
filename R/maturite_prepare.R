#' Préparer les données pour l’ajustement d’un modèle de maturité (L50 ou A50)
#'
#' Cette fonction filtre et structure les données en vue de l’ajustement d’un modèle de maturité.
#' Elle supprime les individus indéterminés (`maturite == "IND"` ou `sexe == "IND"`) et les valeurs manquantes
#' dans la variable quantitative (`ltm` ou `age`), puis définit les niveaux de facteurs attendus.
#'
#' @param specimen_data Un data.frame contenant les colonnes `maturite`, `sexe` et `ltm` ou `age`
#' @param variable Variable quantitative à utiliser : `"ltm"` (par défaut) ou `"age"`
#'
#' @return Un data.frame filtré et prêt pour l’ajustement du modèle
#' @export
#' @importFrom dplyr filter mutate
#' @importFrom glue glue
#' @importFrom rlang .data
#' @importFrom checkmate assert_data_frame assert_choice
maturite_prepare <- function(specimen_data, variable = c("ltm", "age")) {
  variable <- match.arg(variable)
  
  # Vérifications robustes
  assert_data_frame(specimen_data, min.rows = 1, any.missing = TRUE)
  assert_choice(variable, c("ltm", "age"))
  
  required_cols <- c("maturite", "sexe", variable)
  if (!all(required_cols %in% names(specimen_data))) {
    stop(glue("❌ Le jeu de données doit contenir les colonnes : {paste(required_cols, collapse = ', ')}."))
  }
  
  specimen_data |>
    filter(
      maturite != "IND",
      sexe != "IND",
      !is.na(.data[[variable]])
    ) |>
    mutate(
      maturite = factor(maturite, levels = c("N", "O"), ordered = TRUE),
      sexe = factor(sexe, levels = c("F", "M"))
    ) |>
    droplevels()
}
