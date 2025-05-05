#' Préparer les données pour l’ajustement d’un modèle de maturité (L50 ou A50)
#'
#' Cette fonction filtre et structure les données en vue de l’ajustement d’un modèle de maturité.
#' Elle supprime les individus indéterminés (`maturite == "IND"` ou `sexe == "IND"`) et les valeurs manquantes
#' dans la variable quantitative (`ltm` ou `age`), puis définit les niveaux de facteurs attendus.
#'
#' @param specimen_data Un data.frame contenant les colonnes `maturite`, `sexe` et la variable quantitative (`ltm` ou `age`)
#' @param variable Variable quantitative à utiliser : "ltm" (par défaut) ou "age"
#' @param drop_levels Logique. Si TRUE (défaut), supprime les niveaux inutilisés après filtrage.
#'
#' @return Un data.frame filtré et prêt pour l’ajustement du modèle, contenant les colonnes d'origine avec
#'   `maturite` en facteur ordonné (`N` < `O`) et `sexe` centré sur les femelles (`F`, `M`).
#' @export
#' @importFrom dplyr filter mutate
#' @importFrom glue glue
#' @importFrom rlang .data
#' @importFrom checkmate assert_data_frame assert_choice
#'
#' @examples
#' library(dplyr)
#' data <- tribble(
#'   ~maturite, ~sexe, ~ltm,
#'   "O", "F", 350,
#'   "N", "F", 280,
#'   "IND", "M", 300,
#'   "O", "IND", 400,
#'   "O", "M", NA
#' )
#' maturite_prepare(data, variable = "ltm")

maturite_prepare <- function(specimen_data, variable = c("ltm", "age"), drop_levels = TRUE) {
  variable <- match.arg(variable)
  
  # Contrôles de base
  assert_data_frame(specimen_data, min.rows = 1, any.missing = TRUE)
  assert_choice(variable, c("ltm", "age"))
  
  required_cols <- c("maturite", "sexe", variable)
  if (!all(required_cols %in% names(specimen_data))) {
    stop(glue("❌ Le jeu de données doit contenir les colonnes : {paste(required_cols, collapse = ', ')}."))
  }
  
  res <- specimen_data |>
    filter(
      maturite != "IND",
      sexe != "IND",
      !is.na(.data[[variable]])
    ) |>
    mutate(
      maturite = factor(maturite, levels = c("N", "O"), ordered = TRUE),
      sexe = factor(sexe, levels = c("F", "M"))
    )
  
  # Message si trop de pertes
  if (nrow(res) < 10) {
    message(glue("⚠️ Attention : seulement {nrow(res)} lignes conservées après filtrage."))
  }
  
  if (drop_levels) res <- droplevels(res)
  return(res)
}
