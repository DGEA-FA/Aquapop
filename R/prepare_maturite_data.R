#' Prépare les données pour l’ajustement d’un modèle de maturité (L50 ou A50)
#'
#' @param specimen_data Jeu de données contenant les colonnes `maturite`, `sexe`, et la variable quantitative (`ltm` ou `age`)
#' @param variable Variable quantitative à utiliser : `"ltm"` (par défaut) ou `"age"`
#'
#' @return Un data.frame filtré et transformé prêt pour le modèle
#' @export
prepare_maturite_data <- function(specimen_data, variable = c("ltm", "age")) {
  variable <- match.arg(variable)
  
  specimen_data %>%
    filter(
      maturite != "IND",
      sexe != "IND",
      !is.na(.data[[variable]])
    ) %>%
    droplevels() %>%
    mutate(
      maturite = factor(maturite, levels = c("N", "O"), ordered = TRUE),
      sexe = relevel(factor(sexe), ref = "F")
    )
}
