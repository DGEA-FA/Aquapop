#' Préparation des données pour l’analyse L50
#'
#' Cette fonction filtre et formate les données de spécimens pour les analyses de longueur à la maturité sexuelle (L50).
#'
#' @param specimen_data Un data.frame contenant au minimum les colonnes `ltm`, `maturite` et `sexe`.
#'
#' @return Un data.frame filtré, avec facteurs ordonnés pour `maturite` (N < O) et `sexe` (référence = "F").
#' @export
#'
#' @examples
#' df_clean <- prepare_maturite_l50_data(specimen_data)
#'
prepare_maturite_l50_data <- function(specimen_data) {
  specimen_data %>%
    filter(
      maturite != "IND",
      sexe != "IND",
      !is.na(ltm)
    ) %>%
    droplevels() %>%
    mutate(
      maturite = factor(maturite, levels = c("N", "O"), ordered = TRUE),
      sexe = relevel(factor(sexe), ref = "F")
    )
}
