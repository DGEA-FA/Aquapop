#' Préparer les données pour l'ajustement d'un modèle de maturité (L50 ou A50)
#'
#' Cette fonction filtre et structure les données en vue de l'ajustement d'un
#' modèle de maturité. Elle retire les individus indéterminés
#' (`maturite == "IND"` ou `sexe == "IND"`) ainsi que les valeurs manquantes
#' dans la variable quantitative choisie (`ltm` ou `age`), puis définit les
#' niveaux de facteurs attendus.
#'
#' Cette fonction ne valide pas si les données sont suffisantes pour ajuster
#' un modèle. Cette responsabilité revient à `maturite_validate_data()`.
#'
#' @param specimen_data Un `data.frame` contenant les colonnes `maturite`,
#'   `sexe` et la variable quantitative (`ltm` ou `age`).
#' @param variable Variable quantitative à utiliser : `"ltm"` (par défaut) ou
#'   `"age"`.
#' @param drop_levels Logique. Si `TRUE` (défaut), supprime les niveaux
#'   inutilisés après filtrage.
#'
#' @return Un `data.frame` filtré et prêt pour l'ajustement du modèle, contenant
#'   les colonnes d'origine avec `maturite` en facteur ordonné (`N` < `O`) et
#'   `sexe` en facteur (`F`, `M`).
#'
#' @export
#'
#' @importFrom checkmate assert_choice assert_data_frame
#' @importFrom dplyr filter mutate
#' @importFrom glue glue
#' @importFrom rlang .data
#'
#' @examples
#' data_exemple <- data.frame(
#'   maturite = c("O", "N", "IND", "O", "O"),
#'   sexe = c("F", "F", "M", "IND", "M"),
#'   ltm = c(350, 280, 300, 400, NA),
#'   age = c(4, 3, 2, 5, 6)
#' )
#'
#' maturite_prepare(data_exemple, variable = "ltm")
maturite_prepare <- function(specimen_data,
                             variable = c("ltm", "age"),
                             drop_levels = TRUE) {
  variable <- match.arg(variable)
  
  # Validation des intrants ----
  assert_data_frame(specimen_data, any.missing = TRUE)
  assert_choice(variable, c("ltm", "age"))
  
  colonnes_requises <- c("maturite", "sexe", variable)
  
  if (!all(colonnes_requises %in% names(specimen_data))) {
    stop(
      glue(
        "Le jeu de données doit contenir les colonnes : {paste(colonnes_requises, collapse = ', ')}."
      )
    )
  }
  
  # Préparation des données ----
  data_preparee <- specimen_data |>
    filter(
      maturite != "IND",
      sexe != "IND",
      !is.na(.data[[variable]])
    ) |>
    mutate(
      maturite = factor(maturite, levels = c("N", "O"), ordered = TRUE),
      sexe = factor(sexe, levels = c("F", "M"))
    )
  
  if (isTRUE(drop_levels)) {
    data_preparee <- droplevels(data_preparee)
  }
  
  return(data_preparee)
}