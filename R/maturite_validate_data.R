#' Valider et préparer les données de maturité
#'
#' Fonction interne utilisée par les fonctions de modélisation de la maturité
#' pour gérer proprement les cas limites avant l'ajustement des modèles.
#'
#' Elle vérifie :
#' \itemize{
#'   \item la présence des colonnes requises ;
#'   \item si le jeu de données brut est vide ;
#'   \item si des données exploitables demeurent après nettoyage ;
#'   \item si le nombre d'individus restants est suffisant ;
#'   \item si les deux états de maturité (`N` et `O`) sont présents.
#' }
#'
#' @param specimen_data Un `data.frame` contenant les données de spécimens.
#' @param variable Chaîne indiquant la variable quantitative à utiliser :
#'   `"ltm"` ou `"age"`.
#' @param min_n Nombre minimal d'individus exploitables requis après nettoyage.
#'
#' @return Une liste contenant :
#' \describe{
#'   \item{success}{Booléen indiquant si les données sont exploitables}
#'   \item{data}{`data.frame` préparé pour la modélisation, ou `NULL`}
#'   \item{message}{Message explicatif si les données sont non exploitables}
#' }
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' data_exemple <- data.frame(
#'   maturite = c("N", "O", "IND", "O"),
#'   sexe = c("F", "M", "F", "IND"),
#'   ltm = c(250, 320, 280, 300),
#'   age = c(2, 4, 3, 5)
#' )
#'
#' maturite_validate_data(data_exemple, variable = "ltm", min_n = 2)
#' }
#' @importFrom checkmate assert_choice assert_data_frame
#' @importFrom dplyr filter mutate
#' @importFrom glue glue
#' @importFrom rlang .data
maturite_validate_data <- function(specimen_data,
                                   variable = c("ltm", "age"),
                                   min_n = 10) {
  variable <- match.arg(variable)
  
  # Validation des intrants ----
  assert_data_frame(specimen_data, any.missing = TRUE)
  assert_choice(variable, c("ltm", "age"))
  
  colonnes_requises <- c("maturite", "sexe", variable)
  colonnes_manquantes <- setdiff(colonnes_requises, names(specimen_data))
  
  if (length(colonnes_manquantes) > 0) {
    stop(
      glue(
        "Le jeu de données doit contenir les colonnes : {paste(colonnes_requises, collapse = ', ')}."
      )
    )
  }
  
  # Cas sans ligne ----
  if (nrow(specimen_data) == 0) {
    return(list(
      success = FALSE,
      data = NULL,
      message = paste(
        "Aucun spécimen valide disponible pour modéliser la maturité",
        "en fonction de la",
        ifelse(variable == "ltm", "longueur.", "de l'âge.")
      )
    ))
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
    ) |>
    droplevels()
  
  # Cas sans donnée exploitable après nettoyage ----
  if (nrow(data_preparee) == 0) {
    return(list(
      success = FALSE,
      data = NULL,
      message = paste(
        "Aucune donnée exploitable n'est disponible après le nettoyage des",
        "variables maturite, sexe et", variable, "."
      )
    ))
  }
  
  # Cas avec effectif insuffisant ----
  if (nrow(data_preparee) < min_n) {
    return(list(
      success = FALSE,
      data = NULL,
      message = glue(
        "Trop peu d'individus exploitables après nettoyage (n = {nrow(data_preparee)})."
      )
    ))
  }
  
  # Cas sans variation de maturité exploitable ----
  etats_maturite <- unique(as.character(stats::na.omit(data_preparee$maturite)))
  
  if (!all(c("N", "O") %in% etats_maturite)) {
    return(list(
      success = FALSE,
      data = NULL,
      message = paste(
        "Les données exploitables ne contiennent pas à la fois des individus",
        "immatures et matures. La modélisation de la maturité est impossible."
      )
    ))
  }
  
  # Retour ----
  return(list(
    success = TRUE,
    data = data_preparee,
    message = NULL
  ))
}