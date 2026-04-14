#' Valider et préparer les données de maturité
#'
#' Fonction interne utilisée par les fonctions de modélisation de la maturité
#' pour gérer proprement les cas limites avant l'ajustement des modèles.
#'
#' Elle vérifie :
#' \itemize{
#'   \item la présence des colonnes requises ;
#'   \item si le jeu de données brut est vide ;
#'   \item si des données sont disponibles pour la variable demandée (`ltm` ou `age`) ;
#'   \item si des données exploitables demeurent après nettoyage ;
#'   \item si les deux états de maturité (`N` et `O`) sont présents.
#' }
#'
#' Contrairement aux versions précédentes, aucun seuil minimal d'effectif (`n`)
#' n'est imposé. Si les données sont présentes mais insuffisantes pour ajuster
#' correctement un modèle, cela sera détecté plus tard lors de l'ajustement
#' (ex. : non-convergence des modèles).
#'
#' @param specimen_data Un `data.frame` contenant les données de spécimens.
#' @param variable Chaîne indiquant la variable quantitative à utiliser :
#'   `"ltm"` ou `"age"`.
#'
#' @return Une liste contenant :
#' \describe{
#'   \item{success}{Booléen indiquant si les données sont exploitables}
#'   \item{data}{`data.frame` préparé pour la modélisation, ou `NULL`}
#'   \item{message}{Message explicatif si les données sont non exploitables}
#' }
#'
#' @details
#' Cette fonction distingue explicitement le cas où aucune donnée n'est disponible
#' pour la variable demandée (`ltm` ou `age`). Dans ce cas, un message spécifique
#' est retourné. Dans tous les autres cas, les données sont transmises aux fonctions
#' d'ajustement, même si elles sont limitées.
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
#' maturite_validate_data(data_exemple, variable = "ltm")
#' }
#'
#' @importFrom checkmate assert_choice assert_data_frame
#' @importFrom dplyr filter mutate
#' @importFrom glue glue
#' @importFrom rlang .data
maturite_validate_data <- function(specimen_data,
                                   variable = c("ltm", "age")) {
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
        "en fonction de",
        ifelse(variable == "ltm", "la longueur.", "l'âge.")
      )
    ))
  }
  
  # Cas particulier : aucune donnée disponible pour la variable demandée ----
  if (all(is.na(specimen_data[[variable]]))) {
    return(list(
      success = FALSE,
      data = NULL,
      message = if (variable == "age") {
        "Il n'y a pas d'âge disponible dans ce jeu de données, ce qui empêche la modélisation de l'âge à maturité."
      } else {
        "Il n'y a pas de longueur disponible dans ce jeu de données, ce qui empêche la modélisation de la longueur à maturité."
      }
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