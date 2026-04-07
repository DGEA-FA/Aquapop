#' Générer une phrase descriptive pour le modèle de mortalité sélectionné
#'
#' Cette fonction génère une phrase interprétative à partir du modèle de
#' mortalité sélectionné et de la table de comparaison des modèles.
#'
#' Elle retourne `NULL` si les informations nécessaires ne sont pas disponibles.
#'
#' @param data_comparaison Un `data.frame` contenant au minimum les colonnes
#'   `methode` et `A`, généralement issu de `mortalite_compare_modele()$data`.
#' @param modele_nom Nom du modèle à décrire (ex: `"nb1"`).
#'
#' @return Une chaîne de caractères résumant le modèle et la mortalité annuelle,
#'   ou `NULL` si la phrase ne peut pas être générée.
#'
#' @importFrom glue glue
#'
#' @export
#'
#' @examples
#' df <- data.frame(methode = c("nb1", "poisson"), A = c(37, 51))
#' mortalite_phrase_resume(df, "nb1")
mortalite_phrase_resume <- function(data_comparaison, modele_nom) {
  # Validation de base ====
  if (is.null(modele_nom) || length(modele_nom) != 1 || is.na(modele_nom) || modele_nom == "") {
    return(NULL)
  }
  
  if (is.null(data_comparaison) || !is.data.frame(data_comparaison) || nrow(data_comparaison) == 0) {
    return(NULL)
  }
  
  if (!"methode" %in% names(data_comparaison)) {
    return(NULL)
  }
  
  # Trouver la ligne du modèle ====
  ligne <- data_comparaison[data_comparaison$methode == modele_nom, , drop = FALSE]
  
  if (nrow(ligne) == 0) {
    return(NULL)
  }
  
  modele_upper <- toupper(modele_nom)
  
  # Cas sans estimation de A ====
  if (!"A" %in% names(ligne) || is.na(ligne$A[1])) {
    return(
      glue("Le modèle {modele_upper} a été sélectionné, mais la mortalité annuelle n'est pas disponible.") |>
        as.character()
    )
  }
  
  # Phrase complète ====
  glue(
    "Le modèle {modele_upper} décrit le mieux la mortalité de la population. ",
    "La mortalité annuelle s'élève à {ligne$A[1]} %."
  ) |>
    as.character()
}