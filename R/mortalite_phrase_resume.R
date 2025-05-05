#' Générer une phrase descriptive pour le modèle de mortalité sélectionné
#'
#' @importFrom glue glue
#' @param data_comparaison data.frame contenant les colonnes `Méthode` et `A`
#' @param modele_nom Nom du modèle à décrire (ex: "NB1")
#'
#' @return Une chaîne de caractères résumant le modèle et la mortalité annuelle
#' @export
#'
#' @examples
#' df <- data.frame(Méthode = c("NB1", "Poisson"), A = c(37, 51))
#' mortalite_phrase_resume(df, "NB1")
#'
mortalite_phrase_resume <- function(data_comparaison, modele_nom) {
  if (missing(modele_nom) || is.null(modele_nom) || modele_nom == "") {
    stop("Le nom du modèle est invalide ou manquant.")
  }
  
  if (is.null(data_comparaison) || nrow(data_comparaison) == 0) {
    stop("Aucune donnée de comparaison disponible.")
  }
  
  if (!"Méthode" %in% names(data_comparaison)) {
    stop("La colonne 'Méthode' est manquante dans les données.")
  }
  
  ligne <- data_comparaison[data_comparaison$Méthode == modele_nom, , drop = FALSE]
  
  if (nrow(ligne) == 0) {
    stop(glue("Modèle {modele_nom} non trouvé dans les résultats."))
  }
  
  modele_upper <- toupper(modele_nom)
  
  if (!"A" %in% names(ligne) || is.na(ligne$A)) {
    return(glue("Le modèle {modele_upper} a été sélectionné, mais la mortalité annuelle n’est pas disponible."))
  }
  
  glue("Le modèle {modele_upper} décrit le mieux la mortalité de la population. La mortalité annuelle s’élève à {ligne$A} %.") |> as.character()
}
