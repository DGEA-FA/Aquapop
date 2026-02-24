#' Sélectionner le meilleur modèle de mortalité selon aicc et ajustement HNP
#'
#' Cette fonction identifie automatiquement le meilleur modèle parmi ceux comparés (Poisson, NB1, NB2, CMP, GP),
#' en priorisant les modèles avec un bon ajustement HNP (inférieur à 10 %). Si aucun modèle ne satisfait ce critère,
#' le modèle avec le plus faible aicc est sélectionné.
#'
#' @param tablemodele Un `data.frame` (souvent `mortalite_compare_modele_res$data`)
#'   contenant au minimum les colonnes :
#'   \describe{
#'     \item{methode}{Nom du modèle (ex. "Poisson", "NB1", "NB2", "CMP", "GP").}
#'     \item{aicc}{Critère d'information corrigé (numérique).}
#'     \item{ajustement_hnp}{Pourcentage d'ajustement HNP (numérique).}
#'   }
#'
#' @return Une chaîne de caractères correspondant au nom du meilleur modèle, ou `NA` si aucun modèle n’est sélectionnable.
#' @export
#'
#' @importFrom dplyr filter pull
#'
#' @examples
#' # Exemple minimal
#' df <- data.frame(
#'   methode = c("Poisson", "NB1", "NB2", "CMP", "GP"),
#'   aicc = c(112, 108, 109, 107, 106),
#'   ajustement_hnp = c(15, 12, 8, 6, 11)
#' )
#' mortalite_select_best_modele(df)
mortalite_select_best_modele <- function(tablemodele) {
  # Validation ----
  if (!all(c("methode", "aicc","ajustement_hnp") %in% names(tablemodele))) {
    stop("Le tableau fourni n’est pas valide. Il doit contenir les colonnes : 'methode', 'aicc' et 'ajustement_hnp'.")
  }
  
  # Filtrage des modèles bien ajustés (HNP < 10) ----
  modeles_bien_ajustes <- tablemodele |>
    filter(ajustement_hnp < 10)
  
  # Sélection finale selon le plus faible AICc ----
  if (nrow(modeles_bien_ajustes) > 0) {
    selection <- modeles_bien_ajustes |>
      filter(aicc == min(aicc, na.rm = TRUE)) |>
      pull(methode)
  } else {
    selection <- tablemodele |>
      filter(aicc == min(aicc, na.rm = TRUE)) |>
      pull(methode)
  }
  
  # Retourner la sélection (ou NA) ----
  if (length(selection) == 0 || is.na(selection[1])) {
    warning("Aucun modèle n’a pu être sélectionné.")
    return(NA)
  }
  
  return(selection[1])
}
