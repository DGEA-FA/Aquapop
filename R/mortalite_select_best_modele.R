#' Sélectionner le meilleur modèle de mortalité selon AICc et ajustement HNP
#'
#' Cette fonction identifie automatiquement le meilleur modèle parmi ceux
#' comparés (Poisson, NB1, NB2, CMP, GP), en priorisant les modèles ayant un
#' bon ajustement HNP (inférieur à 10 %). Si aucun modèle ne satisfait ce
#' critère, le modèle avec le plus faible AICc est sélectionné parmi les modèles
#' disponibles.
#'
#' La fonction retourne `NULL` si aucun modèle n'est sélectionnable.
#'
#' @param tablemodele Un `data.frame` généralement issu de
#'   `mortalite_compare_modele()$data`, contenant au minimum les colonnes :
#'   \describe{
#'     \item{methode}{Nom du modèle.}
#'     \item{aicc}{Critère d'information corrigé (numérique).}
#'     \item{ajustement_hnp}{Pourcentage d'ajustement HNP (numérique).}
#'     \item{convergence}{Booléen indiquant si le modèle est interprétable.}
#'   }
#'
#' @return Une chaîne de caractères correspondant au nom du meilleur modèle, ou
#'   `NULL` si aucun modèle n'est sélectionnable.
#'
#' @importFrom dplyr filter pull
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   methode = c("poisson", "nb1", "nb2", "cmp", "gp"),
#'   aicc = c(112, 108, 109, 107, 106),
#'   ajustement_hnp = c(15, 12, 8, 6, 11),
#'   convergence = c(TRUE, TRUE, TRUE, TRUE, TRUE)
#' )
#' mortalite_select_best_modele(df)
mortalite_select_best_modele <- function(tablemodele) {
  # Validation de base ====
  if (is.null(tablemodele) || !is.data.frame(tablemodele) || nrow(tablemodele) == 0) {
    return(NULL)
  }
  
  required_cols <- c("methode", "aicc", "ajustement_hnp")
  
  if (!all(required_cols %in% names(tablemodele))) {
    return(NULL)
  }
  
  # Filtrer les modèles sélectionnables ====
  modeles_valides <- tablemodele |>
    filter(!is.na(aicc))
  
  if ("convergence" %in% names(modeles_valides)) {
    modeles_valides <- modeles_valides |>
      filter(convergence %in% TRUE)
  }
  
  if (nrow(modeles_valides) == 0) {
    return(NULL)
  }
  
  # Priorité aux modèles bien ajustés selon HNP ====
  modeles_bien_ajustes <- modeles_valides |>
    filter(!is.na(ajustement_hnp), ajustement_hnp < 10)
  
  if (nrow(modeles_bien_ajustes) > 0) {
    selection <- modeles_bien_ajustes |>
      filter(aicc == min(aicc, na.rm = TRUE)) |>
      pull(methode)
  } else {
    selection <- modeles_valides |>
      filter(aicc == min(aicc, na.rm = TRUE)) |>
      pull(methode)
  }
  
  if (length(selection) == 0 || is.na(selection[1])) {
    return(NULL)
  }
  
  selection[1]
}