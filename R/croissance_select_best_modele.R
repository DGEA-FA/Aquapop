#' Sélectionner le meilleur modèle de croissance selon le plus bas AICc
#'
#' Cette fonction identifie automatiquement le meilleur modèle parmi
#' Von Bertalanffy, Gompertz ou Logistique en se basant sur le plus faible
#' critère d'information corrigé (AICc). En cas d'égalité, elle retourne
#' le premier modèle ex aequo.
#'
#' @param tablemodele Un `data.frame` produit par `croissance_compare_modele()`,
#'   contenant au minimum les colonnes `methode` et `aicc`.
#'
#' @return Une chaîne de caractères correspondant au nom du meilleur modèle sélectionné.
#'   Retourne `NA` avec un avertissement si aucun modèle ne peut être sélectionné.
#'
#' @examples
#' df <- tibble::tibble(
#'   methode = c("Von Bertalanffy", "Gompertz", "Logistique"),
#'   aicc = c(120.3, 118.5, 121.0)
#' )
#' croissance_select_best_modele(df)
#'
#' @export
#' @importFrom dplyr filter pull
croissance_select_best_modele <- function(tablemodele) {
  if (!"methode" %in% names(tablemodele) || !"aicc" %in% names(tablemodele)) {
    stop("Le tableau de modèle n’est pas valide. Assurez-vous qu’il provient bien de `croissance_compare_modele()`.")
  }
  
  best_row <- tablemodele |>
    filter(aicc == min(aicc, na.rm = TRUE)) |>
    pull(methode)
  
  if (length(best_row) == 0) {
    warning("Aucun modèle n’a pu être sélectionné.")
    return(NA)
  }
  
  return(best_row[1])
}
