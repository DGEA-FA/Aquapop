#' Sélectionner le meilleur modèle CPUE selon le plus bas AICc
#'
#' Cette fonction identifie automatiquement le meilleur modèle d’abondance
#' (CPUE) parmi les modèles Poisson, NB1, NB2, CMP et GP, en se basant sur le
#' critère d'information corrigé (AICc). La priorité est donnée aux modèles
#' bien ajustés (ajustement HNP < 10). Si aucun modèle ne satisfait ce critère,
#' la sélection est effectuée parmi tous les modèles disponibles.
#'
#' @param tablemodele Un `data.frame` retourné par
#'   `cpue_compare_modele(..., format = "data.frame")`, contenant au minimum
#'   les colonnes `Méthode`, `AICc` et `Ajustement (résultat du test HNP)`.
#'
#' @return Une chaîne de caractères correspondant à la méthode du meilleur modèle sélectionné.
#'   Retourne `NA` avec un avertissement si aucun modèle ne peut être sélectionné.
#'
#' @examples
#' df <- tibble::tibble(
#'   Méthode = c("poisson", "nb1", "nb2"),
#'   `Ajustement (résultat du test HNP)` = c(5, 12, 9),
#'   AICc = c(110, 105, 100)
#' )
#' cpue_select_best_modele(df)
#'
#' @export
#' @importFrom dplyr filter pull
cpue_select_best_modele <- function(tablemodele) {
  if (!"Méthode" %in% names(tablemodele) || !"AICc" %in% names(tablemodele)) {
    stop("Le tableau fourni n’est pas valide. Assurez-vous qu’il provient de `cpue_compare_modele()`.")
  }
  
  bien_ajuste <- tablemodele |>
    filter(`Ajustement (résultat du test HNP)` < 10)
  
  if (nrow(bien_ajuste) > 0) {
    best <- bien_ajuste |>
      filter(AICc == min(AICc, na.rm = TRUE)) |>
      pull(Méthode)
  } else {
    best <- tablemodele |>
      filter(AICc == min(AICc, na.rm = TRUE)) |>
      pull(Méthode)
  }
  
  if (length(best) == 0) {
    warning("Aucun modèle n’a pu être sélectionné.")
    return(NA)
  }
  
  return(best[1])
}
