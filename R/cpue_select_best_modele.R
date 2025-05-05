#' Sélectionner le meilleur modèle CPUE selon le plus bas AICc
#'
#' Cette fonction identifie automatiquement le meilleur modèle d’abondance (CPUE)
#' parmi les modèles Poisson, NB1, NB2, CMP et GP. La sélection est effectuée selon
#' le plus bas AICc parmi les modèles bien ajustés (ajustement HNP < 10).
#' Si aucun modèle ne satisfait ce critère, la fonction retourne celui avec le plus bas AICc global.
#'
#' @param tablemodele Un `data.frame` retourné par `modele_cpue_comparaison(..., format = "data.frame")`.
#'
#' @return Une chaîne de caractères (`Méthode`) correspondant au nom du meilleur modèle sélectionné.
#'
#' @importFrom dplyr filter pull
#'
#' @export
cpue_select_best_modele <- function(tablemodele) {
  if (!"Méthode" %in% names(tablemodele) || !"AICc" %in% names(tablemodele)) {
    stop("Le tableau fourni n’est pas valide. Assurez-vous qu’il provient de `modele_cpue_comparaison()`.")
  }
  
  # Priorité aux modèles bien ajustés
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
