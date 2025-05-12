#' Sélectionner le meilleur modèle de mortalité selon AICc et ajustement HNP
#'
#' Cette fonction identifie automatiquement le meilleur modèle parmi ceux comparés (Poisson, NB1, NB2, CMP, GP),
#' en priorisant les modèles avec un bon ajustement HNP (inférieur à 10 %). Si aucun modèle ne satisfait ce critère,
#' le modèle avec le plus faible AICc est sélectionné.
#'
#'
#' @return Une chaîne de caractères correspondant au nom du meilleur modèle, ou `NA` si aucun modèle n’est sélectionnable.
#' @export
#'
#' @importFrom dplyr filter pull
#'
#' @examples
#' # Exemple minimal
#' df <- data.frame(
#'   Méthode = c("Poisson", "NB1", "NB2", "CMP", "GP"),
#'   AICc = c(112, 108, 109, 107, 106),
#'   "Ajustement HNP (%)" = c(15, 12, 8, 6, 11)
#' )
#' mortalite_select_best_modele(df)
mortalite_select_best_modele <- function(tablemodele) {
  # Validation ----
  if (!all(c("Méthode", "AICc", "Ajustement HNP (%)") %in% names(tablemodele))) {
    stop("Le tableau fourni n’est pas valide. Il doit contenir les colonnes : 'Méthode', 'AICc' et 'Ajustement HNP (%)'.")
  }
  
  # Filtrage des modèles bien ajustés (HNP < 10) ----
  modeles_bien_ajustes <- tablemodele |>
    filter(`Ajustement HNP (%)` < 10)
  
  # Sélection finale selon le plus faible AICc ----
  if (nrow(modeles_bien_ajustes) > 0) {
    selection <- modeles_bien_ajustes |>
      filter(AICc == min(AICc, na.rm = TRUE)) |>
      pull(Méthode)
  } else {
    selection <- tablemodele |>
      filter(AICc == min(AICc, na.rm = TRUE)) |>
      pull(Méthode)
  }
  
  # Retourner la sélection (ou NA) ----
  if (length(selection) == 0 || is.na(selection[1])) {
    warning("Aucun modèle n’a pu être sélectionné.")
    return(NA)
  }
  
  return(selection[1])
}
