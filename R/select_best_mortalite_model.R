#' Sélectionner le meilleur modèle de mortalité selon le plus bas AICc
#'
#' Cette fonction identifie automatiquement le meilleur modèle de mortalité
#' parmi Poisson, NB1, NB2, CMP et GP, basé sur le plus bas AICc parmi
#' les modèles bien ajustés (ajustement HNP < 10). Si aucun modèle n’est
#' bien ajusté, elle sélectionne celui avec le plus bas AICc global.
#'
#' @param tablemodele Un `data.frame` retourné par `mortalite_modele_comparaison(format = "data.frame")`
#'
#' @return Une chaîne de caractères (`Méthode`) correspondant au meilleur modèle.
#' @export
#'
#' @examples
#' tableau <- mortalite_modele_comparaison(df_age_etendue, format = "data.frame")
#' select_best_mortalite_model(tableau)
select_best_mortalite_model <- function(tablemodele) {
  if (!"Méthode" %in% names(tablemodele) || !"AICc" %in% names(tablemodele)) {
    stop("Le tableau fourni n’est pas valide. Assurez-vous qu’il provient de `mortalite_modele_comparaison()`.")
  }
  
  bien_ajuste <- tablemodele |>
    dplyr::filter(`Ajustement HNP (%)` < 10)
  
  if (nrow(bien_ajuste) > 0) {
    best <- bien_ajuste |>
      dplyr::filter(AICc == min(AICc, na.rm = TRUE)) |>
      dplyr::pull(Méthode)
  } else {
    best <- tablemodele |>
      dplyr::filter(AICc == min(AICc, na.rm = TRUE)) |>
      dplyr::pull(Méthode)
  }
  
  if (length(best) == 0) {
    warning("Aucun modèle n’a pu être sélectionné.")
    return(NA)
  }
  
  return(best[1])
}
