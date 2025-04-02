#' Sélectionne le meilleur modèle de croissance basé sur le plus bas AICc
#'
#' Cette fonction identifie automatiquement le meilleur modèle (parmi Von Bertalanffy, Gompertz ou Logistique)
#' selon le critère d’AICc le plus faible.
#'
#' @param tablemodele Un `data.frame` retourné par `courbe_croissance_comparaison()`
#'
#' @return Une chaîne de caractères (nom du meilleur modèle) ou un message en cas d'erreur
#' @export
#'
#' @examples
#' mod <- courbe_croissance_comparaison(specimen)
#' select_best_croissance_model(mod)
select_best_croissance_model <- function(tablemodele) {
  if (!"methode" %in% names(tablemodele) || !"AICc" %in% names(tablemodele)) {
    stop("Le tableau de modèle n’est pas valide. Assurez-vous qu’il provient bien de `courbe_croissance_comparaison()`.")
  }
  
  best_row <- tablemodele |>
    dplyr::filter(AICc == min(AICc, na.rm = TRUE)) |>
    dplyr::pull(methode)
  
  if (length(best_row) == 0) {
    warning("Aucun modèle n’a pu être sélectionné.")
    return(NA)
  }
  
  return(best_row[1])  # Si égalité, retourne le premier
}
