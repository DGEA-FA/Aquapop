#' Sélectionner le meilleur modèle de mortalité selon le plus bas AICc
#'
#' Cette fonction identifie automatiquement le meilleur modèle de mortalité
#' parmi Poisson, NB1, NB2, CMP et GP, basé sur le plus bas AICc parmi
#' les modèles bien ajustés (ajustement HNP < 10). Si aucun modèle n’est
#' bien ajusté, elle sélectionne celui avec le plus bas AICc global.
#'
#' @param tablemodele Un `data.frame` retourné par `mortalite_compare_modele()$data`
#'
#' @return Une chaîne de caractères (`Méthode`) correspondant au meilleur modèle.
#' @export
#'
#' @examples
#' mortalite_compare_modele_res_data <- mortalite_compare_modele(data = df_age_etendue)$data
#' mortalite_select_best_modele(mortalite_compare_modele_res_data)
mortalite_select_best_modele <- function(tablemodele) {
  if (!"Méthode" %in% names(tablemodele) || !"AICc" %in% names(tablemodele)) {
    stop("Le tableau fourni n’est pas valide. Assurez-vous qu’il provient de `mortalite_compare_modele()$data`.")
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
