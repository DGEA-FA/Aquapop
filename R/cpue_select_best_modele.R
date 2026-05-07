#' Sélectionner le meilleur modèle CPUE selon le plus bas AICc
#'
#' Cette fonction identifie automatiquement le meilleur modèle d'abondance
#' (CPUE) parmi les modèles Poisson, NB1, NB2, CMP et GP, en se basant sur le
#' critère d'information corrigé (AICc). La priorité est donnée aux modèles
#' bien ajustés (ajustement HNP < 10). Si aucun modèle ne satisfait ce critère,
#' la sélection est effectuée parmi tous les modèles disponibles.
#'
#' @param tablemodele Un `data.frame` retourné par
#'   `cpue_compare_modele(..., format = "data.frame")`, contenant au minimum
#'   les colonnes `methode`, `aicc` et `ajustement_hnp`.
#'
#' @return Une chaîne de caractères correspondant à la méthode du meilleur modèle sélectionné.
#'   Retourne `NA` avec un avertissement si aucun modèle ne peut être sélectionné.
#'
#' @examples
#' df <- tibble::tibble(
#'   methode = c("poisson", "nb1", "nb2"),
#'   ajustement_hnp = c(5, 12, 9),
#'   aicc = c(110, 105, 100)
#' )
#' cpue_select_best_modele(df)
#'
#' @export
#' @importFrom dplyr filter pull
#' @importFrom rlang .data
cpue_select_best_modele <- function(tablemodele) {
  if (!"methode" %in% names(tablemodele) || !"aicc" %in% names(tablemodele)) {
    stop("Le tableau fourni n'est pas valide. Assurez-vous qu’il provient de `cpue_compare_modele()`.")
  }
  
  bien_ajuste <- tablemodele |>
    filter(.data$ajustement_hnp < 10)
  
  if (nrow(bien_ajuste) > 0) {
    best <- bien_ajuste |>
      filter(.data$aicc == min(.data$aicc, na.rm = TRUE)) |>
      pull(.data$methode)
  } else {
    best <- tablemodele |>
      filter(.data$aicc == min(.data$aicc, na.rm = TRUE)) |>
      pull(.data$methode)
  }
  
  if (length(best) == 0) {
    warning("Aucun modèle n’a pu être sélectionné.")
    return(NA_character_)
  }
  
  return(best[1])
}
