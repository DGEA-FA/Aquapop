#' Sélectionner le meilleur modèle combiné (L50)
#'
#' Cette fonction identifie automatiquement le meilleur modèle combiné (sexes confondus)
#' parmi ceux qui ont convergé et obtenu un bon ajustement (aucun rejet dans le commentaire),
#' en se basant sur le plus faible AICc.
#'
#' @param evaluation_df Un `data.frame` retourné par `maturite_eval_modele()`
#'   appliqué aux modèles combinés. Il doit inclure au minimum les colonnes :
#'   `modele_id`, `convergence`, `commentaire`, `aicc`.
#'
#' @return Une liste contenant :
#' \describe{
#'   \item{best_model}{Identifiant du meilleur modèle combiné (`modele_id`), ou `NULL` si aucun modèle n'est valide}
#'   \item{message}{Message interprétatif indiquant la sélection ou l'absence de modèles valides}
#' }
#'
#' @examples
#' df <- tibble::tibble(
#'   modele_id = c("C_logit", "C_probit"),
#'   convergence = c(TRUE, TRUE),
#'   commentaire = c("Modèle valide.", "Modèle valide."),
#'   aicc = c(102.3, 99.8)
#' )
#' maturite_select_best_combined_modele(df)
#'
#' @export
#' @importFrom dplyr filter pull
maturite_select_best_combined_modele <- function(evaluation_df) {
  # --- Étape 1 : Filtrer les modèles valides (convergence + commentaire positif) ---
  valid_models <- evaluation_df |>
    filter(
      .data$convergence == TRUE,
      .data$ajust == TRUE
      #!grepl("rejeter|choisir un autre modèle", .data$commentaire)
    )
  
  # --- Étape 2 : Sélection du meilleur modèle ---
  best_model <- NULL
  
  if (nrow(valid_models) > 0) {
    best_model <- valid_models |>
    filter(.data$aicc == min(.data$aicc, na.rm = TRUE)) |>
    pull(.data$modele_id) |>
    head(1)
  }
  
  list(
    best_model = best_model
  )
}
