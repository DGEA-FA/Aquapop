#' Sélectionner les meilleurs modèles L50 pour chaque sexe (approche séparée)
#'
#' Cette fonction identifie les meilleurs modèles L50 pour les mâles et les femelles
#' à partir d'un tableau d'évaluation. Elle retient uniquement les modèles ayant
#' convergé et un commentaire favorable (pas de rejet), puis sélectionne celui
#' avec le plus bas AICc pour chaque sexe.
#'
#' @param evaluation_df Un `data.frame` retourné par `maturite_eval_modele()`, contenant
#'   au minimum les colonnes suivantes : `modele_id`, `convergence`, `commentaire`, `aicc`.
#'
#' @return Une liste contenant :
#' \describe{
#'   \item{best_model_M}{Identifiant du meilleur modèle pour les mâles, ou `NULL`}
#'   \item{best_model_F}{Identifiant du meilleur modèle pour les femelles, ou `NULL`}
#' }
#'
#' @export
#' @importFrom dplyr filter pull
maturite_select_best_separated_modele <- function(evaluation_df) {
  # --- Étape 1 : Filtrer les modèles valides ---
  valid_models <- evaluation_df |>
    filter(
      .data$convergence == TRUE,
      .data$ajust == TRUE
      #!grepl("rejeter|choisir un autre modèle", .data$commentaire)
    )
  
  # --- Étape 2 : Séparer les modèles par sexe ---
  valid_M <- filter(valid_models, grepl("^M_", .data$modele_id))
  valid_F <- filter(valid_models, grepl("^F_", .data$modele_id))

  # --- Étape 4 : Sélection du meilleur modèle pour chaque sexe ---
  
  best_M <- NULL
  
  if (nrow(valid_M) > 0) {
    best_M <- valid_M |>
      filter(.data$aicc == min(.data$aicc, na.rm = TRUE)) |>
      pull(.data$modele_id) |>
      head(1)
  }

  best_F <- NULL
  
  if (nrow(valid_F) > 0) {
    best_F <- valid_F |>
    filter(.data$aicc == min(.data$aicc, na.rm = TRUE)) |>
    pull(.data$modele_id) |>
    head(1)
  }
  
  list(
    best_model_M = best_M,
    best_model_F = best_F
    )
}
