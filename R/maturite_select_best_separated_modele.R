#' Sélectionner les meilleurs modèles L50 pour chaque sexe (approche séparée)
#'
#' Cette fonction identifie les meilleurs modèles L50 pour les mâles et les femelles
#' à partir d'un tableau d'évaluation. Elle retient uniquement les modèles ayant
#' convergé et un commentaire favorable (pas de rejet), puis sélectionne celui
#' avec le plus bas AICc pour chaque sexe. Si un des deux sexes ne dispose
#' d'aucun modèle valide, elle recommande de passer à une approche combinée.
#'
#' @param evaluation_df Un `data.frame` retourné par `maturite_eval_modele()`, contenant
#'   au minimum les colonnes suivantes : `modele_id`, `convergence`, `commentaire`, `aicc`.
#'
#' @return Une liste contenant :
#' \describe{
#'   \item{best_model_M}{Identifiant du meilleur modèle pour les mâles, ou `NULL`}
#'   \item{best_model_F}{Identifiant du meilleur modèle pour les femelles, ou `NULL`}
#'   \item{use_combined}{Booléen indiquant si une approche combinée est recommandée}
#'   \item{message}{Texte interprétatif décrivant la décision}
#' }
#'
#' @export
#' @importFrom dplyr filter pull
maturite_select_best_separated_modele <- function(evaluation_df) {
  # --- Étape 1 : Filtrer les modèles valides ---
  valid_models <- evaluation_df |>
    filter(
      convergence == TRUE,
      !grepl("rejeter|choisir un autre modèle", commentaire)
    )
  
  # --- Étape 2 : Séparer les modèles par sexe ---
  valid_M <- filter(valid_models, grepl("^M_", modele_id))
  valid_F <- filter(valid_models, grepl("^F_", modele_id))
  
  # --- Étape 3 : Aucun modèle valide du tout ---
  if (nrow(valid_M) == 0 && nrow(valid_F) == 0) {
    return(list(
      use_combined = TRUE,
      message = "Aucun modèle valide n'a convergé pour les mâles et les femelles en approche séparée. Testez une approche sexes combinés."
    ))
  }
  
  # --- Étape 4 : Aucun modèle valide pour les mâles ---
  if (nrow(valid_M) == 0) {
    return(list(
      use_combined = TRUE,
      message = "Aucun modèle valide n'a convergé pour les mâles. Testez une approche sexes combinés."
    ))
  }
  
  # --- Étape 5 : Aucun modèle valide pour les femelles ---
  if (nrow(valid_F) == 0) {
    return(list(
      use_combined = TRUE,
      message = "Aucun modèle valide n'a convergé pour les femelles. Testez une approche sexes combinés."
    ))
  }
  
  # --- Étape 6 : Modèles valides pour les deux sexes ---
  best_M <- valid_M |>
    filter(aicc == min(aicc)) |>
    pull(modele_id) |>
    head(1)
  
  best_F <- valid_F |>
    filter(aicc == min(aicc)) |>
    pull(modele_id) |>
    head(1)
  
  return(list(
    best_model_M = best_M,
    best_model_F = best_F,
    use_combined = FALSE,
    message = paste0(
      "Les modèles sexes séparés ont été sélectionnés avec succès.\n",
      "Modèle sélectionné pour les mâles : ", best_M, "\n",
      "Modèle sélectionné pour les femelles : ", best_F
    )
  ))
}
