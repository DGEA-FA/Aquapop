#' Sélectionne les meilleurs modèles L50 pour chaque sexe
#'
#' À partir du tableau d’évaluation, cette fonction sélectionne les modèles L50 qui convergent
#' et qui ont un bon ajustement pour les mâles et les femelles, en choisissant celui avec le plus bas AICc.
#' Si aucun modèle n’est valide pour un des deux sexes, elle recommande de passer à l’approche combinée.
#'
#' @importFrom dplyr pull
#' @importFrom dplyr filter
#' @param evaluation_df Un data.frame retourné par `evaluate_L50_models()`.
#'                      Il doit inclure les colonnes `modele_id`, `convergence`, `commentaire`, et `aicc`.
#'
#' @return Une liste contenant :
#' - `best_model_M`, `best_model_F` : identifiants des meilleurs modèles par sexe (si applicables)
#' - `use_combined` : booléen indiquant si une approche combinée est requise
#' - `message` : texte décrivant la décision
#'
#' @export
maturite_select_best_separated_modele <- function(evaluation_df) {
  # Filtrer les modèles valides : convergence et commentaire favorable
  valid_models <- evaluation_df |>
    filter(
      convergence == TRUE,
      !grepl("rejeter|choisir un autre modèle", commentaire)
    )
  
  # Séparer les modèles par sexe
  valid_M <- filter(valid_models, grepl("^M_", modele_id))
  valid_F <- filter(valid_models, grepl("^F_", modele_id))
  
  # Cas 1 — Aucun modèle valide
  if (nrow(valid_M) == 0 && nrow(valid_F) == 0) {
    return(list(
      use_combined = TRUE,
      message = "Aucun modèle valide n'a convergé pour les mâles et les femelles en approche séparée. Testez une approche sexes combinés."
    ))
  }
  
  # Cas 2 — Mâles absents
  if (nrow(valid_M) == 0) {
    return(list(
      use_combined = TRUE,
      message = "Aucun modèle valide n'a convergé pour les mâles. Testez une approche sexes combinés."
    ))
  }
  
  # Cas 3 — Femelles absentes
  if (nrow(valid_F) == 0) {
    return(list(
      use_combined = TRUE,
      message = "Aucun modèle valide n'a convergé pour les femelles. Testez une approche sexes combinés."
    ))
  }
  
  # Cas 4 — Les deux sexes ont au moins un modèle valide
  best_M <- valid_M |>
    filter(aicc == min(aicc)) |>
    pull(modele_id)  |>
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
