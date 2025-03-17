#' Sélectionne les meilleurs modèles L50 pour chaque sexe
#'
#' @param evaluation_df Dataframe retourné par evaluate_L50_models()
#'
#' @return Une liste contenant les modèles sélectionnés pour M et F, 
#'         un message descriptif, et un indicateur booléen use_combined.
#' @export
select_best_L50_models <- function(evaluation_df) {
  library(dplyr)
  
  # Filtrer les modèles qui convergent et qui ont un bon ajustement
  valid_models <- evaluation_df %>%
    filter(convergence == TRUE, !grepl("rejeter|choisir un autre modèle", commentaire))
  
  # Séparer les modèles pour les mâles et les femelles selon la colonne 'modele_id'
  valid_M <- valid_models %>% filter(grepl("^M_", modele_id))
  valid_F <- valid_models %>% filter(grepl("^F_", modele_id))
  
  # Vérifier s'il y a au moins un modèle valide pour chaque sexe et générer le message approprié
  if (nrow(valid_M) == 0 & nrow(valid_F) == 0) {
    return(list(
      use_combined = TRUE,
      message = " Aucun modèle valide n'a convergé pour les mâles et les femelles en approche séparée. Testez une approche sexes combinés."
    ))
  } else if (nrow(valid_M) == 0) {
    return(list(
      use_combined = TRUE,
      message = "Aucun modèle valide n'a convergé pour les mâles. Testez une approche sexes combinés."
    ))
  } else if (nrow(valid_F) == 0) {
    return(list(
      use_combined = TRUE,
      message = "Aucun modèle valide n'a convergé pour les femelles. Testez une approche sexes combinés."
    ))
  } else {
    message_text <- "Les modèles sexes séparés ont été sélectionnés avec succès."
    # Sélectionner pour chaque sexe le modèle avec le plus bas AICc
    best_M <- valid_M %>% filter(aicc == min(aicc)) %>% pull(modele_id)
    best_F <- valid_F %>% filter(aicc == min(aicc)) %>% pull(modele_id)
    
    return(list(
      best_model_M = best_M,
      best_model_F = best_F,
      use_combined = FALSE,
      message = paste0(
        message_text, "\n",
        "Modèle sélectionné pour les mâles : ", best_M, "\n",
        "Modèle sélectionné pour les femelles : ", best_F
      )
    ))
  }
}
