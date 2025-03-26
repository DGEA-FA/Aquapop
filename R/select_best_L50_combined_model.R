#' Sélectionne le meilleur modèle parmi les modèles combinés
#'
#' @param evaluation_df Dataframe retourné par evaluate_L50_models()
#'
#' @return Une liste contenant le modèle sélectionné ou un message si aucun modèle n'est valide.
#' @export
select_best_L50_combined_model <- function(evaluation_df) {

  valid_models <- evaluation_df %>%
    filter(convergence == TRUE, !grepl("rejeter|choisir un autre modèle", commentaire))
  
  if (nrow(valid_models) == 0) {
    return(list(
      message = "Aucun modèle combiné valide n'a convergé. Vérifiez les données."
    ))
  }
  
  best_model <- valid_models %>% filter(aicc == min(aicc)) %>% pull(modele_id)
  
  return(list(
    best_model = best_model
  ))
}
