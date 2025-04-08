#' Sélectionne le meilleur modèle parmi les modèles combinés
#'
#' À partir du tableau d’évaluation, cette fonction sélectionne le modèle combiné
#' ayant le plus bas AICc parmi ceux qui ont convergé et qui s'ajustent bien.
#'
#' @param evaluation_df Un data.frame retourné par `evaluate_L50_models()` appliqué aux modèles combinés.
#'
#' @return Une liste contenant :
#' \itemize{
#'   \item `best_model` : identifiant du meilleur modèle (ou `NULL` si aucun n'est valide)
#'   \item `message` : texte descriptif à afficher
#' }
#'
#' @export
select_best_maturite_combined_modele <- function(evaluation_df) {
  valid_models <- evaluation_df %>%
    dplyr::filter(
      convergence == TRUE,
      !grepl("rejeter|choisir un autre modèle", commentaire)
    )
  
  if (nrow(valid_models) == 0) {
    return(list(
      best_model = NULL,
      message = "Aucun modèle combiné valide n'a convergé. Vérifiez les données."
    ))
  }
  
  best_model <- valid_models %>%
    dplyr::filter(aicc == min(aicc)) %>%
    dplyr::pull(modele_id)
  
  return(list(
    best_model = best_model,
    message = paste0("Modèle combiné sélectionné : ", best_model)
  ))
}
