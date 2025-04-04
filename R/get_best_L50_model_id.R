#' Identifie le modèle L50 recommandé dans une table
#'
#' @param table_df Table de modèles L50
#' @return Le nom du modèle (modele_id) avec le plus petit AICc parmi les valides
#' @export
get_best_L50_model_id <- function(table_df) {
  valid <- table_df %>%
    dplyr::filter(
      convergence == TRUE,
      !grepl("rejeter|choisir un autre modèle", commentaire)
    )
  if (nrow(valid) == 0) return(NA_character_)
  valid %>%
    dplyr::filter(aicc == min(aicc)) %>%
    dplyr::pull(modele_id) %>%
    .[1]
}
