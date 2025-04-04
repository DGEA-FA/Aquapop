#' Ajoute une colonne 'recommande' à la table de modèles
#'
#' @param df Dataframe retourné par evaluate_L50_models()
#' @return Le même dataframe avec une colonne logique 'recommande'
#' @export
add_best_flag <- function(df) {
  best_id <- get_best_L50_model_id(df)
  df$recommande <- df$modele_id == best_id
  df
}
