#' Trouve l'index du meilleur modèle dans une table
#'
#' @param table_df Table issue de `table_L50_modeles()`
#'
#' @return Un entier (ligne du meilleur modèle)
#' @export
select_best_L50_model_id <- function(table_df) {
  valid <- table_df %>%
    dplyr::filter(
      convergence == TRUE,
      !grepl("rejeter|choisir un autre modèle", commentaire)
    )
  best <- valid %>%
    dplyr::filter(aicc == min(aicc)) %>%
    dplyr::slice(1)  # en cas d'ex aequo
  match(best$modele_id, table_df$modele_id)
}
