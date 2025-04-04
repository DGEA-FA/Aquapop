#' Évalue et sélectionne les modèles L50 (séparés ou combinés)
#'
#' @param specimen_data Données brutes (spécimens)
#' @param prefer_combined Logique : forcer l'utilisation des modèles combinés ?
#' @param return_all Logique : retourner aussi les modèles séparés/combinés même s'ils ne sont pas retenus ?
#' @param format Format de sortie : `"df"` (par défaut) ou `"flextable"`
#'
#' @return Une liste contenant :
#' \itemize{
#'   \item `table` : le tableau principal (df ou flextable)
#'   \item `best_model` : identifiant(s) du/des meilleurs modèles (`character` vector ou NULL)
#'   \item `message` : texte explicatif
#'   \item `table_sep` (optionnel) : les modèles séparés
#'   \item `table_comb` (optionnel) : les modèles combinés
#' }
#'
#' @export
table_L50_modeles <- function(specimen_data, prefer_combined = FALSE, return_all = FALSE, format = c("df", "flextable")) {
  format <- match.arg(format)
  df <- prepare_maturite_l50_data(specimen_data)
  
  models_sep <- fit_L50_models(df)
  eval_sep <- evaluate_L50_models(models_sep)
  best_sep <- select_best_L50_models(eval_sep)
  eval_sep$type <- ifelse(grepl("^M_", eval_sep$modele_id), "séparé_M", "séparé_F")
  
  if (prefer_combined || best_sep$use_combined) {
    models_comb <- fit_L50_combined_models(df)
    eval_comb <- evaluate_L50_models(models_comb)
    eval_comb$type <- "combiné"
    
    best_comb <- select_best_L50_combined_model(eval_comb)
    eval_comb$recommande <- eval_comb$modele_id == best_comb$best_model
    
    message <- paste0(best_sep$message, "\n", best_comb$message)
    
    if (is.null(best_comb$best_model)) {
      message <- paste0(message, "\n⚠️ Aucun modèle utilisable n’a pu être sélectionné.")
      warning("Aucun modèle combiné utilisable trouvé.")
    }
    
    out <- list(
      table = eval_comb,
      best_model = best_comb$best_model,
      message = message
    )
    
    if (return_all) {
      eval_sep$recommande <- FALSE
      out$table_sep <- eval_sep
      out$table_comb <- eval_comb
    }
    
  } else {
    best_model_ids <- c(best_sep$best_model_M, best_sep$best_model_F)
    eval_sep$recommande <- eval_sep$modele_id %in% best_model_ids
    
    message <- best_sep$message
    
    if (length(best_model_ids) == 0 || all(is.na(best_model_ids))) {
      message <- paste0(message, "\n⚠️ Aucun modèle utilisable n’a pu être sélectionné.")
      warning("Aucun modèle séparé utilisable trouvé.")
    }
    
    out <- list(
      table = eval_sep,
      best_model = best_model_ids,
      message = message
    )
    
    if (return_all) {
      out$table_sep <- eval_sep
    }
  }
  
  # 🧼 Convertir au besoin en flextable
  if (format == "flextable") {
    to_flextable <- function(x) {
      x %>%
        flextable::flextable() %>%
        flextable::autofit()
    }
    out$table <- to_flextable(out$table)
    if (!is.null(out$table_sep)) out$table_sep <- to_flextable(out$table_sep)
    if (!is.null(out$table_comb)) out$table_comb <- to_flextable(out$table_comb)
  }
  
  out$message <- message
  return(out)
}
