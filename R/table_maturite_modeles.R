#' Évalue et sélectionne les modèles L50 (ou A50 si variable = "age")
#'
#' @param specimen_data Données brutes (spécimens)
#' @param prefer_combined Logique : forcer l'utilisation des modèles combinés ?
#' @param variable Variable quantitative à utiliser : `"ltm"` (par défaut) ou `"age"`
#'
#' @return Une liste contenant :
#' \itemize{
#'   \item `table` : liste avec `df` et `flextable` pour le tableau principal
#'   \item `best_model` : une liste (ou une liste de deux) avec `modele`, `lien`, `variable`
#'   \item `message` : texte explicatif
#'   \item `table_sep` : liste avec `df` et `flextable` pour les modèles séparés
#'   \item `table_comb` : liste avec `df` et `flextable` pour les modèles combinés
#' }
#'
#' @export
table_maturite_modeles <- function(specimen_data, prefer_combined = FALSE, variable = c("ltm", "age")) {
  variable <- match.arg(variable)
  
  # Fonction interne de conversion
  to_dual_format <- function(df) {
    list(
      df = df,
      flextable = df %>% flextable::flextable() %>% flextable::autofit()
    )
  }
  
  df <- prepare_maturite_data(specimen_data, variable = variable)
  
  models_sep <- fit_maturite_separated_models(df, variable = variable)
  eval_sep <- evaluate_maturite_modeles(models_sep)
  best_sep <- select_best_maturite_separated_modele(eval_sep)
  eval_sep$type <- ifelse(grepl("^M_", eval_sep$modele_id), "séparé_M", "séparé_F")
  
  models_comb <- fit_maturite_combined_models(df, variable = variable)
  eval_comb <- evaluate_maturite_modeles(models_comb)
  eval_comb$type <- "combiné"
  
  best_comb <- select_best_maturite_combined_modele(eval_comb)
  eval_comb$recommande <- eval_comb$modele_id == best_comb$best_model
  
  message <- paste0(best_sep$message, "\n", best_comb$message)
  
  if (is.null(best_comb$best_model) && (is.null(best_sep$best_model_M) || is.null(best_sep$best_model_F))) {
    message <- paste0(message, "\n⚠️ Aucun modèle utilisable n’a pu être sélectionné.")
    warning("Aucun modèle utilisable trouvé.")
  }
  
  if (prefer_combined || best_sep$use_combined) {
    table_main <- to_dual_format(eval_comb)
    best_model <- if (!is.null(best_comb$best_model)) {
      list(
        modele = stringr::str_extract(best_comb$best_model, "TLO|ADD|INT|COM"),
        lien = stringr::str_extract(best_comb$best_model, "logit|probit|cloglog"),
        variable = variable
      )
    } else {
      NULL
    }
  } else {
    best_model <- list(
      best_model_M = if (!is.null(best_sep$best_model_M)) {
        list(
          modele = stringr::str_extract(best_sep$best_model_M, "TLO|ADD|INT|COM"),
          lien = stringr::str_extract(best_sep$best_model_M, "logit|probit|cloglog"),
          variable = variable
        )
      } else NULL,
      best_model_F = if (!is.null(best_sep$best_model_F)) {
        list(
          modele = stringr::str_extract(best_sep$best_model_F, "TLO|ADD|INT|COM"),
          lien = stringr::str_extract(best_sep$best_model_F, "logit|probit|cloglog"),
          variable = variable
        )
      } else NULL
    )
    eval_sep$recommande <- eval_sep$modele_id %in% c(best_sep$best_model_M, best_sep$best_model_F)
    table_main <- to_dual_format(eval_sep)
  }
  
  eval_sep$recommande <- eval_sep$recommande %||% FALSE
  eval_comb$recommande <- eval_comb$recommande %||% FALSE
  
  list(
    table = table_main,
    best_model = best_model,
    message = message,
    table_sep = to_dual_format(eval_sep),
    table_comb = to_dual_format(eval_comb)
  )
}
