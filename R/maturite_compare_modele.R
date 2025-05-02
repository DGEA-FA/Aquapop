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
maturite_compare_modele <- function(specimen_data, prefer_combined = FALSE, variable = c("ltm", "age")) {
  variable <- match.arg(variable)
  
  # Fonction interne de conversion
  to_dual_format <- function(df) {
    list(
      df = df,
      flextable = df |> flextable::flextable() |> style_flextable_aquapop()
    )
  }
  
  # Fonction interne pour ajouter les labels aux colonnes de modèles de maturité
  add_labels_maturite <- function(df) {
    var_labels <- list()
    if ("modele_id" %in% names(df)) var_labels$modele_id <- "Modèle"
    if ("modele" %in% names(df))    var_labels$modele    <- "Type"
    if ("lien" %in% names(df))      var_labels$lien      <- "Lien"
    if ("convergence" %in% names(df)) var_labels$convergence <- "Convergence"
    if ("aicc" %in% names(df))      var_labels$aicc      <- "AICc"
    if ("pearson_x2_pval" %in% names(df)) var_labels$pearson_x2_pval <- "p (χ² de Pearson)"
    if ("goodness_of_link_pval" %in% names(df)) var_labels$goodness_of_link_pval <- "p (test du lien)"
    if ("commentaire" %in% names(df)) var_labels$commentaire <- "Commentaire"
    if ("type" %in% names(df))      var_labels$type      <- "Type de modèle"
    if ("recommande" %in% names(df)) var_labels$recommande <- "✔ Recommandé"
    
    labelled::var_label(df) <- var_labels
    return(df)
  }
  
  # Préparation des données
  df <- maturite_prepare(specimen_data, variable = variable)
  
  # Ajustement des modèles séparés
  models_sep <- maturite_fit_separated_modele(df, variable = variable)
  eval_sep <- maturite_eval_modele(models_sep)
  best_sep <- maturite_select_best_separated_modele(eval_sep)
  eval_sep$type <- ifelse(grepl("^M_", eval_sep$modele_id), "séparé_M", "séparé_F")
  
  # Ajustement des modèles combinés
  models_comb <- maturite_fit_combined_modele(df, variable = variable)
  eval_comb <- maturite_eval_modele(models_comb)
  eval_comb$type <- "combiné"
  
  best_comb <- maturite_select_best_combined_modele(eval_comb)
  eval_comb$recommande <- eval_comb$modele_id == best_comb$best_model
  
  # Formatage des p-values
  eval_sep$pearson_x2_pval <- format_pval(eval_sep$pearson_x2_pval)
  eval_sep$goodness_of_link_pval <- format_pval(eval_sep$goodness_of_link_pval)
  eval_comb$pearson_x2_pval <- format_pval(eval_comb$pearson_x2_pval)
  eval_comb$goodness_of_link_pval <- format_pval(eval_comb$goodness_of_link_pval)
  
  # Message explicatif
  message <- paste0(best_sep$message, "\n", best_comb$message)
  if (is.null(best_comb$best_model) && (is.null(best_sep$best_model_M) || is.null(best_sep$best_model_F))) {
    message <- paste0(message, "\n⚠️ Aucun modèle utilisable n’a pu être sélectionné.")
    warning("Aucun modèle utilisable trouvé.")
  }
  
  # Ajouter les labels
  eval_sep <- add_labels_maturite(eval_sep)
  eval_comb <- add_labels_maturite(eval_comb)
  
  # Structure des sorties
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
  
  # Valeur par défaut pour la colonne `recommande` si elle est absente
  eval_sep$recommande <- eval_sep$recommande %||% FALSE
  eval_comb$recommande <- eval_comb$recommande %||% FALSE
  
  # Résultat final
  list(
    table = table_main,
    best_model = best_model,
    message = message,
    table_sep = to_dual_format(eval_sep),
    table_comb = to_dual_format(eval_comb)
  )
}
