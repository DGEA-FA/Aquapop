#' Évaluer et comparer les modèles L50 (ou A50) de maturité
#'
#' Cette fonction ajuste, évalue et sélectionne les modèles de maturité sexuelle
#' selon l’approche séparée (par sexe) et combinée (sexes confondus), à partir
#' des données de spécimens. Elle retourne les tableaux d’évaluation au format brut
#' et flextable, un message d’interprétation, et le meilleur modèle recommandé
#' selon l’approche choisie ou forcée (`prefer_combined`).
#'
#' @param specimen_data Un `data.frame` contenant les données brutes de spécimens,
#'   incluant les colonnes `maturite`, `sexe`, et la variable quantitative choisie (`ltm` ou `age`).
#' @param prefer_combined Logique indiquant si l’approche combinée doit être forcée (défaut : `FALSE`)
#' @param variable Variable quantitative utilisée dans les modèles : `"ltm"` (par défaut) ou `"age"`
#'
#' @return Une liste contenant :
#' \describe{
#'   \item{table}{Liste avec `df` et `flextable` pour le tableau principal (selon l’approche retenue)}
#'   \item{best_model}{Liste contenant les 3 éléments : `best_model_M`, `best_model_F`, `best_model_combined`}
#'   \item{message}{Texte interprétatif décrivant la sélection et les cas d’échec éventuels}
#'   \item{table_sep}{Liste avec `df` et `flextable` pour les modèles séparés}
#'   \item{table_comb}{Liste avec `df` et `flextable` pour les modèles combinés}
#' }
#'
#' @export
#' @importFrom stringr str_extract
#' @importFrom labelled var_label
#' @importFrom flextable flextable
maturite_compare_modele <- function(specimen_data, prefer_combined = FALSE, variable = c("ltm", "age")) {
  variable <- match.arg(variable)
  
  # Fonction interne : convertir en liste (df + flextable)
  to_dual_format <- function(df) {
    list(
      df = df,
      flextable = df |> flextable() |> style_flextable_aquapop()
    )
  }
  
  # Fonction interne : appliquer des labels aux colonnes
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
    var_label(df) <- var_labels
    df
  }
  
  # Préparation des données
  df <- maturite_prepare(specimen_data, variable = variable)
  
  # --- Modèles séparés ---
  models_sep <- maturite_fit_separated_modele(df, variable = variable)
  eval_sep <- maturite_eval_modele(models_sep)
  best_sep <- maturite_select_best_separated_modele(eval_sep)
  eval_sep$type <- ifelse(grepl("^M_", eval_sep$modele_id), "séparé_M", "séparé_F")
  
  # --- Modèles combinés ---
  models_comb <- maturite_fit_combined_modele(df, variable = variable)
  eval_comb <- maturite_eval_modele(models_comb)
  eval_comb$type <- "combiné"
  best_comb <- maturite_select_best_combined_modele(eval_comb)
  
  # Marquage du modèle combiné recommandé
  eval_comb$recommande <- rep(FALSE, nrow(eval_comb))
  if (!is.null(best_comb$best_model) && length(best_comb$best_model) > 0) {
    eval_comb$recommande <- eval_comb$modele_id %in% best_comb$best_model
  }
  
  # Formatage des p-values
  eval_sep$pearson_x2_pval <- format_pval(eval_sep$pearson_x2_pval)
  eval_sep$goodness_of_link_pval <- format_pval(eval_sep$goodness_of_link_pval)
  eval_comb$pearson_x2_pval <- format_pval(eval_comb$pearson_x2_pval)
  eval_comb$goodness_of_link_pval <- format_pval(eval_comb$goodness_of_link_pval)
  
  # Message final
  message <- paste0(best_sep$message, "\n", best_comb$message)
  if (is.null(best_comb$best_model) &&
      (is.null(best_sep$best_model_M) || is.null(best_sep$best_model_F))) {
    message <- paste0(message, "\n⚠️ Aucun modèle utilisable n’a pu être sélectionné.")
    warning("Aucun modèle utilisable trouvé.")
  }
  
  # Application des labels
  eval_sep <- add_labels_maturite(eval_sep)
  eval_comb <- add_labels_maturite(eval_comb)
  
  # --- Sélection de l’approche à retenir ---
  if (prefer_combined || best_sep$use_combined) {
    table_main <- to_dual_format(eval_comb)
    best_model <- list(
      best_model_M = NULL,
      best_model_F = NULL,
      best_model_combined = NULL
    )
    if (!is.null(best_comb$best_model)) {
      best_model$best_model_combined <- list(
        modele = str_extract(best_comb$best_model, "TLO|ADD|INT|COM"),
        lien = str_extract(best_comb$best_model, "logit|probit|cloglog"),
        variable = variable
      )
    }
  } else {
    table_main <- to_dual_format(eval_sep)
    modele_ids_valides <- c(best_sep$best_model_M, best_sep$best_model_F) %||% character(0)
    eval_sep$recommande <- rep(FALSE, nrow(eval_sep))
    if (length(modele_ids_valides) > 0) {
      eval_sep$recommande <- eval_sep$modele_id %in% modele_ids_valides
    }
    
    best_model <- list(
      best_model_M = if (!is.null(best_sep$best_model_M)) {
        list(
          modele = str_extract(best_sep$best_model_M, "TLO|ADD|INT|COM"),
          lien = str_extract(best_sep$best_model_M, "logit|probit|cloglog"),
          variable = variable
        )
      } else NULL,
      best_model_F = if (!is.null(best_sep$best_model_F)) {
        list(
          modele = str_extract(best_sep$best_model_F, "TLO|ADD|INT|COM"),
          lien = str_extract(best_sep$best_model_F, "logit|probit|cloglog"),
          variable = variable
        )
      } else NULL,
      best_model_combined = NULL
    )
  }
  
  # Valeurs par défaut pour les colonnes "recommande"
  eval_sep$recommande <- eval_sep$recommande %||% FALSE
  eval_comb$recommande <- eval_comb$recommande %||% FALSE
  
  # --- Retour des résultats ---
  list(
    table = table_main,
    best_model = best_model,
    message = message,
    table_sep = to_dual_format(eval_sep),
    table_comb = to_dual_format(eval_comb)
  )
}
