#' Évaluer et comparer les modèles L50 (ou A50) de maturité
#'
#' Cette fonction ajuste, évalue et sélectionne les modèles de maturité sexuelle
#' selon l'approche séparée (par sexe) et combinée (sexes confondus), à partir
#' des données de spécimens. Elle retourne les tableaux d'évaluation au format brut
#' et flextable, un message d'interprétation, ainsi que les meilleurs modèles
#' disponibles pour les mâles, les femelles et l'approche combinée.
#'
#' En cas de jeu de données vide ou insuffisant, la fonction retourne un objet
#' structuré avec `success = FALSE`, sans générer d'erreur.
#'
#' @param specimen_data Un `data.frame` contenant les données brutes de spécimens,
#'   incluant les colonnes `maturite`, `sexe`, et la variable quantitative choisie
#'   (`ltm` ou `age`).
#' @param prefer_combined Logique indiquant si l'approche combinée doit être forcée
#'   pour le tableau principal (défaut : `FALSE`).
#' @param variable Variable quantitative utilisée dans les modèles : `"ltm"`
#'   (par défaut) ou `"age"`.
#'
#' @return Une liste contenant :
#' \describe{
#'   \item{success}{Indique si la comparaison des modèles a pu être effectuée}
#'   \item{table}{Liste avec `df` et `flextable` pour le tableau principal}
#'   \item{best_model}{Liste contenant `best_model_M`, `best_model_F` et
#'   `best_model_combined`}
#'   \item{message}{Texte interprétatif décrivant la sélection ou les cas d'échec}
#'   \item{table_sep}{Liste avec `df` et `flextable` pour les modèles séparés}
#'   \item{table_comb}{Liste avec `df` et `flextable` pour les modèles combinés}
#' }
#'
#' @export
#'
#' @importFrom flextable flextable
#' @importFrom labelled var_label
#' @importFrom stringr str_extract
#' @importFrom tibble tibble
maturite_compare_modele <- function(specimen_data,
                                    prefer_combined = FALSE,
                                    variable = c("ltm", "age")) {
  variable <- match.arg(variable)
  
  # ==== Fonctions internes ----
  
  to_dual_format <- function(df) {
    list(
      df = df,
      flextable = df |>
        flextable() |>
        style_flextable_aquapop()
    )
  }
  
  add_labels_maturite <- function(df) {
    var_labels <- list()
    
    if ("modele_id" %in% names(df)) {
      var_labels$modele_id <- "Modèle"
    }
    if ("modele" %in% names(df)) {
      var_labels$modele <- "Type"
    }
    if ("lien" %in% names(df)) {
      var_labels$lien <- "Lien"
    }
    if ("convergence" %in% names(df)) {
      var_labels$convergence <- "Convergence"
    }
    if ("aicc" %in% names(df)) {
      var_labels$aicc <- "AICc"
    }
    if ("pearson_x2_pval" %in% names(df)) {
      var_labels$pearson_x2_pval <- "p (χ² de Pearson)"
    }
    if ("goodness_of_link_pval" %in% names(df)) {
      var_labels$goodness_of_link_pval <- "p (test du lien)"
    }
    if ("commentaire" %in% names(df)) {
      var_labels$commentaire <- "Commentaire"
    }
    if ("type" %in% names(df)) {
      var_labels$type <- "Type de modèle"
    }
    if ("recommande" %in% names(df)) {
      var_labels$recommande <- "✔ Recommandé"
    }
    
    var_label(df) <- var_labels
    df
  }
  
  empty_eval <- tibble(
    modele_id = character(),
    modele = character(),
    lien = character(),
    convergence = logical(),
    pearson_x2_pval = character(),
    goodness_of_link_pval = character(),
    aicc = numeric(),
    commentaire = character(),
    type = character(),
    recommande = logical()
  )
  
  # ==== Validation des données ----
  
  validation_res <- maturite_validate_data(
    specimen_data = specimen_data,
    variable = variable,
    min_n = 10
  )
  
  if (!isTRUE(validation_res$success)) {
    empty_dual <- to_dual_format(add_labels_maturite(empty_eval))
    
    return(list(
      success = FALSE,
      table = empty_dual,
      best_model = list(
        best_model_M = NULL,
        best_model_F = NULL,
        best_model_combined = NULL
      ),
      message = validation_res$message,
      table_sep = empty_dual,
      table_comb = empty_dual
    ))
  }
  
  df <- validation_res$data
  
  # ==== Modèles séparés ----
  
  models_sep <- maturite_fit_separated_modele(df, variable = variable)
  eval_sep <- maturite_eval_modele(models_sep)
  best_sep <- maturite_select_best_separated_modele(eval_sep)
  
  eval_sep$type <- ifelse(
    grepl("^M_", eval_sep$modele_id),
    "séparé_M",
    "séparé_F"
  )
  
  # ==== Modèles combinés ----
  
  models_comb <- maturite_fit_combined_modele(df, variable = variable)
  eval_comb <- maturite_eval_modele(models_comb)
  best_comb <- maturite_select_best_combined_modele(eval_comb)
  
  eval_comb$type <- "combiné"
  eval_comb$recommande <- rep(FALSE, nrow(eval_comb))
  
  if (!is.null(best_comb$best_model) && length(best_comb$best_model) > 0) {
    eval_comb$recommande <- eval_comb$modele_id %in% best_comb$best_model
  }
  
  # ==== Formatage des p-valeurs ----
  
  eval_sep$pearson_x2_pval <- format_pval(eval_sep$pearson_x2_pval)
  eval_sep$goodness_of_link_pval <- format_pval(eval_sep$goodness_of_link_pval)
  eval_comb$pearson_x2_pval <- format_pval(eval_comb$pearson_x2_pval)
  eval_comb$goodness_of_link_pval <- format_pval(eval_comb$goodness_of_link_pval)
  
  # ==== Message final ----
  
  message <- paste0(best_sep$message, "\n", best_comb$message)
  
  if (is.null(best_comb$best_model) &&
      is.null(best_sep$best_model_M) &&
      is.null(best_sep$best_model_F)) {
    message <- paste0(
      message,
      "\nAucun modèle utilisable n'a pu être sélectionné."
    )
  }
  
  # ==== Labels ----
  
  eval_sep <- add_labels_maturite(eval_sep)
  eval_comb <- add_labels_maturite(eval_comb)
  
  # ==== Meilleurs modèles disponibles ----
  
  best_model <- list(
    best_model_M = if (!is.null(best_sep$best_model_M)) {
      list(
        modele = "TLO",
        lien = str_extract(best_sep$best_model_M, "logit|probit|cloglog"),
        variable = variable
      )
    } else {
      NULL
    },
    best_model_F = if (!is.null(best_sep$best_model_F)) {
      list(
        modele = "TLO",
        lien = str_extract(best_sep$best_model_F, "logit|probit|cloglog"),
        variable = variable
      )
    } else {
      NULL
    },
    best_model_combined = if (!is.null(best_comb$best_model)) {
      list(
        modele = str_extract(best_comb$best_model, "TLO|ADD|INT|COM"),
        lien = str_extract(best_comb$best_model, "logit|probit|cloglog"),
        variable = variable
      )
    } else {
      NULL
    }
  )
  
  # ==== Tableau principal ----
  
  if (prefer_combined || isTRUE(best_sep$use_combined)) {
    table_main <- to_dual_format(eval_comb)
  } else {
    modele_ids_valides <- c(best_sep$best_model_M, best_sep$best_model_F)
    
    eval_sep$recommande <- rep(FALSE, nrow(eval_sep))
    
    if (length(modele_ids_valides) > 0) {
      eval_sep$recommande <- eval_sep$modele_id %in% modele_ids_valides
    }
    
    table_main <- to_dual_format(eval_sep)
  }
  
  # ==== Valeurs par défaut sûres ----
  
  if (!"recommande" %in% names(eval_sep)) {
    eval_sep$recommande <- FALSE
  }
  
  if (!"recommande" %in% names(eval_comb)) {
    eval_comb$recommande <- FALSE
  }
  
  # ==== Retour ----
  
  list(
    success = TRUE,
    table = table_main,
    best_model = best_model,
    message = message,
    table_sep = to_dual_format(eval_sep),
    table_comb = to_dual_format(eval_comb)
  )
}