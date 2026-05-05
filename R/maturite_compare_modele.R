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
#' @importFrom dplyr mutate select if_else
#' @importFrom flextable flextable set_header_labels
#' @importFrom stringr str_extract
#' @importFrom tibble tibble
maturite_compare_modele <- function(specimen_data,
                                    prefer_combined = FALSE,
                                    variable = c("ltm", "age")) {
  variable <- match.arg(variable)
  
  # ==== Tableau vide par défaut ----
  
  empty_eval <- tibble(
    modele_id = character(),
    modele = character(),
    lien = character(),
    convergence = character(),
    pearson_x2_pval = character(),
    goodness_of_link_pval = character(),
    aicc = character(),
    commentaire = character(),
    type = character(),
    recommande = character()
  )
  
  empty_ft <- flextable(empty_eval) |>
    set_header_labels(
      modele_id = "Modèle",
      modele = "Type",
      lien = "Lien",
      convergence = "Convergence",
      pearson_x2_pval = "p (χ² de Pearson)",
      goodness_of_link_pval = "p (test du lien)",
      aicc = "AICc",
      commentaire = "Commentaire",
      type = "Type de modèle",
      recommande = "✔ Recommandé"
    ) |>
    style_flextable_aquapop()
  
  empty_dual <- list(
    df = empty_eval,
    flextable = empty_ft
  )
  
  # ==== Validation des données ----
  
  validation_res <- maturite_validate_data(
    specimen_data = specimen_data,
    variable = variable
  )
  
  if (!isTRUE(validation_res$success)) {
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
  
  # ==== Valeurs par défaut sûres ----
  
  if (!"recommande" %in% names(eval_sep)) {
    eval_sep$recommande <- FALSE
  }
  
  if (!"recommande" %in% names(eval_comb)) {
    eval_comb$recommande <- FALSE
  }
  
  # ==== Détermination des modèles recommandés séparés ----
  
  modele_ids_valides_sep <- c(best_sep$best_model_M, best_sep$best_model_F)
  
  eval_sep$recommande <- rep(FALSE, nrow(eval_sep))
  
  if (length(modele_ids_valides_sep) > 0) {
    eval_sep$recommande <- eval_sep$modele_id %in% modele_ids_valides_sep
  }
  
  # ==== Tableau affichage séparé ----
  
  eval_sep_affichage <- eval_sep |>
    mutate(
      convergence = if_else(.data$convergence, "Convergé", "Non convergé"),
      aicc = if_else(is.na(.data$aicc), "-", as.character(round(.data$aicc, 2))),
      recommande = if_else(.data$recommande, "✔", "")
    ) |>
    select(
      "modele_id",
      "lien",
      "convergence",
      "pearson_x2_pval",
      "goodness_of_link_pval",
      "aicc",
      "commentaire",
      "type",
      "recommande"
    )
  
  ft_sep <- flextable(eval_sep_affichage) |>
    set_header_labels(
      modele_id = "Modèle",
      lien = "Lien",
      convergence = "Convergence",
      pearson_x2_pval = "p (χ² de Pearson)",
      goodness_of_link_pval = "p (test du lien)",
      aicc = "AICc",
      commentaire = "Commentaire",
      type = "Type de modèle",
      recommande = "✔ Recommandé"
    ) |>
    style_flextable_aquapop()
  
  table_sep <- list(
    df = eval_sep_affichage,
    flextable = ft_sep
  )
  
  # ==== Tableau affichage combiné ----
  
  eval_comb_affichage <- eval_comb |>
    mutate(
      convergence = if_else(.data$convergence, "Convergé", "Non convergé"),
      aicc = if_else(is.na(.data$aicc), "-", as.character(round(.data$aicc, 2))),
      recommande = if_else(.data$recommande, "✔", "")
    ) |>
    select(
      "modele_id",
      "modele",
      "lien",
      "convergence",
      "pearson_x2_pval",
      "goodness_of_link_pval",
      "aicc",
      "commentaire",
      "type",
      "recommande"
    )
  
  ft_comb <- flextable(eval_comb_affichage) |>
    set_header_labels(
      modele_id = "Modèle",
      modele = "Type",
      lien = "Lien",
      convergence = "Convergence",
      pearson_x2_pval = "p (χ² de Pearson)",
      goodness_of_link_pval = "p (test du lien)",
      aicc = "AICc",
      commentaire = "Commentaire",
      type = "Type de modèle",
      recommande = "✔ Recommandé"
    ) |>
    style_flextable_aquapop()
  
  table_comb <- list(
    df = eval_comb_affichage,
    flextable = ft_comb
  )
  
  # ==== Tableau principal ----
  
  if (prefer_combined || isTRUE(best_sep$use_combined)) {
    table_main <- table_comb
  } else {
    table_main <- table_sep
  }
  
  # ==== Retour ----
  
  list(
    success = TRUE,
    table = table_main,
    best_model = best_model,
    message = message,
    table_sep = table_sep,
    table_comb = table_comb
  )
}