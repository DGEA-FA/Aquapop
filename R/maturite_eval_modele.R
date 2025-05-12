#' Évaluer l'ajustement des modèles de maturité (L50 ou A50)
#'
#' @param models Liste des modèles retournée
#'
#' @return Un `data.frame` avec les critères d’évaluation, trié par AICc
#' @importFrom dplyr bind_rows arrange desc
#' @importFrom labelled var_label<-
#' @importFrom glue glue
#' @importFrom tibble tibble
#' @export
maturite_eval_modele <- function(models) {
  
  if (length(models) == 0) {
    results <- tibble(
      modele_id = character(),
      modele = character(),
      lien = character(),
      convergence = logical(),
      pearson_x2_pval = numeric(),
      goodness_of_link_pval = numeric(),
      aicc = numeric(),
      commentaire = character()
    )
  } else {
    results <- lapply(names(models), function(n)
      build_individual_model_row(models[[n]], n)) |>
      bind_rows() |>
      arrange(desc(convergence), aicc)
  }
  
  # Ajout de labels pour un affichage plus clair dans l'application
  var_label(results) <- list(
    modele_id = "ID",
    modele = "Modèle",
    lien = "Lien",
    convergence = "Convergence",
    pearson_x2_pval = "Goodness-of-fit (p-valeur)",
    goodness_of_link_pval = "Goodness-of-link (p-valeur)",
    aicc = "AICc",
    commentaire = "Commentaires"
  )
  
  return(results)
}

#' Évaluer les critères d’un modèle individuel de maturité
#'
#' Fonction interne utilisée par `maturite_eval_modele()` pour extraire les indicateurs
#' d’ajustement d’un modèle de type `glm` : convergence, p-valeurs, AICc, etc.
#'
#' @param mod Un objet `glm`, ou `NULL` si l’ajustement a échoué
#' @param id  Identifiant du modèle (ex. : "ltm_logit", "age_cloglog")
#'
#' @return Un `data.frame` avec les critères d’ajustement du modèle
#' @keywords internal
#'
#' @importFrom stats predict update anova formula
#' @importFrom MuMIn AICc
build_individual_model_row <- function(mod, id) {
  if (is.null(mod)) {
    return(data.frame(
      modele_id = id,
      modele = NA,
      lien = NA,
      convergence = FALSE,
      pearson_x2_pval = NA,
      goodness_of_link_pval = NA,
      aicc = NA,
      commentaire = "Données insuffisantes",
      stringsAsFactors = FALSE
    ))
  }
  
  formule_str <- as.character(formula(mod))[3]
  conv <- mod$converged
  
  # Test d’ajustement : résidus de Pearson
  p_fit <- tryCatch(o.r.test(mod), error = function(e) NA)
  
  # Test de la qualité du lien via ajout de η²
  df <- mod$model
  df$eta2 <- predict(mod, type = "link")^2
  mod_eta2 <- tryCatch(update(mod, . ~ . + eta2, data = df), error = function(e) NA)
  p_link <- tryCatch(anova(mod, mod_eta2, test = "Chisq")$`Pr(>Chi)`[2], error = function(e) NA)
  
  # Critère d'information corrigé AICc
  aicc_val <- tryCatch(AICc(mod), error = function(e) NA)
  
  # Message d'interprétation
  comm <- "Modèle valide."
  if (!conv) {
    comm <- "Ce modèle ne converge pas et devrait être rejeté."
  } else if ((!is.na(p_fit) && p_fit < 0.05) || (!is.na(p_link) && p_link < 0.05)) {
    comm <- "Ce modèle ne s'ajuste pas bien aux données. Il est préférable de choisir un autre modèle."
  }
  
  data.frame(
    modele_id = id,
    modele = formule_str,
    lien = as.character(mod$family$link),
    convergence = conv,
    pearson_x2_pval = p_fit,
    goodness_of_link_pval = p_link,
    aicc = round(aicc_val, 2),
    commentaire = comm,
    stringsAsFactors = FALSE
  )
}
