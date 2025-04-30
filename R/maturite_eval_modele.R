# =============================================================================
# Fonction : maturite_eval_modele()
# Projet   : AquaPop
# But      : Évaluer l’ajustement de modèles L50
# Auteur   : [Votre nom]
# Date     : [Date]
# =============================================================================

#' Évalue l'ajustement des modèles L50
#'
#' @param models Liste des modèles retournée par fit_L50_models() ou fit_L50_combined_models()
#'
#' @return Un dataframe avec les critères de convergence et d'ajustement, trié par ordre croissant d'AICc.
#' @export
maturite_eval_modele <- function(models) {
  library(dplyr)
  library(MuMIn)      # Pour AICc
  library(DescTools)  # Pour o.r.test
  library(glue)
  library(labelled)   # Pour ajouter des labels d'affichage
  
  results <- lapply(names(models), function(n) build_individual_model_row(models[[n]], n)) %>%
    bind_rows() %>%
    arrange(desc(convergence), aicc)
  
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

# =============================================================================
# Fonction interne : build_individual_model_row()
# =============================================================================

#' Évaluer les critères d’un modèle individuel L50
#'
#' Fonction interne utilisée pour extraire les critères d’ajustement
#' (convergence, p-valeurs, AICc, etc.) d’un modèle de type `glm` utilisé
#' dans l’évaluation de la maturité (L50).
#'
#' @param mod Un objet de type `glm` (ou `NULL` si le modèle n’a pas pu être ajusté).
#' @param id  Identifiant du modèle (ex. combinaison lien + variable).
#'
#' @return Un `data.frame` avec les critères d’ajustement pour un seul modèle.
#' @keywords internal
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
  
  modele_id <- id
  modele <- as.character(formula(mod))[3]
  
  conv <- mod$converged
  
  p_fit <- tryCatch(o.r.test(mod), error = function(e) NA)
  
  df <- mod$model
  df$eta2 <- predict(mod, type = "link")^2
  mod_eta2 <- tryCatch(update(mod, . ~ . + eta2, data = df), error = function(e) NA)
  p_link <- tryCatch(anova(mod, mod_eta2, test = "Chisq")$`Pr(>Chi)`[2],
                     error = function(e) NA)
  
  aicc_val <- tryCatch(AICc(mod), error = function(e) NA)
  
  comm <- "Modèle valide."
  if (!conv) {
    comm <- "Ce modèle ne converge pas et devrait être rejeté."
  } else if ((!is.na(p_fit) && p_fit < 0.05) || (!is.na(p_link) && p_link < 0.05)) {
    comm <- "Ce modèle ne s'ajuste pas bien aux données. Il est préférable de choisir un autre modèle."
  }
  
  data.frame(
    modele_id = modele_id,
    modele = modele,
    lien = as.character(mod$family$link),
    convergence = conv,
    pearson_x2_pval = p_fit,
    goodness_of_link_pval = p_link,
    aicc = round(aicc_val, 2),
    commentaire = comm,
    stringsAsFactors = FALSE
  )
}
