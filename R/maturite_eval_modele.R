#' Évaluer l'ajustement des modèles de maturité (L50 ou A50)
#'
#' Cette fonction compile les critères d'évaluation de plusieurs modèles de maturité
#' afin de les comparer selon leur qualité d'ajustement.
#'
#' Les modèles doivent être fournis sous forme d'une liste nommée d'objets `glm`
#' (ou `NULL` si l'ajustement a échoué), typiquement issus d'un processus de
#' génération et de sélection de modèles.
#'
#' Chaque modèle est évalué selon :
#' \itemize{
#'   \item la convergence du modèle
#'   \item un test d'ajustement basé sur les résidus de Pearson
#'   \item un test du lien (ajout d’un terme quadratique)
#'   \item le critère AICc
#' }
#'
#' Les résultats sont retournés dans un tableau trié en priorisant les modèles
#' convergents, puis par ordre croissant de AICc.
#'
#' @param models Une liste nommée de modèles `glm`. Chaque élément peut être :
#' \describe{
#'   \item{glm}{Un modèle ajusté}
#'   \item{NULL}{Si le modèle n’a pas pu être ajusté (ex. : données insuffisantes)}
#' }
#' Les noms de la liste servent d’identifiants (`modele_id`) dans le tableau final.
#'
#' @return Un tableau de résultats contenant, pour chaque modèle :
#' \describe{
#'   \item{modele_id}{Identifiant du modèle (nom de la liste)}
#'   \item{modele}{Formule du modèle}
#'   \item{lien}{Fonction de lien utilisée}
#'   \item{convergence}{Indique si le modèle a convergé}
#'   \item{pearson_x2_pval}{p-valeur du test d’ajustement (résidus de Pearson)}
#'   \item{goodness_of_link_pval}{p-valeur du test du lien}
#'   \item{aicc}{Critère d'information d'Akaike corrigé}
#'   \item{commentaire}{Interprétation qualitative de l’ajustement}
#' }
#'
#' Les colonnes sont enrichies avec des labels via le package `{labelled}` pour
#' faciliter leur affichage dans l'application Shiny.
#'
#' @export
#'
#' @importFrom dplyr bind_rows arrange desc
#' @importFrom labelled var_label<-
#' @importFrom tibble tibble
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
    results <- lapply(names(models), function(n) {
      build_individual_model_row(models[[n]], n)
    }) |>
      bind_rows() |>
      arrange(desc(.data$convergence), .data$aicc)
  }
  
  # Ajout de labels pour affichage clair dans l'application
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

#' Évaluer les critères d'un modèle individuel de maturité
#'
#' Fonction interne utilisée par `maturite_eval_modele()` pour extraire les indicateurs
#' d'ajustement d'un modèle de type `glm` : convergence, p-valeurs, aicc, etc.
#'
#' @param mod Un objet `glm`, ou `NULL` si l'ajustement a échoué
#' @param id  Identifiant du modèle (ex. : "ltm_logit", "age_cloglog")
#'
#' @return Un `data.frame` avec les critères d'ajustement du modèle
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
  
  # Test d'ajustement basé sur les résidus de Pearson
  p_fit <- tryCatch(o.r.test(mod), error = function(e) NA)
  
  # Test du lien (ajout du terme eta²)
  df <- mod$model
  df$eta2 <- predict(mod, type = "link")^2
  mod_eta2 <- tryCatch(update(mod, . ~ . + eta2, data = df), error = function(e) NA)
  p_link <- tryCatch(anova(mod, mod_eta2, test = "Chisq")$`Pr(>Chi)`[2], error = function(e) NA)
  
  # Critère AIC corrigé
  aicc_val <- tryCatch(AICc(mod), error = function(e) NA)
  
  # Commentaire interprétatif
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

