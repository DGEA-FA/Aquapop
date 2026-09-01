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
#'   \item{type}{TLO, ADD, COM ou INT}
#'   \item{lien}{Fonction de lien utilisée}
#'   \item{b0, b1, b2 et b3}{Coefficients des modèles}
#'   \item{point50, point50_F et point50_M}{les valeurs de L50 ou A50 des modèles}
#'   \item{point50_IC95_inf et point50_IC95_sup}{les intervalles de confiances,
#'   pour les modèles sexes séparés ou TLO seulement}
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
  
  # Fonction interne : évaluer un modèle individuel
  
  build_individual_model_row <- function(mod, id) {
   
    if (is.null(mod)) {
      return(data.frame(
        modele_id = id,
        type = NA_character_,
        lien = NA_character_,
        b0 = NA_real_,
        b1 = NA_real_,
        b2 = NA_real_,
        b3 = NA_real_,
        point50 = NA_real_,
        point50_F = NA_real_,
        point50_M = NA_real_,
        point50_IC95_inf = NA_real_,
        point50_IC95_sup = NA_real_,
        convergence = FALSE,
        pearson_x2_pval = NA_real_,
        goodness_of_link_pval = NA_real_,
        aicc = NA_real_
      ))
    }
    
    formule_str <- as.character(formula(mod))[3]
    type <- sub("_.*$", "", id)
    conv <- isTRUE(mod$converged)
    lien <- as.character(mod$family$link)
    
    is_separated <- grepl("^[MF]_", id)
    
    stats_mod <- maturite_extract_resultats_modele(
      mod = mod,
      id = id
    )
    
    b0 <- stats_mod$b0
    b1 <- stats_mod$b1
    
    b2 <- stats_mod$b2
    b3 <- stats_mod$b3
    
    point50 <- stats_mod$point50
    point50_F <- stats_mod$point50_F
    point50_M <- stats_mod$point50_M
    
    point50_IC95_inf <- stats_mod$point50_IC95_inf
    point50_IC95_sup <- stats_mod$point50_IC95_sup
    
    # Application des fonctions d'ajustement et évaluation du modèle
    evaluation <- maturite_evaluer_ajustement(mod)
    
    conv <- evaluation$convergence
    
    p_fit <- evaluation$pearson_x2_pval
    
    p_link <- evaluation$goodness_of_link_pval
    
    ajust <- evaluation$ajust
    
    aicc_val <- evaluation$aicc
    
    # Résultat du modèle individuel
    
    tibble(
      modele_id = id,
      type = type,
      modele = formule_str,
      lien = lien,
      b0 = b0,
      b1 = b1,
      b2 = b2,
      b3 = b3,
      point50 = point50,
      point50_F = point50_F,
      point50_M = point50_M,
      point50_IC95_inf = point50_IC95_inf,
      point50_IC95_sup = point50_IC95_sup,
      convergence = conv,
      pearson_x2_pval = p_fit,
      goodness_of_link_pval = p_link,
      ajust = ajust,
      aicc = aicc_val
    )
  }
  
  # Aucun modèle fourni

  if (length(models) == 0) {
    
    results <- tibble(
      modele_id = character(),
      type = character(),
      modele = character(),
      lien = character(),
      b0 = numeric(),
      b1 = numeric(),
      b2 = b2,
      b3 = b3,
      point50 = numeric(),
      point50_F = numeric(),
      point50_M = numeric(),
      point50_IC95_inf = numeric(),
      point50_IC95_sup = numeric(),
      convergence = logical(),
      pearson_x2_pval = numeric(),
      goodness_of_link_pval = numeric(),
      ajust = logical(),
      aicc = numeric()
    )
    
  } else {
    
    # Évaluation de tous les modèles
    results <- lapply(
      names(models),
      function(n) {
        build_individual_model_row(
          mod = models[[n]],
          id = n
        )
      }
    ) |>
      dplyr::bind_rows()
    
    
    # Ordre d'affichage :

    results <- results |>
      dplyr::arrange(
        dplyr::desc(.data$convergence),
        .data$aicc
      )
  }
  
  # Labels pour l'affichage

  labelled::var_label(results) <- list(
    modele_id = "ID",
    modele = "Formule",
    type = 'Type',
    lien = "Lien",
    b0 = "b0",
    b1 = "b1",
    b2 = "sexe",
    b3 = "interaction",
    point50 = "point50",
    point50_F = "point50_F",
    point50_M = "point50_M",
    point50_IC95_inf = "IC95 inférieur",
    point50_IC95_sup = "IC95 supérieur",
    convergence = "Convergence",
    pearson_x2_pval = "Goodness-of-fit (p-valeur)",
    goodness_of_link_pval = "Goodness-of-link (p-valeur)",
    ajust = "Ajustement",
    aicc = "AICc"
  )
  
  
  return(results)
}