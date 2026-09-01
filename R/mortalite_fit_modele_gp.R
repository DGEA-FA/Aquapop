#' Ajuster un modèle de mortalité de type GP (Generalized Poisson)
#'
#' Cette fonction ajuste un modèle de régression `glmmTMB` avec distribution GP
#' (Generalized Poisson) sur des données de fréquence d'âge. Elle applique un
#' test HNP (Half-Normal Plot) en 2 à 5 simulations, puis retourne les
#' estimations du modèle et un commentaire sur la qualité de l'ajustement.
#'
#' La fonction retourne toujours un `data.frame` d'une ligne. Si le modèle ne
#' peut pas être ajusté ou si certaines statistiques ne peuvent pas être
#' calculées, les valeurs correspondantes sont retournées à `NA` et
#' `convergence = FALSE`.
#'
#' @param df_age_etendue Un `data.frame` produit par
#'   `mortalite_prepare_extended()` contenant au minimum les colonnes `age` et
#'   `number`.
#'
#' @return Une liste contenant :
#' \describe{
#'  Un `data.frame` d'une ligne contenant :
#' \describe{
#'   \item{methode}{Modèle utilisé (`"gp"`).}
#'   \item{ajustement_hnp}{Pourcentage moyen d'observations hors bande (HNP).}
#'   \item{aicc}{Critère d'information corrigé (AICc).}
#'   \item{Z}{Coefficient d'âge (valeur absolue).}
#'   \item{SE}{Erreur standard de `Z`.}
#'   \item{A}{Taux de mortalité annuel estimé en pourcentage.}
#'   \item{ic95}{Intervalle de confiance approximatif de `A`.}
#'   \item{commentaire}{Appréciation qualitative de l'ajustement ou message d'échec.}
#'   \item{convergence}{Booléen indiquant si le modèle a convergé.}
#'   \item{nb_iterations_hnp}{Nombre d'itérations HNP effectuées.}
#'   Un `plot` : graphique des résidus du test hnp, ou `NULL`
#' }
#'
#' @importFrom dplyr case_when
#' @importFrom MuMIn AICc
#' @importFrom glue glue
#' @importFrom hnp hnp
#' @importFrom stats residuals simulate 
#' @importFrom glmmTMB genpois glmmTMB
#' @importFrom tibble tibble
#'
#' @examples
#' df_fake <- tibble::tibble(age = 1:6, number = c(180, 120, 70, 40, 25, 10))
#' mortalite_fit_modele_gp(df_fake)
#'
#' @export
mortalite_fit_modele_gp <- function(df_age_etendue) {
  # Validation de base ====
  if (is.null(df_age_etendue) || !is.data.frame(df_age_etendue) || nrow(df_age_etendue) == 0) {
    return(list(
      tableau = tibble(
      methode = "gp",
      ajustement_hnp = NA_real_,
      aicc = NA_real_,
      Z = NA_real_,
      SE = NA_real_,
      A = NA_real_,
      ic95 = NA_character_,
      commentaire = "Aucune donnée disponible pour ajuster le modèle.",
      convergence = FALSE,
      nb_iterations_hnp = NA_real_
      ),
      graph_hnp = NULL
    ))
  }
  
  if (!all(c("age", "number") %in% names(df_age_etendue))) {
    return(list(
      tableau = tibble(
      methode = "gp",
      ajustement_hnp = NA_real_,
      aicc = NA_real_,
      Z = NA_real_,
      SE = NA_real_,
      A = NA_real_,
      ic95 = NA_character_,
      commentaire = "Les colonnes `age` et `number` sont requises.",
      convergence = FALSE,
      nb_iterations_hnp = NA_real_
      ),
      graph_hnp = NULL
    ))
  }
  
  if (nrow(df_age_etendue) < 2 || length(unique(df_age_etendue$age)) < 2) {
    return(list(
      tableau = tibble(
      methode = "gp",
      ajustement_hnp = NA_real_,
      aicc = NA_real_,
      Z = NA_real_,
      SE = NA_real_,
      A = NA_real_,
      ic95 = NA_character_,
      commentaire = "Le modèle requiert au moins deux âges distincts.",
      convergence = FALSE,
      nb_iterations_hnp = NA_real_
      ),
      graph_hnp = NULL
    ))
  }
  
  # Ajustement du modèle GP ====
  model <- tryCatch(
    glmmTMB(
      number ~ age,
      family = genpois(link = "log"),
      data = df_age_etendue
    ),
    error = function(e) NULL
  )
  
  if (is.null(model)) {
    return(list(
      tableau = tibble(
      methode = "gp",
      ajustement_hnp = NA_real_,
      aicc = NA_real_,
      Z = NA_real_,
      SE = NA_real_,
      A = NA_real_,
      ic95 = NA_character_,
      commentaire = "Le modèle n'a pas pu être ajusté.",
      convergence = FALSE,
      nb_iterations_hnp = NA_real_
      ),
      graph_hnp = NULL
    ))
  }
  
  model_converged <- isTRUE(model$fit$convergence == 0)
  
  if (!model_converged) {
    return(list(
      tableau = tibble(
      methode = "gp",
      ajustement_hnp = NA_real_,
      aicc = tryCatch(AICc(model), error = function(e) NA_real_),
      Z = NA_real_,
      SE = NA_real_,
      A = NA_real_,
      ic95 = NA_character_,
      commentaire = "Le modèle ne semble pas avoir convergé.",
      convergence = FALSE,
      nb_iterations_hnp = NA_real_
      ),
      graph_hnp = NULL
    ))
  }
  
  # Fonctions internes HNP ====
  diagfun_gp <- function(obj) {
    residuals(obj, type = "pearson")
  }
  
  simfun_gp <- function(n, obj) {
    simulate(obj)[[1]]
  }
  
  fitfun_gp <- function(y) {
    max_attempts <- 10L
    
    fit <- try(
      glmmTMB(
        y ~ age,
        family = genpois(link = "log"),
        data = df_age_etendue
      ),
      silent = TRUE
    )
    
    attempt <- 1L
    
    while (inherits(fit, "try-error") && attempt < max_attempts) {
      y_retry <- try(simulate(model)[[1]], silent = TRUE)
      
      if (inherits(y_retry, "try-error")) {
        return(NULL)
      }
      
      fit <- try(
        glmmTMB(
          y_retry ~ age,
          family = genpois(link = "log"),
          data = df_age_etendue
        ),
        silent = TRUE
      )
      
      attempt <- attempt + 1L
    }
    
    if (inherits(fit, "try-error")) {
      return(NULL)
    }
    
    fit
  }
  
  # Test HNP  ====
  simuler_hnp_gp <- function(model, n_iter){
    
    set.seed(2023)
    
    resultats_hnp <- replicate(
      n_iter,
      hnp(
        model,
        newclass = TRUE,
        diagfun = diagfun_gp,
        simfun = simfun_gp,
        fitfun = fitfun_gp,
        how.many.out = TRUE,
        plot.sim = FALSE
      ),
      simplify = FALSE
    )
    
    list(
      hnp = resultats_hnp,
      pct = sapply(
        resultats_hnp,
        function(x) x$out / x$total * 100
      )
    )
  }
  
  res_hnp <- test_hnp(simuler_hnp_gp, model)
  
  ajustement_hnp    <- res_hnp$ajustement_hnp
  nb_iterations_hnp <- res_hnp$nb_iterations_hnp
  graph_hnp         <- res_hnp$graph_hnp
  
  # Test HNP initial ====
#  res_hnp <- tryCatch(
#    {
#      set.seed(2023)
#      replicate(
#        2,
#        hnp(
#          model,
#          newclass = TRUE,
#          diagfun = diagfun_gp,
#          simfun = simfun_gp,
#          fitfun = fitfun_gp,
#          how.many.out = TRUE,
#          plot.sim = FALSE
#        ),
#        simplify = FALSE
#      )
#      
#      list(
#        hnp <- resultats_hnp,
#        pct = sapply(
#          resultats_hnp,
#          function(result_hnp) result_hnp$out / result_hnp$total * 100
#        )
#      )
#    },
#    error = function(e) NULL
#  )
#  
#  if (is.null(res_hnp)) {
#    ajustement_hnp <- NA_real_
#    nb_iterations_hnp <- NA_real_
#    hnp_graph <- NULL
#    
#  } else {
#    
#    hnp_valeurs <- res_hnp$pct
#    hnp_graph <- res_hnp$hnp
#    
#    ajustement_hnp <- round(mean(hnp_valeurs), 2)
#    nb_iterations_hnp <- 2
#    
#    if (!is.na(ajustement_hnp) && ajustement_hnp >= 10 && ajustement_hnp < 15) {
#      res_hnp_suppl <- tryCatch(
#        {
#          replicate(
#            3,
#            hnp(
#              model,
#              newclass = TRUE,
#              diagfun = diagfun_gp,
#              simfun = simfun_gp,
#              fitfun = fitfun_gp,
#              how.many.out = TRUE,
#              plot.sim = FALSE
#            ),
#            simplify = FALSE
#          )
#          
#          list(
#            hnp = resultats_hnp,
#            pct = sapply(
#              resultats_hnp,
#              function(result_hnp) result_hnp$out / result_hnp$total * 100
#            )
#          )
#        },
#        error = function(e) NULL
#      )
#      
#      if (!is.null(res_hnp_suppl)) {
#        hnp_valeurs <- c(hnp_valeurs, res_hnp_suppl$pct)
#        hnp_graph <- c(
#          hnp_graph,
#          res_hnp_suppl$hnp
#        )
#        ajustement_hnp <- round(mean(hnp_valeurs), 2)
#        nb_iterations_hnp <- 5
#      }
#    }
#  }
#  
  # Extraction des coefficients ====
  coef_table <- tryCatch(
    summary(model)$coefficients$cond,
    error = function(e) NULL
  )
  
  if (is.null(coef_table) || !("age" %in% rownames(coef_table))) {
    return(list(
      tableau = tibble(
      methode = "gp",
      ajustement_hnp = ajustement_hnp,
      aicc = tryCatch(AICc(model), error = function(e) NA_real_),
      Z = NA_real_,
      SE = NA_real_,
      A = NA_real_,
      ic95 = NA_character_,
      commentaire = "Le modèle a convergé, mais les paramètres n'ont pas pu être extraits.",
      convergence = FALSE,
      nb_iterations_hnp = nb_iterations_hnp
      ),
      graph_hnp = graph_hnp
    ))
  }
  
  Z <- abs(coef_table["age", "Estimate"])
  SE <- coef_table["age", "Std. Error"]
  
  # Conversion en taux de mortalité annuel ====
  A <- (1 - exp(-Z)) * 100
  lowerZ <- Z - SE
  upperZ <- Z + SE
  lowerA <- round((1 - exp(-lowerZ)) * 100, 1)
  upperA <- round((1 - exp(-upperZ)) * 100, 1)
  ic_95 <- glue("[{lowerA} – {upperA}]") |>
    gsub("\\.", ",", x = _)
  
  # Commentaire ====
  commentaire <- case_when(
    is.na(ajustement_hnp) ~ "Modèle ajusté, mais test HNP non calculable.",
    ajustement_hnp < 10 ~ "Bon ajustement",
    ajustement_hnp < 15 ~ "Ajustement marginal",
    TRUE ~ "Mauvais ajustement"
  )
  
  resultat <- tibble(
    methode = "gp",
    ajustement_hnp = ajustement_hnp,
    aicc = tryCatch(AICc(model), error = function(e) NA_real_),
    Z = round(Z, 4),
    SE = round(SE, 4),
    A = round(A, 1),
    ic95 = ic_95,
    commentaire = commentaire,
    convergence = TRUE,
    nb_iterations_hnp = nb_iterations_hnp
  )
  
  list(
    tableau = resultat,
    graph_hnp = graph_hnp
  )
}