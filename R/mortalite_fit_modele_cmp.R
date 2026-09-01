# ==== helper mortalité - test HNP CMP =========================================

#' Exécuter le test HNP pour un modèle CMP
#'
#' Cette fonction interne applique le schéma HNP utilisé dans les modèles de
#' mortalité : 2 simulations initiales, puis 3 simulations additionnelles si
#' l'ajustement est marginal (entre 10 et 15 % d'observations hors bande).
#'
#' La fonction retourne toujours un `data.frame` d'une ligne. Si le modèle ne
#' peut pas être ajusté ou si certaines statistiques ne peuvent pas être
#' calculées, les valeurs correspondantes sont retournées à `NA` et
#' `convergence = FALSE`.
#'
#' @param model Objet modèle ajusté avec `glmmTMB`.
#' @param df_age_etendue Un `data.frame` contenant les colonnes `age` et `number`.
#' @param max_refit_attempts Nombre maximal d'essais permis dans la fonction
#'   interne de réajustement utilisée par `hnp()`.
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
#' @importFrom glmmTMB glmmTMB nbinom1
#' @importFrom hnp hnp
#' @importFrom stats residuals simulate
#' @noRd
#' @keywords internal


   # Fonctions de base pour CMP  ====
diagfun_cmp <- function(obj) {
  residuals(obj, type = "pearson")
}

simfun_cmp <- function(n, obj) {
  simulate(obj)[[1]]
}

make_fitfun_cmp <- function(df_age_etendue, model, max_refit_attempts = 10L) {
  
  function(y) {
    
    attempt <- 1L
    
    fit <- try(
      glmmTMB(
        y ~ age,
        family = compois(link = "log"),
        data = df_age_etendue
      ),
      silent = TRUE
    )
    
    while (inherits(fit, "try-error") &&
           attempt < max_refit_attempts) {
      
      y_retry <- try(simulate(model)[[1]], silent = TRUE)
      
      if (inherits(y_retry, "try-error")) {
        return(NULL)
      }
      
      fit <- try(
        glmmTMB(
          y_retry ~ age,
          family = compois(link = "log"),
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
}

# Test HNP  ====
simuler_hnp_cmp <- function(model, df_age_etendue, n_iter){
  
  fitfun_cmp <- make_fitfun_cmp(
    df_age_etendue = df_age_etendue,
    model = model
  )
  
  set.seed(2023)
  
  resultats_hnp <- replicate(
    n_iter,
    hnp(
      model,
      newclass = TRUE,
      diagfun = diagfun_cmp,
      simfun = simfun_cmp,
      fitfun = fitfun_cmp,
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

run_hnp_cmp <- function(model, df_age_etendue){
  
  test_hnp(
    simuler = simuler_hnp_cmp,
    model = model,
    df_age_etendue = df_age_etendue
  )
  
}


  

# ==== modèle mortalité CMP ====================================================

#' Ajuster un modèle de mortalité de type CMP (Conway-Maxwell-Poisson)
#'
#' Cette fonction ajuste un modèle CMP (`glmmTMB`) sur des données de fréquence
#' d'âge étendues. Elle applique un test HNP (Half-Normal Plot) avec 2 à
#' 5 simulations pour évaluer la qualité de l'ajustement, puis retourne les
#' paramètres estimés, le taux de mortalité annuel, et un commentaire sur
#' l'ajustement.
#'
#' Dans cette version refactorisée, la logique est rendue plus robuste :
#' \itemize{
#'   \item les cas d'échec retournent une ligne standardisée plutôt qu'une erreur;
#'   \item la boucle de réajustement utilisée dans le test HNP est bornée;
#'   \item la logique HNP est isolée dans une fonction interne dédiée.
#' }
#'
#' @param df_age_etendue Un `data.frame` produit par `mortalite_prepare_extended()`
#'   contenant au minimum :
#' \describe{
#'   \item{age}{Âge des individus (entier)}
#'   \item{number}{Nombre d'individus observés à cet âge}
#' }
#'
#' @return Un `data.frame` d'une ligne contenant :
#' \describe{
#'   \item{methode}{Type de modèle (`"cmp"`)}
#'   \item{ajustement_hnp}{Pourcentage moyen d'observations hors bande (test HNP)}
#'   \item{aicc}{Critère d'information corrigé (AICc)}
#'   \item{Z}{Coefficient de régression sur l'âge}
#'   \item{SE}{Erreur standard associée à `Z`}
#'   \item{A}{Taux de mortalité annuel estimé (%)}
#'   \item{ic95}{Intervalle de confiance de `A`}
#'   \item{commentaire}{Appréciation qualitative de l'ajustement}
#'   \item{convergence}{Convergence interprétable du modèle (booléen)}
#'   \item{nb_iterations_hnp}{Nombre de simulations HNP effectuées (2 ou 5)}
#' }
#'
#' @examples
#' df_fake <- tibble::tibble(age = 1:6, number = c(180, 120, 70, 40, 25, 10))
#' mortalite_fit_modele_cmp(df_fake)
#'
#' @importFrom dplyr case_when
#' @importFrom MuMIn AICc
#' @importFrom glue glue
#' @importFrom glmmTMB compois glmmTMB
#' @importFrom tibble tibble
#' @export
mortalite_fit_modele_cmp <- function(df_age_etendue) {
  # Validation de base ====
  if (is.null(df_age_etendue) || !is.data.frame(df_age_etendue) || nrow(df_age_etendue) == 0) {
    return(list(
      tableau = tibble(
        methode = "cmp",
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
        methode = "cmp",
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
        methode = "cmp",
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
  
  # Ajustement du modèle CMP ====
  model <- tryCatch(
    glmmTMB(
      number ~ age,
      family = compois(link = "log"),
      data = df_age_etendue
    ),
    error = function(e) NULL
  )
  
  if (is.null(model)) {
    return(list(
      tableau = tibble(
        methode = "cmp",
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
  
  convergence_modele <- isTRUE(model$fit$convergence == 0)
  
  hnp_res <- run_hnp_cmp(
    model = model,
    df_age_etendue = df_age_etendue
  )
  
  graph_hnp <- hnp_res$graph_hnp
  
  coef_table <- tryCatch(
    summary(model)$coefficients$cond,
    error = function(e) NULL
  )
  
  if (is.null(coef_table) || !("age" %in% rownames(coef_table))) {
    return(list(
      tableau = tibble(
      methode = "cmp",
      ajustement_hnp = hnp_res$ajustement_hnp,
      aicc = tryCatch(AICc(model), error = function(e) NA_real_),
      Z = NA_real_,
      SE = NA_real_,
      A = NA_real_,
      ic95 = NA_character_,
      commentaire = "Le modèle a été ajusté, mais les paramètres n'ont pas pu être extraits.",
      convergence = FALSE,
      nb_iterations_hnp = hnp_res$nb_iterations_hnp
      ),
      graph_hnp = graph_hnp
      ))
  }
  
  Z <- abs(coef_table["age", "Estimate"])
  SE <- coef_table["age", "Std. Error"]
  
  A <- (1 - exp(-Z)) * 100
  lowerZ <- Z - SE
  upperZ <- Z + SE
  lowerA <- round((1 - exp(-lowerZ)) * 100, 1)
  upperA <- round((1 - exp(-upperZ)) * 100, 1)
  ic_95 <- glue("[{lowerA} – {upperA}]") |>
    gsub("\\.", ",", x = _)
  
  commentaire <- case_when(
    is.na(hnp_res$ajustement_hnp) ~ "Modèle ajusté, mais test HNP non calculable.",
    hnp_res$ajustement_hnp < 10 ~ "Bon ajustement",
    hnp_res$ajustement_hnp < 15 ~ "Ajustement marginal",
    TRUE ~ "Mauvais ajustement"
  )
  
  resultat <- tibble(
    methode = "cmp",
    ajustement_hnp = hnp_res$ajustement_hnp,
    aicc = tryCatch(AICc(model), error = function(e) NA_real_),
    Z = round(Z, 4),
    SE = round(SE, 4),
    A = round(A, 1),
    ic95 = ic_95,
    commentaire = commentaire,
    convergence = convergence_modele,
    nb_iterations_hnp = hnp_res$nb_iterations_hnp
    )
  
  list(
    tableau = resultat,
    graph_hnp = graph_hnp
  )
}