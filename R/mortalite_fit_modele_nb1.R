#' Ajuster un modèle de mortalité de type NB1 (Negative Binomial 1)
#'
#' Cette fonction ajuste un modèle NB1 sur des données de fréquence d'âge
#' étendues à l'aide de `glmmTMB`. Elle applique un test HNP avec jusqu'à
#' 5 simulations pour évaluer la qualité de l'ajustement.
#'
#' La fonction retourne toujours un `data.frame` d'une ligne. Si le modèle ne
#' peut pas être ajusté ou si certaines statistiques ne peuvent pas être
#' calculées, les valeurs correspondantes sont retournées à `NA` et
#' `convergence = FALSE`.
#'
#' @param df_age_etendue Un `data.frame` produit par
#'   `mortalite_prepare_extended()`, contenant au minimum les colonnes `age` et
#'   `number`.
#'
#' @return Un `data.frame` d'une ligne contenant :
#' \describe{
#'   \item{methode}{Type de modèle utilisé (`"nb1"`).}
#'   \item{ajustement_hnp}{Pourcentage moyen d'observations hors bande (test HNP).}
#'   \item{aicc}{Critère d'information corrigé (AICc).}
#'   \item{Z}{Coefficient estimé de l'âge dans le modèle.}
#'   \item{SE}{Erreur standard associée à `Z`.}
#'   \item{A}{Taux de mortalité annuel estimé en pourcentage.}
#'   \item{ic95}{Intervalle de confiance approximatif du taux `A`.}
#'   \item{commentaire}{Interprétation qualitative de l'ajustement ou message d'échec.}
#'   \item{convergence}{Booléen indiquant si le modèle a convergé.}
#'   \item{nb_iterations_hnp}{Nombre total de simulations HNP effectuées.}
#' }
#'
#' @importFrom dplyr case_when
#' @importFrom MuMIn AICc
#' @importFrom glue glue
#' @importFrom glmmTMB glmmTMB nbinom1
#' @importFrom hnp hnp
#' @importFrom stats residuals simulate 
#' @importFrom tibble tibble
#'
#' @examples
#' df_fake <- tibble::tibble(age = 1:6, number = c(180, 120, 70, 40, 25, 10))
#' mortalite_fit_modele_nb1(df_fake)
#'
#' @export
mortalite_fit_modele_nb1 <- function(df_age_etendue) {
  # Validation de base ====
  if (is.null(df_age_etendue) || !is.data.frame(df_age_etendue) || nrow(df_age_etendue) == 0) {
    return(tibble(
      methode = "nb1",
      ajustement_hnp = NA_real_,
      aicc = NA_real_,
      Z = NA_real_,
      SE = NA_real_,
      A = NA_real_,
      ic95 = NA_character_,
      commentaire = "Aucune donnée disponible pour ajuster le modèle.",
      convergence = FALSE,
      nb_iterations_hnp = NA_real_
    ))
  }
  
  if (!all(c("age", "number") %in% names(df_age_etendue))) {
    return(tibble(
      methode = "nb1",
      ajustement_hnp = NA_real_,
      aicc = NA_real_,
      Z = NA_real_,
      SE = NA_real_,
      A = NA_real_,
      ic95 = NA_character_,
      commentaire = "Les colonnes `age` et `number` sont requises.",
      convergence = FALSE,
      nb_iterations_hnp = NA_real_
    ))
  }
  
  if (nrow(df_age_etendue) < 2 || length(unique(df_age_etendue$age)) < 2) {
    return(tibble(
      methode = "nb1",
      ajustement_hnp = NA_real_,
      aicc = NA_real_,
      Z = NA_real_,
      SE = NA_real_,
      A = NA_real_,
      ic95 = NA_character_,
      commentaire = "Le modèle requiert au moins deux âges distincts.",
      convergence = FALSE,
      nb_iterations_hnp = NA_real_
    ))
  }
  
  # Ajustement du modèle NB1 ====
  model <- tryCatch(
    glmmTMB(
      number ~ age,
      family = nbinom1(link = "log"),
      data = df_age_etendue
    ),
    error = function(e) NULL
  )
  
  if (is.null(model)) {
    return(tibble(
      methode = "nb1",
      ajustement_hnp = NA_real_,
      aicc = NA_real_,
      Z = NA_real_,
      SE = NA_real_,
      A = NA_real_,
      ic95 = NA_character_,
      commentaire = "Le modèle n'a pas pu être ajusté.",
      convergence = FALSE,
      nb_iterations_hnp = NA_real_
    ))
  }
  
  model_converged <- isTRUE(model$fit$convergence == 0)
  
  if (!model_converged) {
    return(tibble(
      methode = "nb1",
      ajustement_hnp = NA_real_,
      aicc = tryCatch(AICc(model), error = function(e) NA_real_),
      Z = NA_real_,
      SE = NA_real_,
      A = NA_real_,
      ic95 = NA_character_,
      commentaire = "Le modèle ne semble pas avoir convergé.",
      convergence = FALSE,
      nb_iterations_hnp = NA_real_
    ))
  }
  
  # Fonction interne : test HNP pour NB1 ====
  simuler_hnp_nb1 <- function(model, data_modele, n_iter = 2) {
    replicate(
      n_iter,
      hnp(
        model,
        newclass = TRUE,
        diagfun = residuals,
        simfun = function(n, obj) simulate(obj)[[1]],
        fitfun = function(y) {
          try(
            glmmTMB(
              y ~ age,
              family = nbinom1(link = "log"),
              data = data_modele
            ),
            silent = TRUE
          )
        },
        how.many.out = TRUE,
        plot.sim = FALSE
      ),
      simplify = FALSE
    ) |>
      sapply(function(result_hnp) result_hnp$out / result_hnp$total * 100)
  }
  
  # Test HNP initial ====
  hnp_valeurs <- tryCatch(
    {
      set.seed(2023)
      simuler_hnp_nb1(model, df_age_etendue, n_iter = 2)
    },
    error = function(e) NULL
  )
  
  if (is.null(hnp_valeurs)) {
    ajustement_hnp <- NA_real_
    nb_iterations_hnp <- NA_real_
  } else {
    ajustement_hnp <- round(mean(hnp_valeurs), 2)
    nb_iterations_hnp <- 2
    
    if (!is.na(ajustement_hnp) && ajustement_hnp >= 10 && ajustement_hnp < 15) {
      hnp_valeurs_suppl <- tryCatch(
        simuler_hnp_nb1(model, df_age_etendue, n_iter = 3),
        error = function(e) NULL
      )
      
      if (!is.null(hnp_valeurs_suppl)) {
        hnp_valeurs <- c(hnp_valeurs, hnp_valeurs_suppl)
        ajustement_hnp <- round(mean(hnp_valeurs), 2)
        nb_iterations_hnp <- 5
      }
    }
  }
  
  # Extraction des coefficients ====
  coef_table <- tryCatch(
    summary(model)$coefficients$cond,
    error = function(e) NULL
  )
  
  if (is.null(coef_table) || !("age" %in% rownames(coef_table))) {
    return(tibble(
      methode = "nb1",
      ajustement_hnp = ajustement_hnp,
      aicc = tryCatch(AICc(model), error = function(e) NA_real_),
      Z = NA_real_,
      SE = NA_real_,
      A = NA_real_,
      ic95 = NA_character_,
      commentaire = "Le modèle a convergé, mais les paramètres n'ont pas pu être extraits.",
      convergence = FALSE,
      nb_iterations_hnp = nb_iterations_hnp
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
  ic_95 <- glue("[{lowerA}-{upperA}]")
  
  # Commentaire ====
  commentaire <- case_when(
    is.na(ajustement_hnp) ~ "Modèle ajusté, mais test HNP non calculable.",
    ajustement_hnp < 10 ~ "Bon ajustement",
    ajustement_hnp < 15 ~ "Ajustement marginal",
    TRUE ~ "Mauvais ajustement"
  )
  
  tibble(
    methode = "nb1",
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
}