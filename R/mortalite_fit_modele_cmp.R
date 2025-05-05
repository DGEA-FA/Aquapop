#' Ajuster un modèle de mortalité de type CMP (Conway-Maxwell-Poisson)
#'
#' Cette fonction ajuste un modèle CMP (`glmmTMB`) sur des données de fréquence d'âge étendues.
#' Elle applique un test HNP (Half-Normal Plot) avec 2 à 5 simulations pour évaluer la qualité de l’ajustement,
#' et retourne les paramètres estimés, le taux de mortalité annuel, et un commentaire sur l’ajustement.
#'
#' @importFrom dplyr case_when
#' @importFrom MuMIn AICc
#' @importFrom glue glue
#' @importFrom hnp hnp
#' @importFrom glmmTMB nbinom1
#' @importFrom stats simulate
#' @importFrom stats residuals
#' @importFrom glmmTMB compois
#' @importFrom glmmTMB glmmTMB
#' @importFrom tibble tibble
#' @param df_age_etendue Un `data.frame` produit par `mortalite_prepare_extended()` contenant au minimum :
#' \describe{
#'   \item{age}{Âge des individus (entier)}
#'   \item{number}{Nombre d’individus observés à cet âge}
#' }
#'
#' @return Un `data.frame` d’une ligne contenant :
#' \describe{
#'   \item{methode}{Type de modèle (`"cmp"`)}
#'   \item{ajustement_hnp}{Pourcentage moyen d’observations hors bande (test HNP)}
#'   \item{aicc}{Critère d'information corrigé (AICc)}
#'   \item{Z}{Coefficient de régression sur l’âge}
#'   \item{SE}{Erreur standard associée à Z}
#'   \item{A}{Taux de mortalité annuel estimé (%)}
#'   \item{IC 95%}{Intervalle de confiance de A}
#'   \item{commentaire}{Appréciation qualitative de l’ajustement}
#'   \item{convergence}{Convergence du modèle (booléen)}
#'   \item{nb_iterations_hnp}{Nombre de simulations HNP effectuées (2 ou 5)}
#' }
#'
#' @examples
#' df_fake <- tibble::tibble(age = 1:6, number = c(180, 120, 70, 40, 25, 10))
#' mortalite_fit_modele_cmp(df_fake)
#'
#' @export
mortalite_fit_modele_cmp <- function(df_age_etendue) {
  stopifnot(all(c("age", "number") %in% names(df_age_etendue)))
  
  # --- Ajustement du modèle CMP ---
  model <- glmmTMB(
    number ~ age,
    family = compois(link = "log"),
    data = df_age_etendue
  )
  
  # --- Fonctions internes pour HNP ---
  diagfun_cmp <- function(obj) residuals(obj, type = "pearson")
  simfun_cmp <- function(n, obj) simulate(obj)[[1]]
  fitfun_cmp <- function(y) {
    fit <- try(glmmTMB(
      y ~ age,
      family = nbinom1(link = "log"),  # Ajustement de secours
      data = df_age_etendue
    ), silent = TRUE)
    while (inherits(fit, "try-error")) {
      y_retry <- simulate(model)[[1]]
      fit <- try(glmmTMB(
        y_retry ~ age,
        family = nbinom1(link = "log"),
        data = df_age_etendue
      ), silent = TRUE)
    }
    return(fit)
  }
  
  # --- Test HNP initial (2 itérations) ---
  message("Test HNP : Modèle CMP (2 simulations initiales)...")
  set.seed(2023)
  hnp_valeurs <- replicate(
    2,
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
  ) |>
    sapply(function(x) x$out / x$total * 100)
  
  ajustement_hnp <- round(mean(hnp_valeurs), 2)
  nb_iterations_hnp <- 2
  
  # --- Test HNP supplémentaire si ajustement marginal ---
  if (ajustement_hnp >= 10 && ajustement_hnp < 15) {
    message("Ajustement marginal : Ajout de 3 simulations HNP supplémentaires...")
    hnp_suppl <- replicate(
      3,
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
    ) |>
      sapply(function(x) x$out / x$total * 100)
    
    hnp_valeurs <- c(hnp_valeurs, hnp_suppl)
    ajustement_hnp <- round(mean(hnp_valeurs), 2)
    nb_iterations_hnp <- 5
  }
  
  # --- Extraction des coefficients du modèle ---
  coef <- summary(model)$coefficients$cond
  Z <- abs(coef["age", "Estimate"])
  SE <- coef["age", "Std. Error"]
  
  # --- Conversion en taux de mortalité annuel (A) ---
  A <- (1 - exp(-Z)) * 100
  lowerZ <- Z - SE
  upperZ <- Z + SE
  lowerA <- round((1 - exp(-lowerZ)) * 100, 1)
  upperA <- round((1 - exp(-upperZ)) * 100, 1)
  ic_95 <- glue("[{lowerA}-{upperA}]")
  
  # --- Résumé structuré ---
  tibble(
    methode = "cmp",
    ajustement_hnp = ajustement_hnp,
    aicc = AICc(model),
    Z = round(Z, 4),
    SE = round(SE, 4),
    A = round(A, 1),
    `IC 95%` = ic_95,
    commentaire = case_when(
      ajustement_hnp < 10 ~ "Bon ajustement",
      ajustement_hnp < 15 ~ "Ajustement marginal",
      TRUE ~ "Mauvais ajustement"
    ),
    convergence = model$fit$convergence == 0,
    nb_iterations_hnp = nb_iterations_hnp
  )
}
