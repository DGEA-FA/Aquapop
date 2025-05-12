#' Ajuster un modèle de mortalité de type GP (Generalized Poisson)
#'
#' Cette fonction ajuste un modèle de régression `glmmTMB` avec distribution GP (Generalized Poisson)
#' sur des données de fréquence d'âge. Elle applique un test HNP (Half-Normal Plot) en 2 à 5 simulations,
#' puis retourne les estimations du modèle et un commentaire sur la qualité de l’ajustement.
#'
#' @importFrom dplyr case_when
#' @importFrom MuMIn AICc
#' @importFrom glue glue
#' @importFrom hnp hnp
#' @importFrom stats simulate
#' @importFrom stats residuals
#' @importFrom glmmTMB genpois
#' @importFrom glmmTMB glmmTMB
#' @importFrom tibble tibble
#' @param df_age_etendue Un `data.frame` produit par `mortalite_prepare_extended()` contenant au minimum :
#' \describe{
#'   \item{age}{Âge des individus}
#'   \item{number}{Nombre d’individus observés à cet âge}
#' }
#'
#' @return Un `data.frame` d’une ligne contenant :
#' \describe{
#'   \item{methode}{Modèle utilisé (`"gp"`)}
#'   \item{ajustement_hnp}{Pourcentage moyen d’observations hors bande (HNP)}
#'   \item{aicc}{Critère d’information corrigé aicc}
#'   \item{Z}{Coefficient d'âge (valeur absolue)}
#'   \item{SE}{Erreur standard de Z}
#'   \item{A}{Taux de mortalité annuel estimé (%)}
#'   \item{IC 95%}{Intervalle de confiance de A (bornes min–max)}
#'   \item{commentaire}{Appréciation qualitative de l’ajustement}
#'   \item{convergence}{Convergence du modèle (booléen)}
#'   \item{nb_iterations_hnp}{Nombre d’itérations HNP effectuées}
#' }
#'
#' @examples
#' df_fake <- tibble::tibble(age = 1:6, number = c(180, 120, 70, 40, 25, 10))
#' mortalite_fit_modele_gp(df_fake)
#'
#' @export
mortalite_fit_modele_gp <- function(df_age_etendue) {
  stopifnot(all(c("age", "number") %in% names(df_age_etendue)))
  
  # --- Ajustement du modèle GP ---
  model <- glmmTMB(
    number ~ age,
    family = genpois(link = "log"),
    data = df_age_etendue
  )
  
  # --- Fonctions internes HNP ---
  diagfun_gp <- function(obj) residuals(obj, type = "pearson")
  simfun_gp <- function(n, obj) simulate(obj)[[1]]
  
  fitfun_gp <- function(y) {
    fit <- try(glmmTMB(
      y ~ age,
      family = genpois(link = "log"),
      data = df_age_etendue
    ), silent = TRUE)
    
    while (inherits(fit, "try-error")) {
      y_retry <- simulate(model)[[1]]
      fit <- try(glmmTMB(
        y_retry ~ age,
        family = genpois(link = "log"),
        data = df_age_etendue
      ), silent = TRUE)
    }
    return(fit)
  }
  
  # --- Test HNP initial (2 itérations) ---
  message("Test HNP : Modèle GP (2 simulations initiales)...")
  set.seed(2023)
  hnp_valeurs <- replicate(
    2,
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
        diagfun = diagfun_gp,
        simfun = simfun_gp,
        fitfun = fitfun_gp,
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
  
  # --- Extraction des coefficients ---
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
  
  # --- Résultat final structuré ---
  tibble(
    methode = "gp",
    ajustement_hnp = ajustement_hnp,
    aicc = AICc(model),
    Z = round(Z, 4),
    SE = round(SE, 4),
    A = round(A, 1),
    ic95 = ic_95,
    commentaire = case_when(
      ajustement_hnp < 10 ~ "Bon ajustement",
      ajustement_hnp < 15 ~ "Ajustement marginal",
      TRUE ~ "Mauvais ajustement"
    ),
    convergence = model$fit$convergence == 0,
    nb_iterations_hnp = nb_iterations_hnp
  )
}
