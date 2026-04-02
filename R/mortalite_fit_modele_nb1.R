#' Ajuster un modèle de mortalité de type NB1 (Negative Binomial 1)
#'
#' Cette fonction ajuste un modèle NB1 (`glmmTMB`) sur des données de fréquence d'âge étendues.
#' Elle applique un test HNP (Half-Normal Plot) avec jusqu'à 5 simulations pour évaluer la qualité de l'ajustement.
#' Elle retourne une ligne résumant le modèle, l'ajustement, et le taux de mortalité annuel estimé.
#'
#' @importFrom dplyr case_when
#' @importFrom MuMIn AICc
#' @importFrom glue glue
#' @importFrom stats simulate
#' @importFrom stats residuals
#' @importFrom hnp hnp
#' @importFrom glmmTMB nbinom1
#' @importFrom glmmTMB glmmTMB
#' @importFrom tibble tibble
#' @param df_age_etendue Un `data.frame` produit par `mortalite_prepare_extended()`,
#' contenant au minimum les colonnes :
#' \describe{
#'   \item{age}{Âge des individus (entier)}
#'   \item{number}{Nombre d'individus observés à cet âge}
#' }
#'
#' @return Un `data.frame` d'une ligne contenant :
#' \describe{
#'   \item{methode}{Type de modèle utilisé (`"nb1"`)}
#'   \item{ajustement_hnp}{Pourcentage moyen d'observations hors bande (test HNP)}
#'   \item{aicc}{Critère d'information corrigé (aicc)}
#'   \item{Z}{Coefficient estimé de l'âge dans le modèle}
#'   \item{SE}{Erreur standard associée à Z}
#'   \item{A}{Taux de mortalité annuel (%)}
#'   \item{IC 95%}{Intervalle de confiance du taux A}
#'   \item{commentaire}{Interprétation qualitative de l'ajustement}
#'   \item{convergence}{Booléen indiquant la convergence du modèle}
#'   \item{nb_iterations_hnp}{Nombre total de simulations HNP effectuées (2 ou 5)}
#' }
#'
#' @examples
#' df_fake <- tibble::tibble(age = 1:6, number = c(180, 120, 70, 40, 25, 10))
#' mortalite_fit_modele_nb1(df_fake)
#'
#' @export
mortalite_fit_modele_nb1 <- function(df_age_etendue) {
  stopifnot(all(c("age", "number") %in% names(df_age_etendue)))
  
  # --- Ajustement du modèle NB1 ---
  model <- glmmTMB(
    number ~ age,
    family = nbinom1(link = "log"),
    data = df_age_etendue
  )
  
  # --- Fonction interne : test HNP pour NB1 ---
  simuler_hnp_nb1 <- function(model, n_iter = 2) {
    replicate(
      n_iter,
      hnp(
        model,
        newclass = TRUE,
        diagfun = residuals,
        simfun = function(n, obj) simulate(obj)[[1]],
        fitfun = function(y) try(glmmTMB(
          y ~ age,
          family = nbinom1(),
          data = df_age_etendue
        )),
        how.many.out = TRUE,
        plot.sim = FALSE
      ),
      simplify = FALSE
    ) |>
      sapply(function(x) x$out / x$total * 100)
  }
  
  # --- Test HNP initial ---
  message("Test HNP : Modèle NB1 (2 simulations initiales)...")
  set.seed(2023)
  hnp_valeurs <- simuler_hnp_nb1(model, n_iter = 2)
  ajustement_hnp <- round(mean(hnp_valeurs), 2)
  nb_iterations_hnp <- 2
  
  # --- Test HNP additionnel si ajustement marginal ---
  if (ajustement_hnp >= 10 && ajustement_hnp < 15) {
    message("Ajustement marginal : Ajout de 3 simulations HNP supplémentaires...")
    hnp_valeurs_suppl <- simuler_hnp_nb1(model, n_iter = 3)
    hnp_valeurs <- c(hnp_valeurs, hnp_valeurs_suppl)
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
  
  # --- Résumé des résultats ---
  tibble(
    methode = "nb1",
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
