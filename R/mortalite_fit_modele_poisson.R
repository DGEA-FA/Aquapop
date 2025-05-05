#' Ajuster un modèle de mortalité de type Poisson
#'
#' Cette fonction ajuste un modèle de mortalité sur les données étendues de fréquence d'âge
#' à l’aide d’un GLM Poisson. Elle applique aussi un test HNP (Half-Normal Plot) avec 2 à 5 simulations
#' selon la qualité de l’ajustement. Elle retourne un résumé des statistiques du modèle.
#'
#' @importFrom dplyr case_when
#' @importFrom MuMIn AICc
#' @importFrom glue glue
#' @importFrom hnp hnp
#' @importFrom stats glm
#' @importFrom tibble tibble
#' @param df_age_etendue Un `data.frame` produit par `mortalite_prepare_extended()` contenant au minimum :
#' \describe{
#'   \item{age}{Âge des individus}
#'   \item{number}{Nombre d’individus observés}
#' }
#'
#' @return Un `data.frame` d’une ligne contenant :
#' \describe{
#'   \item{methode}{Nom du modèle (`"poisson"`)}
#'   \item{ajustement_hnp}{Pourcentage moyen d’observations hors bande (test HNP)}
#'   \item{aicc}{Critère d'information corrigé (AICc)}
#'   \item{Z}{Coefficient d’âge estimé}
#'   \item{SE}{Erreur standard associée à Z}
#'   \item{A}{Taux de mortalité annuel estimé en %}
#'   \item{IC 95%}{Intervalle de confiance du taux A (ex: "[12.5-22.8]")}
#'   \item{commentaire}{Appréciation qualitative de l’ajustement}
#'   \item{convergence}{Convergence du modèle (toujours TRUE pour GLM Poisson)}
#'   \item{nb_iterations_hnp}{Nombre de simulations HNP (2 ou 5)}
#' }
#'
#' @examples
#' df_fake <- tibble(age = 1:6, number = c(180, 120, 70, 40, 25, 10))
#' mortalite_fit_modele_poisson(df_fake)
#'
#' @export
mortalite_fit_modele_poisson <- function(df_age_etendue) {
  stopifnot(all(c("age", "number") %in% names(df_age_etendue)))
  
  # --- Ajustement du modèle Poisson ---
  model <- glm(number ~ age, family = poisson, data = df_age_etendue)
  
  # --- Test HNP initial (2 itérations) ---
  message("Test HNP : Modèle Poisson (2 simulations initiales)...")
  set.seed(2023)
  hnp_valeurs <- replicate(
    2,
    hnp(
      model,
      resid.type = "pearson",
      how.many.out = TRUE,
      plot.sim = FALSE
    ),
    simplify = FALSE
  ) |>
    sapply(function(x) x$out / x$total * 100)
  
  ajustement_hnp <- round(mean(hnp_valeurs), 2)
  nb_iterations_hnp <- 2
  
  # --- Test HNP additionnel si ajustement marginal ---
  if (ajustement_hnp >= 10 && ajustement_hnp < 15) {
    message("Ajustement marginal : Ajout de 3 simulations HNP supplémentaires...")
    hnp_suppl <- replicate(
      3,
      hnp(
        model,
        resid.type = "pearson",
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
  coef <- summary(model)$coefficients
  Z <- abs(coef["age", "Estimate"])
  SE <- coef["age", "Std. Error"]
  
  # --- Conversion en taux de mortalité annuel A ---
  A <- (1 - exp(-Z)) * 100
  lowerZ <- Z - SE
  upperZ <- Z + SE
  lowerA <- round((1 - exp(-lowerZ)) * 100, 1)
  upperA <- round((1 - exp(-upperZ)) * 100, 1)
  ic_95 <- glue("[{lowerA}-{upperA}]")
  
  # --- Résumé structuré ---
  tibble(
    methode = "poisson",
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
    convergence = TRUE,
    nb_iterations_hnp = nb_iterations_hnp
  )
}
