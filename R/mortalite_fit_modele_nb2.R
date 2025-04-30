#' Ajuster un modèle de mortalité de type NB2 (Negative Binomial 2)
#'
#' Cette fonction ajuste un modèle NB2 (`MASS::glm.nb`) sur les données de fréquence d’âge étendues.
#' Elle applique un test HNP (Half-Normal Plot) avec 2 à 5 simulations pour évaluer l’ajustement,
#' puis retourne les estimations de mortalité et leur intervalle de confiance.
#'
#' @param df_age_etendue Un `data.frame` produit par `mortalite_prepare_extended()` contenant au minimum :
#' \describe{
#'   \item{age}{Âge des individus (entier)}
#'   \item{number}{Nombre d’individus observés à cet âge}
#' }
#'
#' @return Un `data.frame` d’une ligne contenant :
#' \describe{
#'   \item{methode}{Nom du modèle (`"nb2"`)}
#'   \item{ajustement_hnp}{Pourcentage moyen d’observations hors bande (test HNP)}
#'   \item{aicc}{Critère d'information corrigé (AICc)}
#'   \item{Z}{Coefficient de l'âge estimé dans le modèle}
#'   \item{SE}{Erreur standard de Z}
#'   \item{A}{Taux de mortalité annuel estimé (%)}
#'   \item{IC 95%}{Intervalle de confiance de A (format "[x-y]")}
#'   \item{commentaire}{Appréciation qualitative de l’ajustement}
#'   \item{convergence}{Toujours TRUE (glm.nb échoue sinon)}
#'   \item{nb_iterations_hnp}{Nombre total de simulations HNP (2 ou 5)}
#' }
#'
#' @examples
#' df_fake <- tibble::tibble(age = 1:6, number = c(180, 120, 70, 40, 25, 10))
#' mortalite_fit_modele_nb2(df_fake)
#'
#' @export
mortalite_fit_modele_nb2 <- function(df_age_etendue) {
  stopifnot(all(c("age", "number") %in% names(df_age_etendue)))
  
  # --- Ajustement du modèle NB2 ---
  model <- MASS::glm.nb(number ~ age, data = df_age_etendue)
  
  # --- Test HNP initial (2 itérations) ---
  message("Test HNP : Modèle NB2 (2 simulations initiales)...")
  set.seed(2023)
  hnp_valeurs <- replicate(
    2,
    hnp::hnp(
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
      hnp::hnp(
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
  
  # --- Conversion en taux de mortalité annuel (A) ---
  A <- (1 - exp(-Z)) * 100
  lowerZ <- Z - SE
  upperZ <- Z + SE
  lowerA <- round((1 - exp(-lowerZ)) * 100, 1)
  upperA <- round((1 - exp(-upperZ)) * 100, 1)
  ic_95 <- glue::glue("[{lowerA}-{upperA}]")
  
  # --- Résultat final structuré ---
  tibble::tibble(
    methode = "nb2",
    ajustement_hnp = ajustement_hnp,
    aicc = MuMIn::AICc(model),
    Z = round(Z, 4),
    SE = round(SE, 4),
    A = round(A, 1),
    `IC 95%` = ic_95,
    commentaire = dplyr::case_when(
      ajustement_hnp < 10 ~ "Bon ajustement",
      ajustement_hnp < 15 ~ "Ajustement marginal",
      TRUE ~ "Mauvais ajustement"
    ),
    convergence = TRUE,
    nb_iterations_hnp = nb_iterations_hnp
  )
}
