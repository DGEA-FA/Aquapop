#' Ajuster un modèle de CPUE de type Poisson
#'
#' Cette fonction ajuste un modèle linéaire généralisé de type Poisson (GLM)
#' sur les données de CPUE par station. Elle effectue également un test HNP
#' (Half-Normal Plot) pour évaluer la qualité de l’ajustement.
#'
#' @param cpue_data Un `data.frame` produit par `prepare_cpue_data()` contenant au minimum :
#'                  `no_station`, `CPUE`.
#'
#' @return Un `data.frame` d’une ligne résumant le modèle ajusté, avec les colonnes :
#' \describe{
#'   \item{Méthode}{Nom du modèle ("Poisson")}
#'   \item{Ajustement (résultat du test HNP)}{Pourcentage moyen de résidus hors enveloppe}
#'   \item{AICc}{Critère AICc}
#'   \item{CPUE}{Valeur moyenne prédite}
#'   \item{IC 95%}{Intervalle de confiance autour de la prédiction}
#'   \item{Commentaires}{Interprétation de l’ajustement}
#'   \item{Convergence}{Booléen indiquant la convergence}
#'   \item{Nb_iterations_HNP}{Nombre total d’itérations HNP utilisées}
#' }
#'
#' @export
ajuster_modele_cpue_poisson <- function(cpue_data) {
  # 1. Ajustement du modèle
  model <- stats::glm(CPUE ~ 1, family = poisson, data = cpue_data)
  
  # 2. Test HNP initial
  message("Test HNP : Modèle Poisson (2 simulations initiales)...")
  set.seed(2023)
  hnp_results <- replicate(
    2,
    hnp::hnp(model, resid.type = "pearson", how.many.out = TRUE, plot.sim = FALSE),
    simplify = FALSE
  )
  hnp_out <- sapply(hnp_results, function(x) x$out / x$total * 100)
  ajustement <- mean(hnp_out) %>% round(2)
  nb_iter <- 2
  
  # 3. Répétitions supplémentaires si ajustement marginal
  if (ajustement >= 10 && ajustement < 15) {
    message("Ajustement marginal : Ajout de 3 simulations HNP...")
    hnp_extra <- replicate(
      3,
      hnp::hnp(model, resid.type = "pearson", how.many.out = TRUE, plot.sim = FALSE),
      simplify = FALSE
    )
    hnp_out_extra <- sapply(hnp_extra, function(x) x$out / x$total * 100)
    ajustement <- mean(c(hnp_out, hnp_out_extra)) %>% round(2)
    nb_iter <- 5
  }
  
  # 4. Prédictions
  pred <- stats::predict(model, type = "link", se.fit = TRUE)
  fit_mean <- exp(pred$fit[1])
  ic95 <- paste0("(", round(exp(pred$fit[1] - 1.96 * pred$se.fit[1]), 2), "-",
                 round(exp(pred$fit[1] + 1.96 * pred$se.fit[1]), 2), ")")
  
  # 5. Commentaire d’interprétation
  commentaire <- dplyr::case_when(
    ajustement < 10 ~ "Bon ajustement",
    ajustement < 15 ~ "Ajustement marginal",
    TRUE ~ "Mauvais ajustement"
  )
  
  # 6. Résultat (noms simples)
  result <- tibble::tibble(
    methode = "poisson",
    ajustement_hnp = ajustement,
    aicc = MuMIn::AICc(model),
    cpue_moyenne = round(fit_mean, 2),
    ic_95 = ic95,
    commentaire = commentaire,
    convergence = model$converged %||% TRUE,
    nb_iterations_hnp = nb_iter
  )
  
  return(result)
}

