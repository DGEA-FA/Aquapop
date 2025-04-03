#' Ajuster un modèle de CPUE de type NB1 (Negative Binomial 1)
#'
#' Cette fonction ajuste un modèle glmmTMB avec distribution NB1 sur les données de CPUE par station.
#' Elle réalise aussi un test HNP (Half-Normal Plot) pour vérifier l’ajustement du modèle.
#'
#' @param cpue_data Un `data.frame` produit par `prepare_cpue_data()` contenant au minimum :
#'                  `no_station`, `CPUE`.
#'
#' @return Un `data.frame` d’une ligne résumant le modèle ajusté.
#' @export
ajuster_modele_cpue_nb1 <- function(cpue_data) {
  # 1. Ajustement du modèle NB1
  model <- glmmTMB::glmmTMB(CPUE ~ 1, family = glmmTMB::nbinom1(), data = cpue_data)
  
  # 2. Test HNP initial (2 itérations)
  message("Test HNP : Modèle NB1 (2 simulations initiales)...")
  set.seed(2023)
  hnp_results <- replicate(
    2,
    hnp::hnp(
      model,
      newclass = TRUE,
      diagfun = stats::residuals,
      simfun = function(n, obj) stats::simulate(obj)[[1]],
      fitfun = function(y) try(glmmTMB::glmmTMB(y ~ 1, family = glmmTMB::nbinom1(), data = cpue_data)),
      how.many.out = TRUE,
      plot.sim = FALSE
    ),
    simplify = FALSE
  )
  hnp_out <- sapply(hnp_results, function(x) x$out / x$total * 100)
  ajustement <- mean(hnp_out) %>% round(2)
  nb_iter <- 2
  
  # 3. Ajout de simulations si ajustement marginal
  if (ajustement >= 10 && ajustement < 15) {
    message("Ajustement marginal : Ajout de 3 simulations HNP...")
    hnp_extra <- replicate(
      3,
      hnp::hnp(
        model,
        newclass = TRUE,
        diagfun = stats::residuals,
        simfun = function(n, obj) stats::simulate(obj)[[1]],
        fitfun = function(y) try(glmmTMB::glmmTMB(y ~ 1, family = glmmTMB::nbinom1(), data = cpue_data)),
        how.many.out = TRUE,
        plot.sim = FALSE
      ),
      simplify = FALSE
    )
    hnp_out_extra <- sapply(hnp_extra, function(x) x$out / x$total * 100)
    ajustement <- mean(c(hnp_out, hnp_out_extra)) %>% round(2)
    nb_iter <- 5
  }
  
  # 4. Prédiction et IC
  pred <- stats::predict(model, type = "link", se.fit = TRUE)
  fit_mean <- exp(pred$fit[1])
  ic95 <- paste0("(", round(exp(pred$fit[1] - 1.96 * pred$se.fit[1]), 2), "-",
                 round(exp(pred$fit[1] + 1.96 * pred$se.fit[1]), 2), ")")
  
  # 5. Commentaire
  commentaire <- dplyr::case_when(
    ajustement < 10 ~ "Bon ajustement",
    ajustement < 15 ~ "Ajustement marginal",
    TRUE ~ "Mauvais ajustement"
  )
  
  # 6. Résultat final
  result <- tibble::tibble(
    methode = "nb1",
    ajustement_hnp = ajustement,
    aicc = MuMIn::AICc(model),
    cpue_moyenne = round(fit_mean, 2),
    ic_95 = ic95,
    commentaire = commentaire,
    convergence = model$fit$convergence == 0,
    nb_iterations_hnp = nb_iter
  )
  
  return(result)
}
