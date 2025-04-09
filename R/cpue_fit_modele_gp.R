#' Ajuster un modèle de CPUE de type GP (Generalized Poisson)
#'
#' Cette fonction ajuste un modèle glmmTMB avec distribution Generalized Poisson (GP)
#' sur les données de CPUE par station. Elle applique aussi un test HNP
#' (Half-Normal Plot) pour évaluer la qualité de l’ajustement.
#'
#' @param cpue_data Un `data.frame` produit par `cpue_prepare()` contenant au minimum :
#'                  `no_station`, `CPUE`.
#'
#' @return Un `data.frame` d’une ligne résumant le modèle ajusté.
#' @export
cpue_fit_modele_gp <- function(cpue_data) {
  # 1. Ajustement du modèle GP
  model <- glmmTMB::glmmTMB(CPUE ~ 1, family = glmmTMB::genpois(link = "log"), data = cpue_data)
  
  # 2. Test HNP initial (2 simulations)
  message("Test HNP : Modèle GP (2 simulations initiales)...")
  set.seed(2023)
  nb_iter <- 2
  hnp_results <- replicate(
    2,
    hnp::hnp(
      model,
      newclass = TRUE,
      diagfun = stats::residuals,
      simfun = function(n, obj) stats::simulate(obj)[[1]],
      fitfun = function(y) {
        fit <- try(glmmTMB::glmmTMB(y ~ 1, family = glmmTMB::genpois(link = "log"), data = cpue_data), silent = TRUE)
        while (inherits(fit, "try-error")) {
          y <- stats::simulate(model)[[1]]
          fit <- try(glmmTMB::glmmTMB(y ~ 1, family = glmmTMB::genpois(link = "log"), data = cpue_data), silent = TRUE)
        }
        return(fit)
      },
      how.many.out = TRUE,
      plot.sim = FALSE
    ),
    simplify = FALSE
  )
  
  hnp_out <- sapply(hnp_results, function(x) x$out / x$total * 100)
  ajustement <- mean(hnp_out) %>% round(2)
  
  # 3. Refaire 3 simulations si ajustement marginal
  if (ajustement >= 10 && ajustement < 15) {
    message("Ajustement marginal : Ajout de 3 simulations HNP...")
    hnp_extra <- replicate(
      3,
      hnp::hnp(
        model,
        newclass = TRUE,
        diagfun = stats::residuals,
        simfun = function(n, obj) stats::simulate(obj)[[1]],
        fitfun = function(y) {
          fit <- try(glmmTMB::glmmTMB(y ~ 1, family = glmmTMB::genpois(link = "log"), data = cpue_data), silent = TRUE)
          while (inherits(fit, "try-error")) {
            y <- stats::simulate(model)[[1]]
            fit <- try(glmmTMB::glmmTMB(y ~ 1, family = glmmTMB::genpois(link = "log"), data = cpue_data), silent = TRUE)
          }
          return(fit)
        },
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
    methode = "gp",
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
