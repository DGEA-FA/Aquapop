#' Ajuster un modèle de CPUE de type CMP (Conway-Maxwell-Poisson)
#'
#' Cette fonction ajuste un modèle CMP via `glmmTMB` sur les données de CPUE par station.
#' Elle effectue également un test HNP pour évaluer la qualité de l’ajustement.
#'
#' @param cpue_data Un `data.frame` produit par `cpue_prepare()` contenant au minimum :
#'                  `no_station`, `CPUE`.
#'
#' @return Un `data.frame` d’une ligne résumant le modèle ajusté.
#' @export
#' #' @importFrom glmmTMB glmmTMB compois
#' @importFrom hnp hnp
#' @importFrom stats simulate residuals predict
#' @importFrom dplyr case_when
#' @importFrom tibble tibble
#' @importFrom MuMIn AICc
cpue_fit_modele_cmp <- function(cpue_data) {
  # 1. Ajustement du modèle CMP
  model <- glmmTMB::glmmTMB(CPUE ~ 1, family = glmmTMB::compois(link = "log"), data = cpue_data)
  
  # 2. Test HNP initial (2 itérations)
  message("Test HNP : Modèle CMP (2 simulations initiales)...")
  set.seed(2023)
  hnp_results <- replicate(
    2,
    hnp::hnp(
      model,
      newclass = TRUE,
      diagfun = function(obj) residuals(obj, type = "pearson"),
      simfun = function(n, obj) stats::simulate(obj)[[1]],
      fitfun = function(y) {
        fit <- try(glmmTMB::glmmTMB(y ~ 1, family = glmmTMB::compois(link = "log"), data = cpue_data), silent = TRUE)
        while (inherits(fit, "try-error")) {
          y_retry <- stats::simulate(model)[[1]]
          fit <- try(glmmTMB::glmmTMB(y_retry ~ 1, family = glmmTMB::compois(link = "log"), data = cpue_data), silent = TRUE)
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
  nb_iter <- 2
  
  # 3. Simulations supplémentaires si ajustement marginal
  if (ajustement >= 10 && ajustement < 15) {
    message("Ajustement marginal : Ajout de 3 simulations HNP...")
    hnp_extra <- replicate(
      3,
      hnp::hnp(
        model,
        newclass = TRUE,
        diagfun = function(obj) residuals(obj, type = "pearson"),
        simfun = function(n, obj) stats::simulate(obj)[[1]],
        fitfun = function(y) {
          fit <- try(glmmTMB::glmmTMB(y ~ 1, family = glmmTMB::compois(link = "log"), data = cpue_data), silent = TRUE)
          while (inherits(fit, "try-error")) {
            y_retry <- stats::simulate(model)[[1]]
            fit <- try(glmmTMB::glmmTMB(y_retry ~ 1, family = glmmTMB::compois(link = "log"), data = cpue_data), silent = TRUE)
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
    ajustement < 10 ~ "Bon ajustement.",
    ajustement < 15 ~ "Ajustement marginal.",
    TRUE ~ "Mauvais ajustement."
  )
  
  # 6. Résultat
  result <- tibble::tibble(
    methode = "cmp",
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
