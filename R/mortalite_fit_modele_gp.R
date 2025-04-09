#' Ajuster un modèle de mortalité de type GP (Generalized Poisson)
#'
#' Cette fonction ajuste un modèle GP via `glmmTMB` sur les données de fréquence d'âge étendues.
#' Elle applique aussi un test HNP (Half-Normal Plot) pour évaluer la qualité de l'ajustement.
#'
#' @param df_age_etendue Un `data.frame` contenant les colonnes `age` et `number`, produit par `prepare_age_data_etendue()`.
#'
#' @return Un `data.frame` d’une ligne résumant le modèle ajusté.
#' @export
#'
#' @examples
#' mortalite_fit_modele_gp(df_age_etendue)
mortalite_fit_modele_gp <- function(df_age_etendue) {
  stopifnot(all(c("age", "number") %in% names(df_age_etendue)))
  
  # 1. Ajustement du modèle GP
  model <- glmmTMB::glmmTMB(
    number ~ age,
    family = glmmTMB::genpois(link = "log"),
    data = df_age_etendue
  )
  
  # 2. Test HNP initial (2 simulations)
  message("Test HNP : Modèle GP (2 simulations initiales)...")
  set.seed(2023)
  
  hnp_results <- replicate(
    2,
    hnp::hnp(
      model,
      newclass = TRUE,
      diagfun = function(obj) residuals(obj, type = "pearson"),
      simfun = function(n, obj) stats::simulate(obj)[[1]],
      fitfun = function(y) {
        fit <- try(glmmTMB::glmmTMB(
          y ~ age,
          family = glmmTMB::genpois(link = "log"),
          data = df_age_etendue
        ), silent = TRUE)
        while (inherits(fit, "try-error")) {
          y_retry <- stats::simulate(model)[[1]]
          fit <- try(glmmTMB::glmmTMB(
            y_retry ~ age,
            family = glmmTMB::genpois(link = "log"),
            data = df_age_etendue
          ), silent = TRUE)
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
    message("Ajustement marginal : Ajout de 3 simulations HNP supplémentaires...")
    hnp_extra <- replicate(
      3,
      hnp::hnp(
        model,
        newclass = TRUE,
        diagfun = function(obj) residuals(obj, type = "pearson"),
        simfun = function(n, obj) stats::simulate(obj)[[1]],
        fitfun = function(y) {
          fit <- try(glmmTMB::glmmTMB(
            y ~ age,
            family = glmmTMB::genpois(link = "log"),
            data = df_age_etendue
          ), silent = TRUE)
          while (inherits(fit, "try-error")) {
            y_retry <- stats::simulate(model)[[1]]
            fit <- try(glmmTMB::glmmTMB(
              y_retry ~ age,
              family = glmmTMB::genpois(link = "log"),
              data = df_age_etendue
            ), silent = TRUE)
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
  
  # 4. Extraction des coefficients
  coef <- summary(model)$coefficients$cond
  Z <- abs(coef["age", "Estimate"])
  SE <- coef["age", "Std. Error"]
  
  # 5. Conversion en mortalité annuelle A
  A <- (1 - exp(-Z)) * 100
  lowerZ <- Z - SE
  upperZ <- Z + SE
  lowerA <- round((1 - exp(-lowerZ)) * 100, 1)
  upperA <- round((1 - exp(-upperZ)) * 100, 1)
  ic_95 <- glue::glue("[{lowerA}-{upperA}]")
  
  # 6. Résultat structuré
  result <- tibble::tibble(
    methode = "gp",
    ajustement_hnp = ajustement,
    aicc = MuMIn::AICc(model),
    Z = round(Z, 4),
    SE = round(SE, 4),
    A = round(A, 1),
    `IC 95%` = ic_95,
    commentaire = dplyr::case_when(
      ajustement < 10 ~ "Bon ajustement",
      ajustement < 15 ~ "Ajustement marginal",
      TRUE ~ "Mauvais ajustement"
    ),
    convergence = model$fit$convergence == 0,
    nb_iterations_hnp = nb_iter
  )
  
  return(result)
}
