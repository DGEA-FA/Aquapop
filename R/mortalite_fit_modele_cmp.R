#' Ajuster un modèle de mortalité de type CMP (Conway-Maxwell-Poisson)
#'
#' Cette fonction ajuste un modèle CMP via `glmmTMB` sur les données de fréquence d'âge étendues.
#' Elle applique aussi un test HNP (Half-Normal Plot) pour évaluer la qualité de l'ajustement.
#'
#' @param df_age_etendue Un `data.frame` contenant les colonnes `age` et `number`, produit par `mortalite_prepare_extended()`.
#'
#' @return Un `data.frame` d’une ligne résumant le modèle ajusté.
#' @export
#'
#' @examples
#' mortalite_fit_modele_cmp(df_age_etendue)
mortalite_fit_modele_cmp <- function(df_age_etendue) {
  stopifnot(all(c("age", "number") %in% names(df_age_etendue)))
  
  # 1. Ajustement initial du modèle CMP
  model <- glmmTMB::glmmTMB(
    number ~ age,
    family = glmmTMB::compois(link = "log"),
    data = df_age_etendue
  )
  
  # 2. Définir les fonctions pour HNP
  diagfun <- function(obj) residuals(obj, type = "pearson")
  simfun <- function(n, obj) simulate(obj)[[1]]
  fitfun <- function(y) {
    fit <- try(glmmTMB::glmmTMB(
      y ~ age,
      family = glmmTMB::nbinom1(link = "log"),
      data = df_age_etendue
    ), silent = TRUE)
    
    while (inherits(fit, "try-error")) {
      y_retry <- simulate(model)[[1]]
      fit <- try(glmmTMB::glmmTMB(
        y_retry ~ age,
        family = glmmTMB::nbinom1(link = "log"),
        data = df_age_etendue
      ), silent = TRUE)
    }
    
    return(fit)
  }
  
  # 3. HNP avec 2 simulations
  message("Test HNP : Modèle CMP (2 simulations initiales)...")
  set.seed(2023)
  hnp_list <- list()
  for (i in 1:2) {
    hnp_list[[i]] <- hnp::hnp(
      model,
      newclass = TRUE,
      diagfun = diagfun,
      simfun = simfun,
      fitfun = fitfun,
      how.many.out = TRUE,
      plot.sim = FALSE
    )
  }
  
  hnp_out <- sapply(hnp_list, function(x) x$out / x$total * 100)
  ajustement <- round(mean(hnp_out), 2)
  nb_iter <- 2
  
  # 4. Si ajustement marginal, 3 itérations de plus
  if (ajustement >= 10 && ajustement < 15) {
    message("Ajustement marginal : Ajout de 3 simulations HNP supplémentaires...")
    for (i in 1:3) {
      hnp_list[[length(hnp_list) + 1]] <- hnp::hnp(
        model,
        newclass = TRUE,
        diagfun = diagfun,
        simfun = simfun,
        fitfun = fitfun,
        how.many.out = TRUE,
        plot.sim = FALSE
      )
    }
    hnp_out_all <- sapply(hnp_list, function(x) x$out / x$total * 100)
    ajustement <- round(mean(hnp_out_all), 2)
    nb_iter <- 5
  }
  
  # 5. Extraction des coefficients
  coef <- summary(model)$coefficients$cond
  Z <- abs(coef["age", "Estimate"])
  SE <- coef["age", "Std. Error"]
  
  # 6. Conversion vers mortalité annuelle
  A <- (1 - exp(-Z)) * 100
  lowerZ <- Z - SE
  upperZ <- Z + SE
  lowerA <- round((1 - exp(-lowerZ)) * 100, 1)
  upperA <- round((1 - exp(-upperZ)) * 100, 1)
  ic_95 <- glue::glue("[{lowerA}-{upperA}]")
  
  # 7. Résumé
  result <- tibble::tibble(
    methode = "cmp",
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
