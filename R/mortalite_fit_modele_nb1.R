#' Ajuster un modèle de mortalité de type NB1 (Negative Binomial 1)
#'
#' Cette fonction ajuste un modèle glmmTMB avec distribution NB1 sur les données de fréquence d'âge étendues.
#' Elle effectue également un test HNP pour évaluer l'ajustement du modèle et retourne les valeurs de Z, A,
#' ainsi que l’IC 95% autour de A.
#'
#' @param df_age_etendue Un `data.frame` contenant les colonnes `age` et `number`, produit par `mortalite_prepare_extended()`.
#'
#' @return Un `data.frame` d’une ligne résumant le modèle ajusté.
#' @export
#'
#' @examples
#' mortalite_fit_modele_nb1(df_age_etendue)
mortalite_fit_modele_nb1 <- function(df_age_etendue) {
  stopifnot(all(c("age", "number") %in% names(df_age_etendue)))
  
  # 1. Ajustement du modèle NB1
  model <- glmmTMB::glmmTMB(number ~ age, family = glmmTMB::nbinom1(link = "log"), data = df_age_etendue)
  
  # 2. Test HNP initial (2 simulations)
  message("Test HNP : Modèle NB1 (2 simulations initiales)...")
  set.seed(2023)
  hnp_results <- replicate(
    2,
    hnp::hnp(
      model,
      newclass = TRUE,
      diagfun = stats::residuals,
      simfun = function(n, obj) stats::simulate(obj)[[1]],
      fitfun = function(y) try(glmmTMB::glmmTMB(y ~ age, family = glmmTMB::nbinom1(), data = df_age_etendue)),
      how.many.out = TRUE,
      plot.sim = FALSE
    ),
    simplify = FALSE
  )
  hnp_out <- sapply(hnp_results, function(x) x$out / x$total * 100)
  ajustement <- mean(hnp_out) %>% round(2)
  nb_iter <- 2
  
  # 3. Si ajustement marginal, ajouter 3 itérations
  if (ajustement >= 10 && ajustement < 15) {
    message("Ajustement marginal : Ajout de 3 simulations HNP supplémentaires...")
    hnp_extra <- replicate(
      3,
      hnp::hnp(
        model,
        newclass = TRUE,
        diagfun = stats::residuals,
        simfun = function(n, obj) stats::simulate(obj)[[1]],
        fitfun = function(y) try(glmmTMB::glmmTMB(y ~ age, family = glmmTMB::nbinom1(), data = df_age_etendue)),
        how.many.out = TRUE,
        plot.sim = FALSE
      ),
      simplify = TRUE
    )
    hnp_out_extra <- sapply(hnp_extra, function(x) x$out / x$total * 100)
    ajustement <- mean(c(hnp_out, hnp_out_extra)) %>% round(2)
    nb_iter <- 5
  }
  
  # 4. Extraction des coefficients
  coef <- summary(model)$coefficients$cond
  Z <- abs(coef["age", "Estimate"])
  SE <- coef["age", "Std. Error"]
  
  # 5. Conversion vers taux de mortalité annuel A
  A <- (1 - exp(-Z)) * 100
  lowerZ <- Z - SE
  upperZ <- Z + SE
  lowerA <- round((1 - exp(-lowerZ)) * 100, 1)
  upperA <- round((1 - exp(-upperZ)) * 100, 1)
  ic_95 <- glue::glue("[{lowerA}-{upperA}]")
  
  # 6. Résumé dans un tibble
  result <- tibble::tibble(
    methode = "nb1",
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
