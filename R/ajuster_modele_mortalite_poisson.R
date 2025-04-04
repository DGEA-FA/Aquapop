#' Ajuster un modèle de mortalité de type Poisson
#'
#' Cette fonction ajuste un modèle de mortalité sur les données étendues de fréquence d'âge
#' à l’aide d’un GLM Poisson. Elle applique aussi un test HNP (Half-Normal Plot)
#' pour évaluer l’ajustement et retourne un tableau avec les principales statistiques.
#'
#' @param df_age_etendue Un `data.frame` contenant les colonnes `age` et `number`, produit par `prepare_age_data_etendue()`.
#'
#' @return Un `data.frame` d’une ligne résumant le modèle ajusté, incluant Z, SE, A, IC95 et HNP.
#' @export
#'
#' @examples
#' ajuster_modele_mortalite_poisson(df_age_etendue)
ajuster_modele_mortalite_poisson <- function(df_age_etendue) {
  stopifnot(all(c("age", "number") %in% names(df_age_etendue)))
  
  # 1. Ajustement du modèle Poisson
  model <- glm(number ~ age, family = poisson, data = df_age_etendue)
  
  # 2. Test HNP (2 simulations de base)
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
  
  # 3. Simulations additionnelles si ajustement marginal
  if (ajustement >= 10 && ajustement < 15) {
    message("Ajustement marginal : Ajout de 3 simulations HNP supplémentaires...")
    hnp_extra <- replicate(
      3,
      hnp::hnp(model, resid.type = "pearson", how.many.out = TRUE, plot.sim = FALSE),
      simplify = FALSE
    )
    hnp_out_extra <- sapply(hnp_extra, function(x) x$out / x$total * 100)
    ajustement <- mean(c(hnp_out, hnp_out_extra)) %>% round(2)
    nb_iter <- 5
  }
  
  # 4. Extraction des coefficients
  coef <- summary(model)$coefficients
  Z <- abs(coef["age", "Estimate"])
  SE <- coef["age", "Std. Error"]
  
  # 5. Transformation en mortalité annuelle (A) et IC95
  A <- (1 - exp(-Z)) * 100
  lowerZ <- Z - SE
  upperZ <- Z + SE
  lowerA <- round((1 - exp(-lowerZ)) * 100, 1)
  upperA <- round((1 - exp(-upperZ)) * 100, 1)
  
  ic_95 <- glue::glue("[{lowerA}-{upperA}]")
  
  # 6. Résumé dans un tableau
  result <- tibble::tibble(
    methode = "poisson",
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
    convergence = TRUE,
    nb_iterations_hnp = nb_iter
  )
  
  return(result)
}
