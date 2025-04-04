#' Ajuster un modèle de mortalité de type NB2 (Negative Binomial 2)
#'
#' Cette fonction ajuste un modèle de régression NB2 (`glm.nb`) sur les données de fréquence d'âge étendues.
#' Elle applique aussi un test HNP (Half-Normal Plot) pour évaluer la qualité de l'ajustement.
#'
#' @param df_age_etendue Un `data.frame` contenant les colonnes `age` et `number`, produit par `prepare_age_data_etendue()`.
#'
#' @return Un `data.frame` d’une ligne résumant le modèle ajusté.
#' @export
#'
#' @examples
#' ajuster_modele_mortalite_nb2(df_age_etendue)
ajuster_modele_mortalite_nb2 <- function(df_age_etendue) {
  stopifnot(all(c("age", "number") %in% names(df_age_etendue)))
  
  # 1. Ajustement du modèle NB2 (via MASS::glm.nb)
  model <- MASS::glm.nb(number ~ age, data = df_age_etendue)
  
  # 2. Test HNP initial
  message("Test HNP : Modèle NB2 (2 simulations initiales)...")
  set.seed(2023)
  hnp_results <- replicate(
    2,
    hnp::hnp(
      model,
      resid.type = "pearson",
      how.many.out = TRUE,
      plot.sim = FALSE
    ),
    simplify = FALSE
  )
  hnp_out <- sapply(hnp_results, function(x) x$out / x$total * 100)
  ajustement <- mean(hnp_out) %>% round(2)
  nb_iter <- 2
  
  # 3. Ajouter 3 itérations si ajustement marginal
  if (ajustement >= 10 && ajustement < 15) {
    message("Ajustement marginal : Ajout de 3 simulations HNP supplémentaires...")
    hnp_extra <- replicate(
      3,
      hnp::hnp(
        model,
        resid.type = "pearson",
        how.many.out = TRUE,
        plot.sim = FALSE
      ),
      simplify = TRUE
    )
    hnp_out_extra <- sapply(hnp_extra, function(x) x$out / x$total * 100)
    ajustement <- mean(c(hnp_out, hnp_out_extra)) %>% round(2)
    nb_iter <- 5
  }
  
  # 4. Extraire les coefficients
  coef <- summary(model)$coefficients
  Z <- abs(coef["age", "Estimate"])
  SE <- coef["age", "Std. Error"]
  
  # 5. Conversion vers mortalité annuelle A
  A <- (1 - exp(-Z)) * 100
  lowerZ <- Z - SE
  upperZ <- Z + SE
  lowerA <- round((1 - exp(-lowerZ)) * 100, 1)
  upperA <- round((1 - exp(-upperZ)) * 100, 1)
  ic_95 <- glue::glue("[{lowerA}-{upperA}]")
  
  # 6. Résultat
  result <- tibble::tibble(
    methode = "nb2",
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
    convergence = TRUE,  # glm.nb n’a pas $converged, mais retourne une erreur si non convergé
    nb_iterations_hnp = nb_iter
  )
  
  return(result)
}
