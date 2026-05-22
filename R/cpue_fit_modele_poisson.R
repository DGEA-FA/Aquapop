#' Ajuster un modèle de CPUE de type Poisson
#'
#' Cette fonction ajuste un modèle linéaire généralisé (GLM) avec distribution de Poisson
#' sur les données de CPUE par station. Elle applique un test HNP (Half-Normal Plot)
#' pour évaluer la qualité de l'ajustement. En cas d'ajustement marginal (entre 10 % et 15 % d'observations hors bande),
#' trois simulations supplémentaires sont effectuées.
#'
#' @param cpue_data Un `data.frame` produit par `cpue_prepare()` contenant au minimum :
#'   - `no_station` : identifiant de la station,
#'   - `cpue` : valeur de capture par unité d'effort.
#'
#' @return Un `data.frame` d'une ligne contenant :
#'   - `methode` : "poisson"
#'   - `ajustement_hnp` : % moyen d'observations hors bande
#'   - `aicc` : AICc du modèle
#'   - `cpue_moyenne` : moyenne prédite sur l'échelle d'origine
#'   - `ic_95` : intervalle de confiance (ex. : "(1.2-2.3)")
#'   - `commentaire` : qualité de l'ajustement
#'   - `convergence` : état de convergence (`TRUE` ou `FALSE`)
#'   - `nb_iterations_hnp` : nombre total d'itérations HNP
#'
#' @examples
#' set.seed(1)
#' fake_data <- tibble::tibble(no_station = 1:10, cpue = stats::rpois(10, lambda = 5))
#' cpue_fit_modele_poisson(fake_data)
#'
#' @importFrom stats glm predict simulate residuals
#' @importFrom hnp hnp
#' @importFrom dplyr case_when n_distinct
#' @importFrom tibble tibble
#' @importFrom MuMIn AICc
#'
#' @export
cpue_fit_modele_poisson <- function(cpue_data) {
  
  # --- Ajustement du modèle Poisson ---
  model_poisson <- try(
    glm(cpue ~ 1, family = poisson, data = cpue_data),
    silent = TRUE
  )
  
  convergence_flag <- !inherits(model_poisson, "try-error") &&
    isTRUE(model_poisson$converged %||% TRUE)
  
  # --- Si le modèle n'a pas convergé : sortie neutralisée ---
  if (!convergence_flag) {
    return(
      tibble(
        methode = "poisson",
        ajustement_hnp = NA_real_,
        aicc = NA_real_,
        cpue_moyenne = NA_real_,
        ic_95 = NA_character_,
        commentaire = "Le modèle n'a pas convergé.",
        convergence = FALSE,
        nb_iterations_hnp = NA_real_
      )
    )
  }
  
  # --- Si une seule station : HNP non applicable ---
  if (n_distinct(cpue_data$no_station) < 2) {
    pred_mean <- unname(round(mean(cpue_data$cpue, na.rm = TRUE), 2))
    
    return(
      tibble(
        methode = "poisson",
        ajustement_hnp = NA_real_,
        aicc = NA_real_,
        cpue_moyenne = pred_mean,
        ic_95 = "IC non calculable",
        commentaire = "Test HNP non applicable : une seule station disponible.",
        convergence = TRUE,
        nb_iterations_hnp = 0
      )
    )
  }
  
  
  # --- Test HNP initial (2 itérations) ---
  message("Test HNP : Modèle Poisson (2 simulations initiales)...")
  set.seed(2023)
  hnp_list <- replicate(
    2,
    hnp(model_poisson, resid.type = "pearson", how.many.out = TRUE, plot.sim = FALSE),
    simplify = FALSE
  )
  hnp_perc <- sapply(hnp_list, function(x) x$out / x$total * 100)
  perc_out <- round(mean(hnp_perc), 2)
  nb_iter <- 2
  
  # --- Simulations supplémentaires si ajustement marginal ---
  if (perc_out >= 10 && perc_out < 15) {
    message("Ajustement marginal : Ajout de 3 simulations HNP...")
    hnp_extra <- replicate(
      3,
      hnp(model_poisson, resid.type = "pearson", how.many.out = TRUE, plot.sim = FALSE),
      simplify = FALSE
    )
    hnp_extra_perc <- sapply(hnp_extra, function(x) x$out / x$total * 100)
    perc_out <- round(mean(c(hnp_perc, hnp_extra_perc)), 2)
    nb_iter <- 5
  }
  
  # --- Prédictions et intervalle de confiance ---
  pred <- predict(model_poisson, type = "link", se.fit = TRUE)
  pred_mean <- unname(round(exp(pred$fit[1]), 2))
  
  if (all(cpue_data$cpue == 0)) {
    pred_ic95 <- "IC non calculable"
  } else {
    lower_num <- exp(pred$fit[1] - 1.96 * pred$se.fit[1])
    upper_num <- exp(pred$fit[1] + 1.96 * pred$se.fit[1])
    
    pred_ic95 <- paste0(
      "[",
      format_num_fr(lower_num, digits = 2),
      " – ",
      format_num_fr(upper_num, digits = 2),
      "]"
    )
  }
  
  # --- Commentaire sur l'ajustement ---
  commentaire <- case_when(
    perc_out < 10 ~ "Bon ajustement.",
    perc_out < 15 ~ "Ajustement marginal.",
    TRUE ~ "Mauvais ajustement."
  )
  
  # --- Résultat final ---
  tibble(
    methode = "poisson",
    ajustement_hnp = perc_out,
    aicc = AICc(model_poisson),
    cpue_moyenne = pred_mean,
    ic_95 = pred_ic95,
    commentaire = commentaire,
    convergence = TRUE,
    nb_iterations_hnp = nb_iter
  )
}