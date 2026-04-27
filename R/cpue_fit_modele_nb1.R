#' Ajuster un modèle de CPUE de type NB1 (Negative Binomial 1)
#'
#' Cette fonction ajuste un modèle NB1 via `glmmTMB` sur les données de CPUE par station.
#' Elle applique un test HNP (Half-Normal Plot) pour évaluer la qualité de l'ajustement.
#' Si l'ajustement est marginal (entre 10 % et 15 % d'observations hors bande),
#' trois simulations supplémentaires sont effectuées. Elle retourne un résumé synthétique.
#'
#' @param cpue_data Un `data.frame` produit par `cpue_prepare()` contenant au minimum :
#'   - `no_station` : identifiant de la station,
#'   - `cpue` : valeur de capture par unité d'effort.
#'
#' @return Un `data.frame` d'une ligne contenant :
#'   - `methode` : "nb1"
#'   - `ajustement_hnp` : % moyen d'observations hors bande
#'   - `aicc` : aicc du modèle
#'   - `cpue_moyenne` : moyenne prédite sur l'échelle d'origine
#'   - `ic_95` : intervalle de confiance (ex. : "(1.2-2.3)")
#'   - `commentaire` : qualité de l'ajustement
#'   - `convergence` : booléen sur la convergence
#'   - `nb_iterations_hnp` : nombre total d'itérations HNP
#'
#' @examples
#' set.seed(1)
#' d <- data.frame(no_station = 1:30, cpue = rnbinom(30, mu = 4, size = 2))
#' cpue_fit_modele_nb1(d)
#'
#' @importFrom glmmTMB glmmTMB nbinom1
#' @importFrom hnp hnp
#' @importFrom stats predict simulate residuals
#' @importFrom dplyr case_when
#' @importFrom tibble tibble
#' @importFrom MuMIn AICc
#' @export
cpue_fit_modele_nb1 <- function(cpue_data) {
  
  # --- Ajustement du modèle NB1 ---
  model_nb1 <- glmmTMB(cpue ~ 1, family = nbinom1(), data = cpue_data)
  
  convergence_flag <- model_nb1$fit$convergence == 0
  
  # --- Si le modèle n'a pas convergé : sortie neutralisée ---
  if (!convergence_flag) {
    return(
      tibble(
        methode = "nb1",
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
  
  # --- Test HNP initial (2 itérations) ---
  message("Test HNP : Modèle NB1 (2 simulations initiales)...")
  set.seed(2023)
  hnp_list <- replicate(
    2,
    hnp(
      model_nb1,
      newclass = TRUE,
      diagfun = residuals,
      simfun = function(n, obj) simulate(obj)[[1]],
      fitfun = function(y) try(glmmTMB(y ~ 1, family = nbinom1(), data = cpue_data)),
      how.many.out = TRUE,
      plot.sim = FALSE
    ),
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
      hnp(
        model_nb1,
        newclass = TRUE,
        diagfun = residuals,
        simfun = function(n, obj) simulate(obj)[[1]],
        fitfun = function(y) try(glmmTMB(y ~ 1, family = nbinom1(), data = cpue_data)),
        how.many.out = TRUE,
        plot.sim = FALSE
      ),
      simplify = FALSE
    )
    hnp_extra_perc <- sapply(hnp_extra, function(x) x$out / x$total * 100)
    perc_out <- round(mean(c(hnp_perc, hnp_extra_perc)), 2)
    nb_iter <- 5
  }
  
  # --- Prédictions et intervalle de confiance ---
  if (all(cpue_data$cpue == 0)) {
    pred_mean <- 0
    pred_ic95 <- "IC non calculable"
  } else {
    pred <- predict(model_nb1, type = "link", se.fit = TRUE)
    pred_mean <- unname(exp(pred$fit[1]))
    
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
    methode = "nb1",
    ajustement_hnp = perc_out,
    aicc = AICc(model_nb1),
    cpue_moyenne = pred_mean,
    ic_95 = pred_ic95,
    commentaire = commentaire,
    convergence = TRUE,
    nb_iterations_hnp = nb_iter
  )
}