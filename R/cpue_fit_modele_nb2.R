#' Ajuster un modèle de CPUE de type NB2 (Negative Binomial 2)
#'
#' Cette fonction ajuste un modèle NB2 via `glm.nb()` sur les données de CPUE par station.
#' Elle applique un test HNP (Half-Normal Plot) pour évaluer la qualité de l'ajustement.
#' Si l'ajustement est marginal (entre 10 % et 15 % d'observations hors bande),
#' trois simulations supplémentaires sont effectuées. Elle retourne un résumé synthétique.
#'
#' @param cpue_data Un `data.frame` produit par `cpue_prepare()` contenant au minimum :
#'   - `no_station` : identifiant de la station,
#'   - `cpue` : valeur de capture par unité d'effort.
#'
#' @return Un `data.frame` d'une ligne contenant :
#'   - `methode` : "nb2"
#'   - `ajustement_hnp` : % moyen d'observations hors bande
#'   - `aicc` : AICc du modèle
#'   - `cpue_moyenne` : moyenne prédite sur l'échelle d'origine
#'   - `ic_95` : intervalle de confiance (ex. : "(1.2-2.3)")
#'   - `commentaire` : qualité de l'ajustement
#'   - `convergence` : booléen indiquant si le modèle a convergé
#'   - `nb_iterations_hnp` : nombre total d'itérations HNP
#'
#' @examples
#' set.seed(1)
#' fake_data <- tibble::tibble(no_station = 1:10, cpue = stats::rnbinom(10, mu = 5, size = 1))
#' cpue_fit_modele_nb2(fake_data)
#'
#' @importFrom MASS glm.nb
#' @importFrom stats predict simulate residuals rnbinom
#' @importFrom hnp hnp
#' @importFrom dplyr case_when
#' @importFrom tibble tibble
#' @importFrom MuMIn AICc
#'
#' @export
cpue_fit_modele_nb2 <- function(cpue_data) {
  
  # --- Fonction interne : test HNP NB2 ---
  simuler_hnp_nb2 <- function(model_nb2, n_iter = 2) {
    replicate(
      n_iter,
      hnp(
        model_nb2,
        resid.type = "pearson",
        how.many.out = TRUE,
        plot.sim = FALSE
      ),
      simplify = FALSE
    ) |> sapply(function(x) x$out / x$total * 100)
  }
  
  # --- Ajustement du modèle NB2 ---
  model_nb2 <- try(MASS::glm.nb(cpue ~ 1, data = cpue_data), silent = TRUE)
  
  convergence_flag <- !inherits(model_nb2, "try-error")
  
  # --- Si le modèle n’a pas convergé : sortie neutralisée ---
  if (!convergence_flag) {
    return(
      tibble(
        methode = "nb2",
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
  message("Test HNP : Modèle NB2 (2 simulations initiales)...")
  set.seed(2023)
  hnp_perc <- simuler_hnp_nb2(model_nb2, n_iter = 2)
  perc_out <- round(mean(hnp_perc), 2)
  nb_iter <- 2
  
  # --- Simulations supplémentaires si ajustement marginal ---
  if (perc_out >= 10 && perc_out < 15) {
    message("Ajustement marginal : Ajout de 3 simulations HNP supplémentaires...")
    hnp_extra <- simuler_hnp_nb2(model_nb2, n_iter = 3)
    hnp_perc <- c(hnp_perc, hnp_extra)
    perc_out <- round(mean(hnp_perc), 2)
    nb_iter <- 5
  }
  
  # --- Prédictions et intervalle de confiance ---
  if (all(cpue_data$cpue == 0)) {
    pred_mean <- 0
    pred_ic95 <- "IC non calculable"
  } else {
    pred <- predict(model_nb2, type = "link", se.fit = TRUE)
    pred_mean <- unname(round(exp(pred$fit[1]), 2))
    ic_low <- round(exp(pred$fit[1] - 1.96 * pred$se.fit[1]), 2)
    ic_up  <- round(exp(pred$fit[1] + 1.96 * pred$se.fit[1]), 2)
    pred_ic95 <- sprintf("(%s-%s)", ic_low, ic_up)
  }
  
  # --- Commentaire sur l'ajustement ---
  commentaire <- case_when(
    perc_out < 10 ~ "Bon ajustement.",
    perc_out < 15 ~ "Ajustement marginal.",
    TRUE ~ "Mauvais ajustement."
  )
  
  # --- Résultat final ---
  tibble(
    methode = "nb2",
    ajustement_hnp = perc_out,
    aicc = AICc(model_nb2),
    cpue_moyenne = pred_mean,
    ic_95 = pred_ic95,
    commentaire = commentaire,
    convergence = TRUE,
    nb_iterations_hnp = nb_iter
  )
}