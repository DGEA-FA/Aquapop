#' Ajuster un modèle de CPUE de type GP (Generalized Poisson)
#'
#' Cette fonction ajuste un modèle de type Generalized Poisson (GP) avec `glmmTMB` sur les données de cpue par station.
#' Elle applique également un test HNP (Half-Normal Plot) pour évaluer la qualité de l'ajustement.
#' En cas d'ajustement marginal (entre 10 % et 15 % d'observations hors bande), des simulations supplémentaires sont effectuées.
#'
#' @param cpue_data Un `data.frame` produit par `cpue_prepare()`, contenant au minimum :
#'   - `no_station` : identifiant de la station,
#'   - `cpue` : valeur de capture par unité d'effort.
#'
#' @return Un `data.frame` d'une ligne contenant :
#'   - `methode` : "gp"
#'   - `ajustement_hnp` : % moyen d'observations hors bande
#'   - `aicc` : aicc du modèle
#'   - `cpue_moyenne` : moyenne prédite sur l'échelle d'origine
#'   - `ic_95` : intervalle de confiance (ex. : "(1.2-2.3)")
#'   - `commentaire` : qualité de l'ajustement
#'   - `convergence` : état de convergence (`TRUE` ou `FALSE`)
#'   - `nb_iterations_hnp` : nombre total d'itérations HNP
#'
#' @importFrom glmmTMB glmmTMB genpois
#' @importFrom hnp hnp
#' @importFrom stats predict simulate residuals
#' @importFrom dplyr case_when
#' @importFrom tibble tibble
#' @importFrom MuMIn AICc
#'
#' @export
cpue_fit_modele_gp <- function(cpue_data) {
  
  # --- Ajustement du modèle GP ---
  model_gp <- glmmTMB(cpue ~ 1, family = genpois(link = "log"), data = cpue_data)
  
  convergence_flag <- model_gp$fit$convergence == 0
  
  # --- Si le modèle n'a pas convergé : sortie neutralisée ---
  if (!convergence_flag) {
    return(
      tibble(
        methode = "gp",
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
  
  # --- Fonction interne : réessaie glmmTMB en cas d'échec ---
  safe_fit_gp <- function(y) {
    fit <- try(glmmTMB(y ~ 1, family = genpois(link = "log"), data = cpue_data), silent = TRUE)
    while (inherits(fit, "try-error")) {
      y <- simulate(model_gp)[[1]]
      fit <- try(glmmTMB(y ~ 1, family = genpois(link = "log"), data = cpue_data), silent = TRUE)
    }
    fit
  }
  
  # --- Test HNP initial (2 itérations) ---
  message("Test HNP : Modèle GP (2 simulations initiales)...")
  set.seed(2023)
  hnp_list <- replicate(
    2,
    hnp(
      model_gp,
      newclass = TRUE,
      diagfun = residuals,
      simfun = function(n, obj) simulate(obj)[[1]],
      fitfun = safe_fit_gp,
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
        model_gp,
        newclass = TRUE,
        diagfun = residuals,
        simfun = function(n, obj) simulate(obj)[[1]],
        fitfun = safe_fit_gp,
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
    pred <- predict(model_gp, type = "link", se.fit = TRUE)
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
    methode = "gp",
    ajustement_hnp = perc_out,
    aicc = AICc(model_gp),
    cpue_moyenne = pred_mean,
    ic_95 = pred_ic95,
    commentaire = commentaire,
    convergence = TRUE,
    nb_iterations_hnp = nb_iter
  )
}