#' Ajuster un modèle de CPUE de type CMP (Conway-Maxwell-Poisson)
#'
#' Cette fonction ajuste un modèle CMP via `glmmTMB` sur les données de CPUE par station.
#' Elle effectue également un test HNP pour évaluer la qualité de l’ajustement.
#'
#' @param cpue_data Un `data.frame` produit par `cpue_prepare()` contenant au minimum
#'   les colonnes `no_station` et `cpue`.
#'
#' @return Un `data.frame` d’une seule ligne résumant le modèle ajusté, avec les colonnes suivantes :
#' \describe{
#'   \item{methode}{Type de modèle utilisé (`"cmp"`)}
#'   \item{ajustement_hnp}{Pourcentage moyen d’observations hors bande du test HNP}
#'   \item{aicc}{Critère d'information corrigé (aicc)}
#'   \item{cpue_moyenne}{Valeur moyenne prédite par le modèle (exponentielle du lien)}
#'   \item{ic_95}{Intervalle de confiance à 95 % sous forme de chaîne de caractères}
#'   \item{commentaire}{Texte interprétant l’ajustement : bon, marginal ou mauvais}
#'   \item{convergence}{État de convergence du modèle (`TRUE` ou `FALSE`)}
#'   \item{nb_iterations_hnp}{Nombre total d’itérations HNP effectuées (2 ou 5)}
#' }
#'
#' @importFrom glmmTMB glmmTMB compois
#' @importFrom hnp hnp
#' @importFrom stats simulate residuals predict
#' @importFrom dplyr case_when
#' @importFrom tibble tibble
#' @importFrom MuMIn AICc
#' @export
cpue_fit_modele_cmp <- function(cpue_data) {
  
  # --- Ajustement du modèle CMP ---
  model <- try(
    glmmTMB(cpue ~ 1, family = compois(link = "log"), data = cpue_data),
    silent = TRUE
  )
  
  convergence_flag <- !inherits(model, "try-error") && model$fit$convergence == 0
  
  # --- Si le modèle n’a pas convergé : sortie neutralisée ---
  if (!convergence_flag) {
    return(
      tibble(
        methode = "cmp",
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
  
  # --- Fonction interne sécurisée pour réajustement ---
  safe_fit_cmp <- function(y) {
    fit <- try(
      glmmTMB(y ~ 1, family = compois(link = "log"), data = cpue_data),
      silent = TRUE
    )
    
    while (inherits(fit, "try-error")) {
      y <- simulate(model)[[1]]
      fit <- try(
        glmmTMB(y ~ 1, family = compois(link = "log"), data = cpue_data),
        silent = TRUE
      )
    }
    
    fit
  }
  
  # --- Test HNP initial ---
  message("Test HNP : Modèle CMP (2 simulations initiales)...")
  set.seed(2023)
  hnp_results <- replicate(
    2,
    hnp(
      model,
      newclass = TRUE,
      diagfun = function(obj) residuals(obj, type = "pearson"),
      simfun = function(n, obj) simulate(obj)[[1]],
      fitfun = safe_fit_cmp,
      how.many.out = TRUE,
      plot.sim = FALSE
    ),
    simplify = FALSE
  )
  hnp_out <- sapply(hnp_results, function(x) x$out / x$total * 100)
  ajustement <- mean(hnp_out) |> round(2)
  nb_iter <- 2
  
  # --- Simulations supplémentaires si ajustement marginal ---
  if (ajustement >= 10 && ajustement < 15) {
    message("Ajustement marginal : Ajout de 3 simulations HNP...")
    hnp_extra <- replicate(
      3,
      hnp(
        model,
        newclass = TRUE,
        diagfun = function(obj) residuals(obj, type = "pearson"),
        simfun = function(n, obj) simulate(obj)[[1]],
        fitfun = safe_fit_cmp,
        how.many.out = TRUE,
        plot.sim = FALSE
      ),
      simplify = FALSE
    )
    hnp_out_extra <- sapply(hnp_extra, function(x) x$out / x$total * 100)
    ajustement <- mean(c(hnp_out, hnp_out_extra)) |> round(2)
    nb_iter <- 5
  }
  
  # --- Prédiction et IC 95 % ---
  if (all(cpue_data$cpue == 0)) {
    fit_mean <- 0
    ic95 <- "IC non calculable"
  } else {
    pred <- predict(model, type = "link", se.fit = TRUE)
    fit_mean <- unname(exp(pred$fit[1]))
    ic95 <- paste0(
      "(",
      round(exp(pred$fit[1] - 1.96 * pred$se.fit[1]), 2),
      "-",
      round(exp(pred$fit[1] + 1.96 * pred$se.fit[1]), 2),
      ")"
    )
  }
  
  # --- Commentaire qualité ajustement ---
  commentaire <- case_when(
    ajustement < 10 ~ "Bon ajustement.",
    ajustement < 15 ~ "Ajustement marginal.",
    TRUE ~ "Mauvais ajustement."
  )
  
  # --- Résultat final ---
  tibble(
    methode = "cmp",
    ajustement_hnp = ajustement,
    aicc = AICc(model),
    cpue_moyenne = fit_mean,
    ic_95 = ic95,
    commentaire = commentaire,
    convergence = TRUE,
    nb_iterations_hnp = nb_iter
  )
}