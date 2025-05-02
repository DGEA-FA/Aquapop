#' Ajuster un modèle de CPUE de type GP (Generalized Poisson)
#'
#' Cette fonction ajuste un modèle de type Generalized Poisson (GP) avec `glmmTMB` sur les données de CPUE par station.
#' Elle applique également un test HNP (Half-Normal Plot) pour évaluer la qualité de l’ajustement.
#' En cas d’ajustement marginal (entre 10 % et 15 % d’observations hors bande), des simulations supplémentaires sont effectuées.
#'
#' @param cpue_data Un `data.frame` produit par `cpue_prepare()`, contenant au minimum les colonnes `no_station` et `CPUE`.
#'
#' @return Un `data.frame` d’une seule ligne résumant le modèle ajusté, avec les colonnes suivantes :
#' \describe{
#'   \item{methode}{Type de modèle utilisé (`"gp"`)}
#'   \item{ajustement_hnp}{Pourcentage moyen d’observations hors bande du test HNP}
#'   \item{aicc}{Critère d'information corrigé (AICc)}
#'   \item{cpue_moyenne}{Valeur moyenne prédite par le modèle (exponentielle du lien)}
#'   \item{ic_95}{Intervalle de confiance à 95 % sous forme de chaîne de caractères}
#'   \item{commentaire}{Texte interprétant l’ajustement : bon, marginal ou mauvais}
#'   \item{convergence}{État de convergence du modèle (`TRUE` ou `FALSE`)}
#'   \item{nb_iterations_hnp}{Nombre total d’itérations HNP effectuées (2 ou 5)}
#' }
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
  # 1. Ajustement du modèle GP
  model <- glmmTMB(CPUE ~ 1, family = genpois(link = "log"), data = cpue_data)
  
  # 2. Test HNP initial (2 simulations)
  message("Test HNP : Modèle GP (2 simulations initiales)...")
  set.seed(2023)
  nb_iter <- 2
  hnp_results <- replicate(
    2,
    hnp(
      model,
      newclass = TRUE,
      diagfun = residuals,
      simfun = function(n, obj) simulate(obj)[[1]],
      fitfun = function(y) {
        fit <- try(glmmTMB(y ~ 1, family = genpois(link = "log"), data = cpue_data), silent = TRUE)
        while (inherits(fit, "try-error")) {
          y <- simulate(model)[[1]]
          fit <- try(glmmTMB(y ~ 1, family = genpois(link = "log"), data = cpue_data), silent = TRUE)
        }
        return(fit)
      },
      how.many.out = TRUE,
      plot.sim = FALSE
    ),
    simplify = FALSE
  )
  
  hnp_out <- sapply(hnp_results, function(x) x$out / x$total * 100)
  ajustement <- mean(hnp_out) |> round(2)
  
  # 3. Refaire 3 simulations si ajustement marginal
  if (ajustement >= 10 && ajustement < 15) {
    message("Ajustement marginal : Ajout de 3 simulations HNP...")
    hnp_extra <- replicate(
      3,
      hnp(
        model,
        newclass = TRUE,
        diagfun = residuals,
        simfun = function(n, obj) simulate(obj)[[1]],
        fitfun = function(y) {
          fit <- try(glmmTMB(y ~ 1, family = genpois(link = "log"), data = cpue_data), silent = TRUE)
          while (inherits(fit, "try-error")) {
            y <- simulate(model)[[1]]
            fit <- try(glmmTMB(y ~ 1, family = genpois(link = "log"), data = cpue_data), silent = TRUE)
          }
          return(fit)
        },
        how.many.out = TRUE,
        plot.sim = FALSE
      ),
      simplify = FALSE
    )
    hnp_out_extra <- sapply(hnp_extra, function(x) x$out / x$total * 100)
    ajustement <- mean(c(hnp_out, hnp_out_extra)) |> round(2)
    nb_iter <- 5
  }
  
  # 4. Prédiction et IC
  pred <- predict(model, type = "link", se.fit = TRUE)
  fit_mean <- exp(pred$fit[1])
  ic95 <- paste0("(", round(exp(pred$fit[1] - 1.96 * pred$se.fit[1]), 2), "-",
                 round(exp(pred$fit[1] + 1.96 * pred$se.fit[1]), 2), ")")
  
  # 5. Commentaire
  commentaire <- case_when(
    ajustement < 10 ~ "Bon ajustement.",
    ajustement < 15 ~ "Ajustement marginal.",
    TRUE ~ "Mauvais ajustement."
  )
  
  # 6. Résultat final
  result <- tibble(
    methode = "gp",
    ajustement_hnp = ajustement,
    aicc = AICc(model),
    cpue_moyenne = round(fit_mean, 2),
    ic_95 = ic95,
    commentaire = commentaire,
    convergence = model$fit$convergence == 0,
    nb_iterations_hnp = nb_iter
  )
  
  return(result)
}
