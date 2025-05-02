#' Ajuster un modèle de CPUE de type Poisson
#'
#' Cette fonction ajuste un modèle linéaire généralisé (GLM) avec distribution de Poisson
#' sur les données de CPUE par station. Elle applique également un test HNP (Half-Normal Plot)
#' pour évaluer la qualité de l’ajustement. En cas d’ajustement marginal (entre 10 % et 15 % d’observations hors bande),
#' des simulations supplémentaires sont effectuées.
#'
#' @param cpue_data Un `data.frame` produit par `cpue_prepare()`, contenant au minimum les colonnes `no_station` et `CPUE`.
#'
#' @return Un `data.frame` d’une seule ligne résumant le modèle ajusté, avec les colonnes suivantes :
#' \describe{
#'   \item{methode}{Type de modèle utilisé (`"poisson"`)}
#'   \item{ajustement_hnp}{Pourcentage moyen d’observations hors bande du test HNP}
#'   \item{aicc}{Critère d'information corrigé (AICc)}
#'   \item{cpue_moyenne}{Valeur moyenne prédite par le modèle (exponentielle du lien)}
#'   \item{ic_95}{Intervalle de confiance à 95 % sous forme de chaîne de caractères}
#'   \item{commentaire}{Texte interprétant l’ajustement : bon, marginal ou mauvais}
#'   \item{convergence}{État de convergence (`TRUE` ou `FALSE`) selon le modèle}
#'   \item{nb_iterations_hnp}{Nombre total d’itérations HNP effectuées (2 ou 5)}
#' }
#'
#' @importFrom stats glm predict simulate residuals
#' @importFrom hnp hnp
#' @importFrom dplyr case_when
#' @importFrom tibble tibble
#' @importFrom MuMIn AICc
#'
#' @export
cpue_fit_modele_poisson <- function(cpue_data) {
  # 1. Ajustement du modèle
  model <- glm(CPUE ~ 1, family = poisson, data = cpue_data)
  
  # 2. Test HNP initial
  message("Test HNP : Modèle Poisson (2 simulations initiales)...")
  set.seed(2023)
  hnp_results <- replicate(
    2,
    hnp(model, resid.type = "pearson", how.many.out = TRUE, plot.sim = FALSE),
    simplify = FALSE
  )
  hnp_out <- sapply(hnp_results, function(x) x$out / x$total * 100)
  ajustement <- mean(hnp_out) |> round(2)
  nb_iter <- 2
  
  # 3. Répétitions supplémentaires si ajustement marginal
  if (ajustement >= 10 && ajustement < 15) {
    message("Ajustement marginal : Ajout de 3 simulations HNP...")
    hnp_extra <- replicate(
      3,
      hnp(model, resid.type = "pearson", how.many.out = TRUE, plot.sim = FALSE),
      simplify = FALSE
    )
    hnp_out_extra <- sapply(hnp_extra, function(x) x$out / x$total * 100)
    ajustement <- mean(c(hnp_out, hnp_out_extra)) |> round(2)
    nb_iter <- 5
  }
  
  # 4. Prédictions
  pred <- predict(model, type = "link", se.fit = TRUE)
  fit_mean <- exp(pred$fit[1])
  ic95 <- paste0("(", round(exp(pred$fit[1] - 1.96 * pred$se.fit[1]), 2), "-",
                 round(exp(pred$fit[1] + 1.96 * pred$se.fit[1]), 2), ")")
  
  # 5. Commentaire d’interprétation
  commentaire <- case_when(
    ajustement < 10 ~ "Bon ajustement.",
    ajustement < 15 ~ "Ajustement marginal.",
    TRUE ~ "Mauvais ajustement."
  )
  
  # 6. Résultat (noms simples)
  result <- tibble(
    methode = "poisson",
    ajustement_hnp = ajustement,
    aicc = AICc(model),
    cpue_moyenne = round(fit_mean, 2),
    ic_95 = ic95,
    commentaire = commentaire,
    convergence = model$converged %||% TRUE,
    nb_iterations_hnp = nb_iter
  )
  
  return(result)
}

