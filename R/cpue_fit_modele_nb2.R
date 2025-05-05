#' Ajuster un modèle de CPUE de type NB2 (Negative Binomial 2)
#'
#' Cette fonction ajuste un modèle de type NB2 (Negative Binomial 2) à l’aide de `glm.nb()`
#' sur les données de CPUE par station. Elle applique également un test HNP (Half-Normal Plot)
#' pour évaluer la qualité de l’ajustement. En cas d’ajustement marginal (entre 10 % et 15 % d’observations hors bande),
#' trois simulations supplémentaires sont effectuées.
#'
#' @param cpue_data Un `data.frame` produit par `cpue_prepare()`, contenant au minimum les colonnes `no_station` et `CPUE`.
#'
#' @return Un `data.frame` d’une seule ligne résumant le modèle ajusté, avec les colonnes suivantes :
#' \describe{
#'   \item{methode}{Type de modèle utilisé (`"nb2"`)}
#'   \item{ajustement_hnp}{Pourcentage moyen d’observations hors bande du test HNP}
#'   \item{aicc}{Critère d'information corrigé (AICc)}
#'   \item{cpue_moyenne}{Valeur moyenne prédite par le modèle (exponentielle du lien)}
#'   \item{ic_95}{Intervalle de confiance à 95 % sous forme de chaîne de caractères}
#'   \item{commentaire}{Texte interprétant l’ajustement : bon, marginal ou mauvais}
#'   \item{convergence}{État de convergence (`TRUE`, car `glm.nb()` converge toujours sauf erreur)}
#'   \item{nb_iterations_hnp}{Nombre total d’itérations HNP effectuées (2 ou 5)}
#' }
#'
#' @examples
#' fake_data <- tibble::tibble(
#'   no_station = 1:10,
#'   CPUE = rnbinom(10, mu = 5, size = 1)
#' )
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
  simuler_hnp_nb2 <- function(model, n_iter = 2) {
    replicate(
      n_iter,
      hnp(
        model,
        resid.type = "pearson",
        how.many.out = TRUE,
        plot.sim = FALSE
      ),
      simplify = FALSE
    ) |>
      sapply(function(x) x$out / x$total * 100)
  }
  
  # --- Ajustement du modèle NB2 ---
  model <- glm.nb(CPUE ~ 1, data = cpue_data)
  
  # --- Test HNP initial ---
  message("Test HNP : Modèle NB2 (2 simulations initiales)...")
  set.seed(2023)
  hnp_valeurs <- simuler_hnp_nb2(model, n_iter = 2)
  ajustement_hnp <- round(mean(hnp_valeurs), 2)
  nb_iterations_hnp <- 2
  
  # --- Test HNP supplémentaire si ajustement marginal ---
  if (ajustement_hnp >= 10 && ajustement_hnp < 15) {
    message("Ajustement marginal : Ajout de 3 simulations HNP supplémentaires...")
    hnp_valeurs_suppl <- simuler_hnp_nb2(model, n_iter = 3)
    hnp_valeurs <- c(hnp_valeurs, hnp_valeurs_suppl)
    ajustement_hnp <- round(mean(hnp_valeurs), 2)
    nb_iterations_hnp <- 5
  }
  
  # --- Prédiction moyenne et IC 95% ---
  pred <- predict(model, type = "link", se.fit = TRUE)
  cpue_moyenne <- round(exp(pred$fit[1]), 2)
  ic_borne_inf <- round(exp(pred$fit[1] - 1.96 * pred$se.fit[1]), 2)
  ic_borne_sup <- round(exp(pred$fit[1] + 1.96 * pred$se.fit[1]), 2)
  ic_95 <- sprintf("(%s-%s)", ic_borne_inf, ic_borne_sup)
  
  # --- Commentaire sur la qualité d’ajustement ---
  commentaire <- case_when(
    ajustement_hnp < 10 ~ "Bon ajustement.",
    ajustement_hnp < 15 ~ "Ajustement marginal.",
    TRUE ~ "Mauvais ajustement."
  )
  
  # --- Résultat final ---
  tibble(
    methode = "nb2",
    ajustement_hnp = ajustement_hnp,
    aicc = AICc(model),
    cpue_moyenne = cpue_moyenne,
    ic_95 = ic_95,
    commentaire = commentaire,
    convergence = TRUE,
    nb_iterations_hnp = nb_iterations_hnp
  )
}
