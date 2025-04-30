#' Ajuster un modèle de CPUE de type NB2 (Negative Binomial 2)
#'
#' Cette fonction ajuste un modèle de régression NB2 (`glm.nb`) sur les données de CPUE par station,
#' puis applique un test HNP (Half-Normal Plot) pour évaluer la qualité de l’ajustement.
#'
#' @param cpue_data Un `data.frame` produit par `cpue_prepare()` contenant au minimum
#'                  les colonnes `no_station` (identifiant de la station) et `CPUE` (valeur numérique de la capture par unité d'effort).
#'
#' @return Un `data.frame` d’une ligne contenant :
#' \describe{
#'   \item{methode}{Nom du modèle ("nb2")}
#'   \item{ajustement_hnp}{Pourcentage moyen d'observations hors bande dans le HNP}
#'   \item{aicc}{Critère d’information corrigé (AICc)}
#'   \item{cpue_moyenne}{Estimation moyenne de la CPUE (valeur prédite)}
#'   \item{ic_95}{Intervalle de confiance à 95 %}
#'   \item{commentaire}{Appréciation qualitative de l’ajustement}
#'   \item{convergence}{Toujours TRUE si le modèle a été ajusté sans erreur}
#'   \item{nb_iterations_hnp}{Nombre d’itérations du test HNP (2 ou 5)}
#' }
#'
#' @examples
#' fake_data <- tibble::tibble(
#'   no_station = 1:10,
#'   CPUE = rnbinom(10, mu = 5, size = 1)
#' )
#' cpue_fit_modele_nb2(fake_data)
#'
#' @export
cpue_fit_modele_nb2 <- function(cpue_data) {
  
  # --- Fonction interne : test HNP NB2 ---
  simuler_hnp_nb2 <- function(model, n_iter = 2) {
    replicate(
      n_iter,
      hnp::hnp(
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
  model <- MASS::glm.nb(CPUE ~ 1, data = cpue_data)
  
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
  pred <- stats::predict(model, type = "link", se.fit = TRUE)
  cpue_moyenne <- round(exp(pred$fit[1]), 2)
  ic_borne_inf <- round(exp(pred$fit[1] - 1.96 * pred$se.fit[1]), 2)
  ic_borne_sup <- round(exp(pred$fit[1] + 1.96 * pred$se.fit[1]), 2)
  ic_95 <- sprintf("(%s-%s)", ic_borne_inf, ic_borne_sup)
  
  # --- Commentaire sur la qualité d’ajustement ---
  commentaire <- dplyr::case_when(
    ajustement_hnp < 10 ~ "Bon ajustement.",
    ajustement_hnp < 15 ~ "Ajustement marginal.",
    TRUE ~ "Mauvais ajustement."
  )
  
  # --- Résultat final ---
  tibble::tibble(
    methode = "nb2",
    ajustement_hnp = ajustement_hnp,
    aicc = MuMIn::AICc(model),
    cpue_moyenne = cpue_moyenne,
    ic_95 = ic_95,
    commentaire = commentaire,
    convergence = TRUE,
    nb_iterations_hnp = nb_iterations_hnp
  )
}
