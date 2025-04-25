#' Génère un graphique illustrant la courbe de croissance ajustée
#'
#' Cette fonction trace la courbe de croissance d’un groupe de spécimens selon le modèle sélectionné 
#' (Von Bertalanffy, Gompertz ou Logistique). Le graphique inclut les points observés, la courbe ajustée,
#' un intervalle de confiance, et la valeur de Linf.
#'
#' @param dfspecimen Un `data.frame` contenant les données des spécimens, incluant au minimum
#'        les colonnes `ltm` (longueur), `age` et `no_specimen`.
#' @param tablemodele Un `data.frame` contenant les paramètres des modèles ajustés, incluant
#'        les colonnes `methode`, `l_inf`, `k`, et `t0`.
#' @param modele Un nom de modèle à utiliser pour le tracé, parmi `"Von Bertalanffy"`, `"Gompertz"` ou `"Logistique"`.
#'
#' @return Un objet `ggplot` affichant la courbe de croissance ajustée avec les intervalles de confiance.
#' @export
croissance_plot <- function(dfspecimen, tablemodele, modele) {
  # # Fonctions internes pour les modèles de croissance
  # vb_function <- function(age, linf, k, t0) linf * (1 - exp(-k * (age - t0)))
  # gompertz_function <- function(age, linf, k, t0) linf * exp(-exp(-k * (age - t0)))
  # logistic_function <- function(age, linf, k, t0) linf / (1 + exp(-k * (age - t0)))
  # 
  # 1. Nettoyage des données : retirer les valeurs manquantes
  data_clean <- dfspecimen |>
    dplyr::filter(!is.na(ltm), !is.na(age)) |>
    dplyr::select(ltm, age, no_specimen)
  rownames(data_clean) <- seq_len(nrow(data_clean))
  
  # 2. Extraire les paramètres du modèle sélectionné
  model_params <- dplyr::filter(tablemodele, methode == modele)
  
  # 3. Définir la séquence d'âges pour les prédictions
  age_summary <- FSA::Summarize(ltm ~ age, data = data_clean)
  age_min <- min(age_summary$age)
  age_max <- max(age_summary$age)
  age_range_plot <- c(0, ceiling(age_max / 5) * 5 + 1)
  age_ticks <- seq(age_range_plot[1], age_range_plot[2])
  
  # 4. Préparer les valeurs initiales du modèle
  start_values <- list(
    linf = model_params$l_inf,
    k = model_params$k,
    t0 = model_params$t0
  )
  
  # 5. Ajustement du modèle non-linéaire
  model_fit <- switch(modele,
                      "Von Bertalanffy" = stats::nls(ltm ~ vb_function(age, linf, k, t0), data = data_clean, start = start_values),
                      "Gompertz"        = stats::nls(ltm ~ gompertz_function(age, linf, k, t0), data = data_clean, start = start_values),
                      "Logistique"      = stats::nls(ltm ~ logistic_function(age, linf, k, t0), data = data_clean, start = start_values)
  )
  
  # 6. Générer les prédictions et intervalles de confiance
  predictions <- data.frame(
    age = age_ticks,
    investr::predFit(model_fit, data.frame(age = age_ticks), interval = "confidence")
  )
  
  # 7. Construction du graphique
  ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = predictions, ggplot2::aes(x = age, ymin = lwr, ymax = upr), fill = "gray80") +
    ggplot2::geom_point(data = data_clean, ggplot2::aes(x = age, y = ltm), size = 2, alpha = 0.1) +
    ggplot2::geom_line(data = predictions, ggplot2::aes(x = age, y = fit), linewidth = 1, linetype = "dashed") +
    ggplot2::geom_line(data = dplyr::filter(predictions, age >= age_min, age <= age_max),
                       ggplot2::aes(x = age, y = fit), linewidth = 1) +
    ggplot2::annotate("segment", x = -Inf, xend = Inf, y = start_values$linf, yend = start_values$linf,
                      linewidth = 0.5, color = "red", linetype = 2) +
    ggplot2::scale_x_continuous(name = "Âge (année)", breaks = age_ticks, limits = age_range_plot, expand = c(0, 0)) +
    ggplot2::scale_y_continuous(name = "Longueur totale maximale (mm)", expand = c(0, 0)) +
    theme_aquapop() +
    ggplot2::labs(caption = paste0("Modèle : ", modele, "\n",
                                   "Equation : L(âge) = ", round(start_values$linf, 2),
                                   " / (1 + exp(-", round(start_values$k, 3),
                                   " * (âge - ", round(start_values$t0, 3), ")))"))
}
