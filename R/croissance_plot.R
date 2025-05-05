#' Tracer la courbe de croissance ajustée pour un modèle sélectionné
#'
#' Cette fonction génère un graphique illustrant la courbe de croissance ajustée d’un groupe de spécimens,
#' selon le modèle choisi parmi : Von Bertalanffy, Gompertz ou Logistique.
#' Le graphique comprend les points observés (`ltm` vs `age`), la courbe ajustée, l’intervalle de confiance,
#' ainsi qu’une ligne horizontale représentant la valeur asymptotique de croissance (`L∞`).
#'
#' @param dfspecimen Un `data.frame` de spécimens, contenant au minimum les colonnes `ltm`, `age` et `no_specimen`.
#' @param tablemodele Un `data.frame` contenant les paramètres du modèle ajusté, incluant `methode`, `l_inf`, `k` et `t0`.
#' @param modele Une chaîne de caractères indiquant le modèle à utiliser : `"Von Bertalanffy"`, `"Gompertz"` ou `"Logistique"`.
#'
#' @return Un objet `ggplot` représentant la courbe de croissance ajustée, avec les points observés et l’intervalle de confiance.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_ribbon labs scale_y_continuous scale_x_continuous annotate
#' @importFrom investr predFit
#' @importFrom stats nls
#' @importFrom dplyr select filter
#' @importFrom tibble tibble
#' @importFrom FSA Summarize
#'
#' @export
#'
#' @examples
#' dfspecimen <- tibble::tibble(
#'   ltm = c(100, 120, 140, 150),
#'   age = c(1, 2, 3, 4),
#'   no_specimen = 1:4
#' )
#' tablemodele <- tibble::tibble(
#'   methode = "Von Bertalanffy",
#'   l_inf = 160,
#'   k = 0.3,
#'   t0 = -0.5
#' )
#' croissance_plot(dfspecimen, tablemodele, "Von Bertalanffy")

croissance_plot <- function(dfspecimen, tablemodele, modele) {
  # --- Fonctions internes ---
  vb_function <- function(age, linf, k, t0) linf * (1 - exp(-k * (age - t0)))
  gompertz_function <- function(age, linf, k, t0) linf * exp(-exp(-k * (age - t0)))
  logistic_function <- function(age, linf, k, t0) linf / (1 + exp(-k * (age - t0)))
  
  # --- Nettoyage des données ---
  data_clean <- dfspecimen |>
    filter(!is.na(ltm), !is.na(age)) |>
    select(ltm, age, no_specimen)
  
  # --- Extraction des paramètres du modèle sélectionné ---
  model_params <- filter(tablemodele, methode == modele)
  
  # --- Séquence d'âges pour les prédictions ---
  age_summary <- Summarize(ltm ~ age, data = data_clean)
  age_min <- min(age_summary$age)
  age_max <- max(age_summary$age)
  age_range_plot <- c(0, ceiling(age_max / 5) * 5 + 1)
  age_ticks <- seq(age_range_plot[1], age_range_plot[2])
  
  # --- Préparation des valeurs initiales ---
  start_values <- list(
    linf = model_params$l_inf,
    k = model_params$k,
    t0 = model_params$t0
  )
  
  # --- Validation ---
  if (!modele %in% c("Von Bertalanffy", "Gompertz", "Logistique")) {
    stop("Le modèle doit être l’un de : 'Von Bertalanffy', 'Gompertz' ou 'Logistique'")
  }
  
  
  # --- Ajustement du modèle ---
  model_fit <- switch(modele,
                      "Von Bertalanffy" = nls(ltm ~ vb_function(age, linf, k, t0), data = data_clean, start = start_values),
                      "Gompertz"        = nls(ltm ~ gompertz_function(age, linf, k, t0), data = data_clean, start = start_values),
                      "Logistique"      = nls(ltm ~ logistic_function(age, linf, k, t0), data = data_clean, start = start_values)
  )
  
  # --- Génération des prédictions ---
  predictions <- data.frame(
    age = age_ticks,
    predFit(model_fit, data.frame(age = age_ticks), interval = "confidence")
  )
  
  # --- Construction du graphique ---
  ggplot() +
    geom_ribbon(data = predictions, aes(x = age, ymin = lwr, ymax = upr), fill = "gray80") +
    geom_point(data = data_clean, aes(x = age, y = ltm), size = 2, alpha = 0.1) +
    geom_line(data = predictions, aes(x = age, y = fit), linewidth = 1, linetype = "dashed") +
    geom_line(data = filter(predictions, age >= age_min, age <= age_max),
                       aes(x = age, y = fit), linewidth = 1) +
    annotate("segment", x = -Inf, xend = Inf, y = start_values$linf, yend = start_values$linf,
                      linewidth = 0.5, color = "red", linetype = 2) +
    scale_x_continuous(name = "Âge (année)", breaks = age_ticks, limits = age_range_plot, expand = c(0, 0)) +
    scale_y_continuous(name = "Longueur totale maximale (mm)", expand = c(0, 0)) +
    theme_aquapop() +
    labs(caption = paste0("Modèle : ", modele, "\n",
                                   "Equation : L(âge) = ", round(start_values$linf, 2),
                                   " / (1 + exp(-", round(start_values$k, 3),
                                   " * (âge - ", round(start_values$t0, 3), ")))"))
}
