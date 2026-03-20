#' Tracer la courbe de croissance ajustée pour un modèle sélectionné
#'
#' Cette fonction génère un graphique illustrant la courbe de croissance ajustée d’un groupe de spécimens,
#' selon le modèle choisi parmi : Von Bertalanffy, Gompertz ou Logistique.
#' Le graphique comprend les points observés (`ltm` vs `age`), la courbe ajustée, l’intervalle de confiance,
#' ainsi qu’une ligne horizontale représentant la valeur asymptotique de croissance (`L∞`).
#'
#' @param dfspecimen Un `data.frame` de spécimens, contenant au minimum les colonnes `ltm`, `age` et `no_specimen`.
#' @param tablemodele Un `data.frame` contenant les paramètres du modèle ajusté, incluant `methode`, `l_inf`, `k`, `t0` et `convergence`.
#' @param modele Une chaîne de caractères indiquant le modèle à utiliser : `"Von Bertalanffy"`, `"Gompertz"` ou `"Logistique"`.
#'
#' @return Un objet `ggplot` représentant la courbe de croissance ajustée, avec les points observés et l’intervalle de confiance.
#' Retourne `NULL` avec un avertissement si le graphique ne peut pas être produit.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_ribbon labs scale_y_continuous scale_x_continuous annotate
#' @importFrom investr predFit
#' @importFrom stats nls
#' @importFrom dplyr select filter
#' @importFrom FSA Summarize
#'
#' @export
#'
#' @examples
#' dfspecimen <- data.frame(
#'   ltm = c(100, 120, 140, 150),
#'   age = c(1, 2, 3, 4),
#'   no_specimen = 1:4
#' )
#' tablemodele <- data.frame(
#'   methode = c("Von Bertalanffy", "Gompertz", "Logistique"),
#'   l_inf = c("160", "170", "-"),
#'   k = c("0.3", "0.2", "-"),
#'   t0 = c("-0.5", "0.1", "-"),
#'   convergence = c("Convergé", "Convergé", "Le modèle n'a pas convergé")
#' )
#' croissance_plot(dfspecimen, tablemodele, "Von Bertalanffy")
croissance_plot <- function(dfspecimen, tablemodele, modele) {
  
  # --- Fonctions internes ----
  vb_function <- function(age, linf, k, t0) linf * (1 - exp(-k * (age - t0)))
  gompertz_function <- function(age, linf, k, t0) linf * exp(-exp(-k * (age - t0)))
  logistic_function <- function(age, linf, k, t0) linf / (1 + exp(-k * (age - t0)))
  
  # --- Validation des intrants ----
  if (is.null(tablemodele)) {
    warning(
      "Le graphique de croissance ne peut pas être produit, car les résultats de modèles ne sont pas disponibles."
    )
    return(NULL)
  }
  
  if (!is.data.frame(tablemodele) || nrow(tablemodele) == 0) {
    warning(
      "Le graphique de croissance ne peut pas être produit, car le tableau des modèles est vide ou invalide."
    )
    return(NULL)
  }
  
  if (is.null(modele) || length(modele) == 0 || is.na(modele)) {
    warning(
      "Le graphique de croissance ne peut pas être produit, car aucun modèle valide n’a été sélectionné."
    )
    return(NULL)
  }
  
  modeles_valides <- c("Von Bertalanffy", "Gompertz", "Logistique")
  
  if (!modele %in% modeles_valides) {
    warning(
      "Le graphique de croissance ne peut pas être produit, car le modèle demandé est invalide."
    )
    return(NULL)
  }
  
  required_cols <- c("methode", "l_inf", "k", "t0", "convergence")
  
  if (!all(required_cols %in% names(tablemodele))) {
    warning(
      "Le graphique de croissance ne peut pas être produit, car le tableau des modèles ne contient pas les colonnes requises."
    )
    return(NULL)
  }
  
  # --- Nettoyage des données ----
  data_clean <- dfspecimen |>
    filter(!is.na(ltm), !is.na(age)) |>
    select(ltm, age, no_specimen)
  
  if (nrow(data_clean) == 0) {
    warning(
      "Le graphique de croissance ne peut pas être produit, car aucune donnée valide de longueur et d’âge n’est disponible."
    )
    return(NULL)
  }
  
  # --- Extraction des paramètres du modèle sélectionné ----
  model_params <- tablemodele |>
    filter(methode == modele)
  
  if (nrow(model_params) == 0) {
    warning(
      "Le graphique de croissance ne peut pas être produit, car le modèle sélectionné est absent du tableau des résultats."
    )
    return(NULL)
  }
  
  if (!identical(model_params$convergence[[1]], "Convergé")) {
    warning(
      "Le graphique de croissance ne peut pas être produit, car le modèle sélectionné n’a pas convergé."
    )
    return(NULL)
  }
  
  if (any(model_params[1, c("l_inf", "k", "t0")] == "-")) {
    warning(
      "Le graphique de croissance ne peut pas être produit, car un ou plusieurs paramètres du modèle sont invalides."
    )
    return(NULL)
  }
  
  # --- Préparation des valeurs initiales ----
  start_values <- list(
    linf = as.numeric(model_params$l_inf[[1]]),
    k = as.numeric(model_params$k[[1]]),
    t0 = as.numeric(model_params$t0[[1]])
  )
  
  if (any(is.na(unlist(start_values)))) {
    warning(
      "Le graphique de croissance ne peut pas être produit, car un ou plusieurs paramètres du modèle sont manquants."
    )
    return(NULL)
  }
  
  # --- Séquence d'âges pour les prédictions ----
  age_summary <- Summarize(ltm ~ age, data = data_clean)
  age_min <- min(age_summary$age)
  age_max <- max(age_summary$age)
  age_range_plot <- c(0, ceiling(age_max / 5) * 5 + 1)
  age_ticks <- seq(age_range_plot[1], age_range_plot[2])
  
  # --- Ajustement du modèle ----
  model_fit <- tryCatch(
    switch(
      modele,
      "Von Bertalanffy" = nls(
        ltm ~ vb_function(age, linf, k, t0),
        data = data_clean,
        start = start_values
      ),
      "Gompertz" = nls(
        ltm ~ gompertz_function(age, linf, k, t0),
        data = data_clean,
        start = start_values
      ),
      "Logistique" = nls(
        ltm ~ logistic_function(age, linf, k, t0),
        data = data_clean,
        start = start_values
      )
    ),
    error = function(e) NULL
  )
  
  if (is.null(model_fit)) {
    warning(
      "Le graphique de croissance ne peut pas être produit, car l’ajustement du modèle a échoué."
    )
    return(NULL)
  }
  
  # --- Génération des prédictions ----
  predictions <- tryCatch(
    data.frame(
      age = age_ticks,
      predFit(model_fit, data.frame(age = age_ticks), interval = "confidence")
    ),
    error = function(e) NULL
  )
  
  if (is.null(predictions)) {
    warning(
      "Le graphique de croissance ne peut pas être produit, car les prédictions du modèle n’ont pas pu être calculées."
    )
    return(NULL)
  }
  
  # --- Texte de légende selon le modèle ----
  equation_label <- switch(
    modele,
    "Von Bertalanffy" = paste0(
      "Equation : L(âge) = ", round(start_values$linf, 2),
      " * (1 - exp(-", round(start_values$k, 3),
      " * (âge - ", round(start_values$t0, 3), ")))"
    ),
    "Gompertz" = paste0(
      "Equation : L(âge) = ", round(start_values$linf, 2),
      " * exp(-exp(-", round(start_values$k, 3),
      " * (âge - ", round(start_values$t0, 3), ")))"
    ),
    "Logistique" = paste0(
      "Equation : L(âge) = ", round(start_values$linf, 2),
      " / (1 + exp(-", round(start_values$k, 3),
      " * (âge - ", round(start_values$t0, 3), ")))"
    )
  )
  
  # --- Construction du graphique ----
  ggplot() +
    geom_ribbon(
      data = predictions,
      aes(x = age, ymin = lwr, ymax = upr),
      fill = "gray80"
    ) +
    geom_point(
      data = data_clean,
      aes(x = age, y = ltm),
      size = 2,
      alpha = 0.1
    ) +
    geom_line(
      data = predictions,
      aes(x = age, y = fit),
      linewidth = 1,
      linetype = "dashed"
    ) +
    geom_line(
      data = filter(predictions, age >= age_min, age <= age_max),
      aes(x = age, y = fit),
      linewidth = 1
    ) +
    annotate(
      "segment",
      x = -Inf, xend = Inf,
      y = start_values$linf, yend = start_values$linf,
      linewidth = 0.5,
      color = "red",
      linetype = 2
    ) +
    scale_x_continuous(
      name = "Âge (année)",
      breaks = age_ticks,
      limits = age_range_plot,
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      name = "Longueur totale maximale (mm)",
      expand = c(0, 0)
    ) +
    theme_aquapop() +
    labs(
      caption = paste0(
        "Modèle : ", modele, "\n",
        equation_label
      )
    )
}