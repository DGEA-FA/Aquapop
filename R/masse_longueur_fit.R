#' Ajuster la relation masse-longueur pour une espèce
#'
#' Cette fonction ajuste une régression linéaire sur les données log-transformées
#' de masse et de longueur. Elle retourne les coefficients estimés (avec IC 95 %),
#' un graphique de la courbe ajustée, et une version formatée du tableau.
#'
#' @importFrom ggplot2 labs geom_line aes geom_point ggplot
#' @importFrom FSA logbtcf
#' @importFrom flextable flextable
#' @importFrom glue glue
#' @importFrom tibble tibble
#' @importFrom stats confint lm
#' @importFrom dplyr filter mutate
#' @param data Un `data.frame` contenant les colonnes `ltm`, `masse`, `sp` et `no_specimen`.
#'
#' @return Une liste nommée contenant :
#' \describe{
#'   \item{data}{Tableau des coefficients estimés (`data.frame`)}
#'   \item{flextable}{Tableau formaté (`flextable`)}
#'   \item{plot}{Graphique ggplot de la relation masse-longueur}
#' }
#' @export
masse_longueur_fit <- function(data) {
  # --- Validation des données ---
  espece <- unique(data$sp)
  if (length(espece) != 1) stop("Les données doivent contenir une seule espèce (sp).")
  
  # --- Prétraitement ---
  donnees_filtrees <- data |>
    filter(!is.na(ltm), !is.na(masse)) |>
    mutate(
      log_masse = log10(masse),
      log_longueur = log10(ltm)
    )
  
  # --- Ajustement du modèle ---
  modele_masse_longueur <- lm(log_masse ~ log_longueur, data = donnees_filtrees)
  resume_modele <- summary(modele_masse_longueur)$coefficients
  intervalle_confiance <- confint(modele_masse_longueur)
  
  # --- Création du tableau brut ---
  table_resultats <- tibble(
    coefficient = c("log10(a)", "b"),
    estimation = round(resume_modele[, 1], 3),
    erreur_standard = round(resume_modele[, 2], 3),
    ic95 = c(
      glue("[{round(intervalle_confiance[1, 1], 3)} - {round(intervalle_confiance[1, 2], 3)}]"),
      glue("[{round(intervalle_confiance[2, 1], 3)} - {round(intervalle_confiance[2, 2], 3)}]")
    )
  )
  
  table_resultats <- set_variable_labels(
    table_resultats,
    coefficient = "Coefficient",
    estimation = "Estimation",
    erreur_standard = "ET",
    ic95 = "IC 95%"
  )
  
  # --- Tableau formaté ---
  table_flextable <- table_resultats |>
    flextable() |>
    labelled_data() |>
    style_flextable_aquapop()
  
  # --- Génération du graphique ---
  sequence_log_longueur <- seq(
    min(donnees_filtrees$log_longueur),
    max(donnees_filtrees$log_longueur),
    length.out = 100
  )
  
  facteur_correction <- logbtcf(modele_masse_longueur, base = 10)
  predictions <- facteur_correction * 10 ^ predict(
    modele_masse_longueur,
    newdata = data.frame(log_longueur = sequence_log_longueur),
    interval = "prediction"
  )
  
  donnees_prediction <- tibble(
    ltm = 10 ^ sequence_log_longueur,
    fit = predictions[, "fit"],
    lwr = predictions[, "lwr"],
    upr = predictions[, "upr"]
  )
  
  graphique_relation <- ggplot() +
    geom_point(data = donnees_filtrees, aes(x = ltm, y = masse)) +
    geom_line(data = donnees_prediction, aes(x = ltm, y = fit), color = "blue") +
    geom_line(data = donnees_prediction, aes(x = ltm, y = lwr), color = "red", linetype = 2) +
    geom_line(data = donnees_prediction, aes(x = ltm, y = upr), color = "red", linetype = 2) +
    theme_aquapop() +
    labs(
      x = "Longueur totale maximale (mm)",
      y = "Masse (g)"
    )
  
  # --- Résultat final ---
  masse_longueur_fit_res <- list(
    data = table_resultats,
    flextable = table_flextable,
    plot = graphique_relation
  )
  
  return(masse_longueur_fit_res)
}
