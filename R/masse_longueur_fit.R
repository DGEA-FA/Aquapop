#' Ajuster la relation masse-longueur pour une espèce
#'
#' Cette fonction ajuste une régression linéaire sur les données log-transformées
#' de masse et de longueur. Elle retourne les coefficients estimés (avec IC 95 %),
#' un graphique de la courbe ajustée, et une version formatée du tableau.
#'
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
    dplyr::filter(!is.na(ltm), !is.na(masse)) |>
    dplyr::mutate(
      log_masse = log10(masse),
      log_longueur = log10(ltm)
    )
  
  # --- Ajustement du modèle ---
  modele_masse_longueur <- stats::lm(log_masse ~ log_longueur, data = donnees_filtrees)
  resume_modele <- summary(modele_masse_longueur)$coefficients
  intervalle_confiance <- stats::confint(modele_masse_longueur)
  
  # --- Création du tableau brut ---
  table_resultats <- tibble::tibble(
    coefficient = c("log10(a)", "b"),
    estimation = round(resume_modele[, 1], 3),
    erreur_standard = round(resume_modele[, 2], 3),
    ic95 = c(
      glue::glue("[{round(intervalle_confiance[1, 1], 3)} - {round(intervalle_confiance[1, 2], 3)}]"),
      glue::glue("[{round(intervalle_confiance[2, 1], 3)} - {round(intervalle_confiance[2, 2], 3)}]")
    )
  )
  
  # --- Tableau formaté ---
  table_flextable <- table_resultats |>
    flextable::flextable() |>
    style_flextable_aquapop()
  
  # --- Génération du graphique ---
  sequence_log_longueur <- seq(
    min(donnees_filtrees$log_longueur),
    max(donnees_filtrees$log_longueur),
    length.out = 100
  )
  
  facteur_correction <- FSA::logbtcf(modele_masse_longueur, base = 10)
  predictions <- facteur_correction * 10 ^ predict(
    modele_masse_longueur,
    newdata = data.frame(log_longueur = sequence_log_longueur),
    interval = "prediction"
  )
  
  donnees_prediction <- tibble::tibble(
    ltm = 10 ^ sequence_log_longueur,
    fit = predictions[, "fit"],
    lwr = predictions[, "lwr"],
    upr = predictions[, "upr"]
  )
  
  graphique_relation <- ggplot2::ggplot() +
    ggplot2::geom_point(data = donnees_filtrees, ggplot2::aes(x = ltm, y = masse)) +
    ggplot2::geom_line(data = donnees_prediction, ggplot2::aes(x = ltm, y = fit), color = "blue") +
    ggplot2::geom_line(data = donnees_prediction, ggplot2::aes(x = ltm, y = lwr), color = "red", linetype = 2) +
    ggplot2::geom_line(data = donnees_prediction, ggplot2::aes(x = ltm, y = upr), color = "red", linetype = 2) +
    theme_aquapop() +
    ggplot2::labs(
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
