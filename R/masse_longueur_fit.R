#' Calculer et afficher la relation masse-longueur pour une espèce
#'
#' Cette fonction ajuste une régression linéaire sur les données log-transformées
#' de masse et de longueur. Elle retourne un tableau contenant les coefficients estimés
#' et un graphique de la courbe ajustée avec ses intervalles de prédiction.
#'
#' @param data Un `data.frame` contenant `ltm`, `masse`, `sp` et `no_specimen`.
#'
#' @return Une liste contenant :
#' \describe{
#'   \item{`data`}{Un `data.frame` avec les coefficients estimés, erreurs standard et IC 95 %.}
#'   \item{`flextable`}{Une version formatée du tableau des coefficients.}
#'   \item{`plot`}{Un graphique `ggplot` de la relation masse-longueur.}
#' }
#' @export
masse_longueur_fit <- function(data) {
  sp <- unique(data$sp)
  if (length(sp) != 1) stop("Les données doivent être filtrées pour une seule espèce.")
  
  df <- data %>%
    dplyr::filter(!is.na(ltm), !is.na(masse)) %>%
    dplyr::mutate(
      logW = log10(masse),
      logL = log10(ltm)
    )
  
  fit <- lm(logW ~ logL, data = df)
  coef_summary <- summary(fit)$coefficients
  conf_int <- confint(fit)
  
  table_coef <- tibble::tibble(
    Coefficient = c("log10(a)", "b"),
    Estimate = round(coef_summary[, 1], 3),
    SE       = round(coef_summary[, 2], 3),
    IC95     = c(
      glue::glue("[{round(conf_int[1, 1], 3)} - {round(conf_int[1, 2], 3)}]"),
      glue::glue("[{round(conf_int[2, 1], 3)} - {round(conf_int[2, 2], 3)}]")
    )
  )
  
  ft <- table_coef %>%
    flextable::flextable() %>%
    style_flextable_aquapop()
  
  xs <- seq(min(df$logL), max(df$logL), length.out = 100)
  cf <- FSA::logbtcf(fit, 10)
  preds <- cf * 10 ^ predict(fit, newdata = data.frame(logL = xs), interval = "prediction")
  pred_df <- tibble::tibble(
    ltm = 10 ^ xs,
    fit = preds[, "fit"],
    lwr = preds[, "lwr"],
    upr = preds[, "upr"]
  )
  
  fig <- ggplot2::ggplot() +
    ggplot2::geom_point(data = df, ggplot2::aes(x = ltm, y = masse)) +
    ggplot2::geom_line(data = pred_df, ggplot2::aes(x = ltm, y = fit), color = "blue") +
    ggplot2::geom_line(data = pred_df, ggplot2::aes(x = ltm, y = lwr), color = "red", linetype = 2) +
    ggplot2::geom_line(data = pred_df, ggplot2::aes(x = ltm, y = upr), color = "red", linetype = 2) +
    ggplot2::theme_classic() +
    ggplot2::labs(
      x = "Longueur totale maximale (mm)",
      y = "Masse (g)"
    )
  
  return(list(
    data = table_coef,
    flextable = ft,
    plot = fig
  ))
}
