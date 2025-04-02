#' Calcule et affiche la relation masse-longueur pour une espèce
#'
#' Cette fonction ajuste une régression linéaire sur les données log-transformées
#' de masse et de longueur. Elle retourne soit :
#' - un tableau des coefficients (`data.frame` ou `flextable`),
#' - ou un graphique `ggplot`.
#'
#' @param data Un `data.frame` contenant `ltm`, `masse`, `sp` et `no_specimen`.
#' @param format Format de sortie : `"data.frame"` (par défaut), `"flextable"` ou `"plot"`.
#'
#' @return Un tableau ou un graphique, selon le format spécifié.
#' @export
relation_masse_longueur <- function(data, format = c("data.frame", "flextable", "plot")) {
  format <- match.arg(format)
  
  sp <- unique(data$sp)
  if (length(sp) != 1) stop("Les données doivent être filtrées pour une seule espèce.")
  
  df <- data %>%
    filter(!is.na(ltm), !is.na(masse)) %>%
    mutate(
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
  
  if (format == "data.frame") return(table_coef)
  
  if (format == "flextable") {
    return(
      table_coef %>%
        flextable::flextable() %>%
        flextable::autofit() %>%
        flextable::align(align = "center", part = "all")
    )
  }
  
  if (format == "plot") {
    xs <- seq(min(df$logL), max(df$logL), length.out = 100)
    cf <- FSA::logbtcf(fit, 10)
    preds <- cf * 10 ^ predict(fit, newdata = data.frame(logL = xs), interval = "prediction")
    pred_df <- tibble::tibble(
      ltm = 10 ^ xs,
      fit = preds[, "fit"],
      lwr = preds[, "lwr"],
      upr = preds[, "upr"]
    )
    
    return(
      ggplot() +
        geom_point(data = df, aes(x = ltm, y = masse)) +
        geom_line(data = pred_df, aes(x = ltm, y = fit), color = "blue") +
        geom_line(data = pred_df, aes(x = ltm, y = lwr), color = "red", linetype = 2) +
        geom_line(data = pred_df, aes(x = ltm, y = upr), color = "red", linetype = 2) +
        theme_classic() +
        labs(
          x = "Longueur totale maximale (mm)",
          y = "Masse (g)"
        )
    )
  }
}
