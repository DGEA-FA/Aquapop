#' Tester la sur-dispersion dans un modèle de Poisson (mortalité)
#'
#' Cette fonction applique `AER::dispersiontest()` à un modèle de type Poisson
#' ajusté sur les fréquences d’âge (`number ~ age`), et retourne une liste contenant :
#' - la valeur de dispersion,
#' - un message interprétatif,
#' - un graphique des résidus vs ajustés.
#'
#' @param df Un data.frame contenant les colonnes `age` et `number`
#'
#' @return Une liste avec `dispersion`, `message`, `plot`
#' @export
#'
#' @examples
#' mortalite_test_surdispersion_poisson(df)
mortalite_test_surdispersion_poisson <- function(df) {
  stopifnot(all(c("age", "number") %in% names(df)))
  
  # Nettoyage et résumé
  df <- df |>
    dplyr::filter(!is.na(age)) |>
    dplyr::mutate(age = as.integer(age))
  
  # Ajuster le modèle de Poisson
  mod_pois <- glm(number ~ age, family = poisson, data = df)
  
  # Tester la sur-dispersion
  disp_test <- AER::dispersiontest(mod_pois, alternative = "greater")
  disp_value <- unname(disp_test$estimate["dispersion"])
  disp_value <- round(disp_value, 2)
  
  # Interprétation
  message <- if (disp_value > 1.5) {
    glue::glue("Les données présentent une sur-dispersion (valeur = {disp_value}). Des modèles alternatifs comme NB1, NB2, CMP ou GP sont recommandés.")
  } else {
    glue::glue("Aucune sur-dispersion majeure détectée (valeur = {disp_value}). Le modèle de Poisson pourrait être acceptable.")
  }
  
  # Graphique : résidus de Pearson vs valeurs ajustées
  df_plot <- tibble::tibble(
    fitted = fitted(mod_pois),
    residuals = residuals(mod_pois, type = "pearson")
  )
  
  plot <- ggplot2::ggplot(df_plot, ggplot2::aes(x = fitted, y = residuals)) +
    ggplot2::geom_point(color = "#0072B2", size = 2) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::labs(
      x = "Valeurs ajustées (Poisson)",
      y = "Résidus de Pearson",
      title = "Résidus vs Valeurs ajustées (modèle Poisson)"
    ) +
    theme_aquapop()
  
  return(list(
    dispersion = disp_value,
    message = message,
    plot = plot
  ))
}

# dispersiontest <- function(data) {
#   #Mainguy et Moral (2021) ont appliqué l’idée d’avoir recours à des extensions de la distribution de Poisson pour tenir
#   #compte de la sur-dispersion plutôt que d’appliquer un facteur de correction comme le font les estimateurs CRCB (Smith et al. 2012) et le PM adapté de Nelson (2019).
#   #si les données sont sur-dispersées, un modèle s’appuyant sur une distribution de Poisson, soit un GLMPoisson, ne s’ajustera pas suffisamment bien aux données observées car l’équidispersion
#   #requise ne sera pas respectée et ainsi, la SE calculée sera biaisée à la baisse, ce qui aura des incidences sur les inférences statistiques.
#   m.data.p <- glm(number ~ age, family = poisson, data = data)
#   
#   #il faut tester la sur-dispersion sur les données originale et non celles avec extensions de zéros
#   dispersiontest <-
#     AER::dispersiontest(m.data.p , alternative = "greater")
#   dispersiontest[["estimate"]][["dispersion"]]  #sur-dispersion si val >> 1
# }