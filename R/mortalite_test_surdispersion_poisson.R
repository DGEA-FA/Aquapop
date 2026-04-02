#' Tester la sur-dispersion dans un modèle de Poisson (mortalité)
#'
#' Cette fonction applique un test de sur-dispersion (`dispersiontest()`)
#' à un modèle de Poisson ajusté sur les fréquences d'âge (`number ~ age`).
#' Elle retourne :
#' - la valeur numérique de dispersion,
#' - un message d'interprétation textuel,
#' - un graphique des résidus de Pearson vs les valeurs ajustées.
#'
#' @param data Un `data.frame` contenant au minimum deux colonnes :
#'   - `age` : l'âge des poissons (entier ou numérique),
#'   - `number` : la fréquence d'individus observés pour chaque âge.
#'
#' @return Une liste nommée avec trois éléments :
#'   - `dispersion` : (numérique) valeur estimée de dispersion ;
#'   - `message` : (caractère) interprétation textuelle de la valeur ;
#'   - `plot` : (ggplot) graphique des résidus de Pearson.
#'
#' @importFrom AER dispersiontest
#' @importFrom dplyr filter mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_hline labs
#' @importFrom glue glue
#' @importFrom tibble tibble
#' @importFrom stats fitted
#' @export
#'
#' @examples
#' # Exemple simulé
#' set.seed(1)
#' df_test <- data.frame(
#'   age = 1:10,
#'   number = rpois(10, lambda = c(5, 10, 15, 20, 15, 10, 5, 4, 3, 2))
#' )
#' res <- mortalite_test_surdispersion_poisson(df_test)
#' print(res$message)
#' print(res$plot)
mortalite_test_surdispersion_poisson <- function(data) {
  if (!all(c("age", "number") %in% names(data))) {
    stop("Le data.frame doit contenir les colonnes 'age' et 'number'.")
  }
  
  # Nettoyage de base
  data <- data |>
    filter(!is.na(age)) |>
    mutate(age = as.integer(age))
  
  # --- Ajustement ---
  mod_pois <- glm(number ~ age, family = poisson, data = data)
  
  # --- Test de sur-dispersion ---
  disp_test <- dispersiontest(mod_pois, alternative = "greater")
  disp_value <- unname(disp_test$estimate["dispersion"]) |> round(2)
  
  # Interprétation
  message <- if (disp_value > 1.5) {
    glue("Les données présentent une sur-dispersion (valeur = {disp_value}). Des modèles alternatifs comme NB1, NB2, CMP ou GP sont recommandés.")
  } else {
    glue("Aucune sur-dispersion majeure détectée (valeur = {disp_value}). Le modèle de Poisson pourrait être acceptable.")
  }
  
  # --- Graphique des résidus ---
  df_plot <- tibble(
    fitted = fitted(mod_pois),
    residuals = residuals(mod_pois, type = "pearson")
  )
  
  plot <- ggplot(df_plot, aes(x = fitted, y = residuals)) +
    geom_point(color = "#0072B2", size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(
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
