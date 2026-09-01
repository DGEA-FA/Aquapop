#' Tester la sur-dispersion dans un modèle de Poisson (mortalité)
#'
#' Cette fonction applique un test de sur-dispersion (`dispersiontest()`)
#' à un modèle de Poisson ajusté sur les fréquences d'âge (`number ~ age`).
#' Elle retourne une liste structurée contenant :
#' \itemize{
#'   \item `success` : indicateur logique de réussite
#'   \item `message` : message d'interprétation ou d'erreur
#'   \item `dispersion` : valeur numérique estimée de dispersion, ou `NULL`
#'   \item `p_value` : p-value du test ou `NULL`
#'   \item `z` : la statistique z du test ou `NULL`
#'   \item `plot` : graphique des résidus de Pearson, ou `NULL`
#' }
#'
#' @param data Un `data.frame` contenant au minimum deux colonnes :
#'   \itemize{
#'   \item `age` : l'âge des poissons
#'   \item `number` : la fréquence d'individus observés pour chaque âge
#'   }
#'
#' @return Une liste avec les éléments suivants :
#' \describe{
#'   \item{success}{Un booléen indiquant si le test a pu être réalisé.}
#'   \item{message}{Un message d'interprétation si le test a réussi, ou un message informatif sinon.}
#'   \item{dispersion}{La valeur estimée de dispersion, ou `NULL` si le test n'a pas pu être réalisé.}
#'   \item{z}{La statistique z du test, ou `NULL` si le test n'a pas pu être réalisé.}
#'   \item{p_value}{La p-value du test de sur-dispersion, ou `NULL` si le test n'a pas pu être réalisé.}
#'   \item{plot}{Un objet `ggplot` des résidus de Pearson, ou `NULL` si le modèle n'a pas pu être ajusté.}
#' }
#'
#' @importFrom AER dispersiontest
#' @importFrom dplyr filter mutate
#' @importFrom ggplot2 aes geom_hline geom_point ggplot labs
#' @importFrom glue glue
#' @importFrom stats fitted glm poisson residuals
#' @importFrom tibble tibble
#'
#' @export
#'
#' @examples
#' set.seed(1)
#' df_test <- data.frame(
#'   age = 1:10,
#'   number = rpois(10, lambda = c(5, 10, 15, 20, 15, 10, 5, 4, 3, 2))
#' )
#'
#' res <- mortalite_test_surdispersion_poisson(df_test)
#' res$message
#' res$plot
mortalite_test_surdispersion_poisson <- function(data) {
  # Validation de base ====
  if (is.null(data) || !is.data.frame(data)) {
    return(list(
      success = FALSE,
      message = "Les données fournies sont invalides.",
      dispersion = NULL,
      plot = NULL
    ))
  }
  
  if (nrow(data) == 0) {
    return(list(
      success = FALSE,
      message = "Aucune donnée n'est disponible pour tester la sur-dispersion.",
      dispersion = NULL,
      plot = NULL
    ))
  }
  
  if (!all(c("age", "number") %in% names(data))) {
    return(list(
      success = FALSE,
      message = "Le tableau doit contenir les colonnes `age` et `number`.",
      dispersion = NULL,
      plot = NULL
    ))
  }
  
  # Nettoyage des données ====
  data_clean <- data |>
    filter(!is.na(.data$age), !is.na(.data$number)) |>
    mutate(age = as.integer(.data$age))
  
  if (nrow(data_clean) < 2) {
    return(list(
      success = FALSE,
      message = "Le test de sur-dispersion requiert au moins deux classes d'âge valides.",
      dispersion = NULL,
      plot = NULL
    ))
  }
  
  if (length(unique(data_clean$age)) < 2) {
    return(list(
      success = FALSE,
      message = "Le test de sur-dispersion requiert au moins deux âges distincts.",
      dispersion = NULL,
      plot = NULL
    ))
  }
  
  # Ajustement du modèle ====
  modele_poisson <- tryCatch(
    glm(number ~ age, family = poisson, data = data_clean),
    error = function(e) NULL
  )
  
  if (is.null(modele_poisson)) {
    return(list(
      success = FALSE,
      message = "Le modèle de Poisson n'a pas pu être ajusté pour tester la sur-dispersion.",
      dispersion = NULL,
      plot = NULL
    ))
  }
  
  # Test de sur-dispersion ====
  test_dispersion <- tryCatch(
    dispersiontest(modele_poisson, alternative = "greater"),
    error = function(e) NULL
  )
  
  if (is.null(test_dispersion) || is.null(test_dispersion$estimate)) {
    return(list(
      success = FALSE,
      message = "Le test de sur-dispersion n'a pas pu être calculé.",
      dispersion = NULL,
      plot = NULL
    ))
  }
  
   dispersion_value <- unname(test_dispersion$estimate["dispersion"]) |>
    round(2)
   
   z_value <- unname(test_dispersion$statistic) |>
     round(2)
   
   p_value <- test_dispersion$p.value |>
     round(3)
  
  if (is.na(dispersion_value)) {
    return(list(
      success = FALSE,
      message = "La valeur de dispersion n'a pas pu être estimée.",
      dispersion = NULL,
      plot = NULL
    ))
  }
  
  # Interprétation ====
   message <- if (p_value < 0.05) {
     glue(
       "Le test met en évidence une sur-dispersion statistiquement significative ",
       "(dispersion = {dispersion_value}, z = {z_value}, p = {p_value}). ",
       "Des modèles alternatifs comme NB1, NB2, CMP ou GP devraiet être envisagés."
     )
   } else {
     glue(
       "Le test ne met pas en évidence de sur-dispersion statistiquement significative ",
       "(dispersion = {dispersion_value}, z = {z_value}, p = {p_value}). "
     )
   }
  
  # Graphique des résidus ====
#  data_plot <- tibble(
#    fitted = fitted(modele_poisson),
#    residuals = residuals(modele_poisson, type = "pearson")
#  )
  
#  plot <- ggplot(data_plot, aes(x = fitted, y = residuals)) +
#    geom_point(color = "#0072B2", size = 2) +
#    geom_hline(yintercept = 0, linetype = "dashed") +
#    labs(
#      x = "Valeurs ajustées (Poisson)",
#      y = "Résidus de Pearson",
#      title = "Résidus vs valeurs ajustées (modèle Poisson)"
#    ) +
#    theme_aquapop()
  
  # Sortie ====
  list(
    success = TRUE,
    message = as.character(message),
    dispersion = dispersion_value,
    z = unname(test_dispersion$statistic),
    p_value = test_dispersion$p.value,
    plot = plot
  )
}