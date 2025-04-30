#' Tracer la structure d’âge observée et la courbe du modèle de mortalité sélectionné
#'
#' Cette fonction affiche l’histogramme des âges issus du tableau `specimen` et y superpose
#' la courbe prédite à partir d’un modèle de mortalité ajusté (`modele`). Le style graphique
#' est cohérent avec les autres figures de structure d’âge (voir `structure_age()`).
#'
#' @param specimen Un `data.frame` contenant au moins les colonnes `sp` (code de l’espèce) et `age`.
#' @param modele Un objet de modèle ajusté (`glm`, `glm.nb`, `glmmTMB`, etc.) pour prédire la fréquence selon l’âge.
#' @param info_modele Un `data.frame` issu de `mortalite_compare_modele()$data` contenant les estimations de A et IC 95%.
#'
#' @return Un objet `ggplot2` combinant histogramme observé et courbe prédite.
#' @export
#'
#' @examples
#' # Exemple fictif avec données simulées
#' data_exemple <- tibble::tibble(
#'   sp = "SAFO",
#'   age = sample(0:10, size = 200, replace = TRUE)
#' )
#' modele_exemple <- glm(age ~ 1, data = data_exemple, family = poisson())
#' info_modele_exemple <- tibble::tibble(
#'   Méthode = "poisson", A = 38, `IC 95%` = "32–45"
#' )
#' mortalite_plot_modele(data_exemple, modele_exemple, info_modele_exemple)
mortalite_plot_modele <- function(specimen, modele, info_modele) {
  # --- Validation des données ---
  stopifnot(all(c("sp", "age") %in% names(specimen)))
  
  # --- Préparation des données ---
  donnees_age <- specimen |>
    dplyr::filter(!is.na(age)) |>
    dplyr::mutate(age = as.integer(age))
  
  if (nrow(donnees_age) == 0) {
    cli::cli_warn("Aucun spécimen avec un âge valide.")
    return(ggplot2::ggplot())
  }
  
  max_age <- max(donnees_age$age, na.rm = TRUE)
  max_y <- ceiling(max(table(donnees_age$age)) * 1.1)
  nom_espece <- get_info_pen(unique(donnees_age$sp))$nom_sp
  
  # --- Prédiction du modèle ---
  donnees_prediction <- tibble::tibble(age = 0:(max_age + 2))
  donnees_prediction$pred <- as.numeric(
    exp(stats::predict(modele, newdata = donnees_prediction, type = "link"))
  )
  
  # --- Extraction du sous-titre ---
  methode_modele <- attr(modele, "methode")
  if (is.null(methode_modele) && !is.null(info_modele)) {
    modele_class <- class(modele)[1]
    methode_modele <- dplyr::case_when(
      modele_class == "glm" ~ "poisson",
      modele_class == "glmmTMB" && grepl("nbinom1", modele$call$family) ~ "nb1",
      modele_class == "negbin" ~ "nb2",
      modele_class == "glmmTMB" && grepl("compois", modele$call$family) ~ "cmp",
      modele_class == "glmmTMB" && grepl("genpois", modele$call$family) ~ "gp",
      TRUE ~ NA_character_
    )
  }
  
  ligne_info_modele <- info_modele |>
    dplyr::filter(tolower(Méthode) == methode_modele)
  
  sous_titre <- if (nrow(ligne_info_modele) == 1) {
    glue::glue("A = {ligne_info_modele$A} %, IC 95% = {ligne_info_modele$`IC 95%`}")
  } else {
    NULL
  }
  
  # --- Tracé final ---
  ggplot2::ggplot(donnees_age, ggplot2::aes(x = age)) +
    ggplot2::geom_histogram(
      binwidth = 1, closed = "right",
      fill = couleur_default, color = "white", na.rm = TRUE
    ) +
    ggplot2::geom_line(
      data = donnees_prediction,
      ggplot2::aes(x = age, y = pred),
      color = "red", linewidth = 1.2,
      inherit.aes = FALSE
    ) +
    ggplot2::labs(
      title = "Distribution d’âge et modèle de mortalité",
      subtitle = sous_titre,
      x = "Âge",
      y = paste0("Nb. ", nom_espece, " échantillonnés")
    ) +
    theme_aquapop() +
    ggplot2::scale_x_continuous(
      expand = c(0, 0),
      limits = c(0, max_age + 2),
      breaks = 0:(max_age + 2)
    ) +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, max_y))
}
