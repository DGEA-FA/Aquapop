#' Tracer la structure d’âge observée et la courbe du modèle de mortalité sélectionné
#'
#' Cette fonction affiche l’histogramme des âges issus de `specimen` et y superpose
#' la courbe prédite du modèle de mortalité sélectionné. Le style graphique est cohérent
#' avec la fonction `structure_age()`.
#'
#' @param specimen Un `data.frame` contenant au moins les colonnes `sp` et `age`.
#' @param modele Un modèle ajusté de type `glm`, `glm.nb`, `glmmTMB`, etc.
#' @param info_modele Le tableau produit par `mortalite_compare_modele()$data`
#'
#' @return Un objet `ggplot` combinant histogramme + courbe prédite.
#' @export
#'
#' @examples
#' mortalite_plot_modele(specimen, modele, comparaison_mortalite_df)
mortalite_plot_modele <- function(specimen, modele, info_modele) {
  # Validation
  stopifnot(all(c("sp", "age") %in% names(specimen)))
  
  # Nettoyage et résumé
  data <- specimen |>
    dplyr::filter(!is.na(age)) |>
    dplyr::mutate(age = as.integer(age))
  
  if (nrow(data) == 0) {
    cli::cli_warn("Aucun spécimen avec un âge valide.")
    return(ggplot2::ggplot())
  }
  
  max_age <- max(data$age, na.rm = TRUE)
  max_y <- ceiling(max(table(data$age)) * 1.1)
  nomsp <- get_info_pen(unique(data$sp))$nom_sp
  
  # Prédiction
  df_pred <- tibble::tibble(age = 0:(max_age + 2))
  df_pred$pred <- as.numeric(exp(stats::predict(modele, newdata = df_pred, type = "link")))
  
  # Extraire le sous-titre
  methode_modele <- attr(modele, "methode")
  if (is.null(methode_modele) && !is.null(info_modele)) {
    modele_class <- class(modele)[1]
    methode_modele <- dplyr::case_when(
      modele_class == "glm"                       ~ "poisson",
      modele_class == "glmmTMB" && grepl("nbinom1", modele$call$family) ~ "nb1",
      modele_class == "negbin"                    ~ "nb2",
      modele_class == "glmmTMB" && grepl("compois", modele$call$family) ~ "cmp",
      modele_class == "glmmTMB" && grepl("genpois", modele$call$family) ~ "gp",
      TRUE ~ NA_character_
    )
  }
  ligne_info <- info_modele |>
    dplyr::filter(tolower(Méthode) == methode_modele)
  
  sous_titre <- if (nrow(ligne_info) == 1) {
    glue::glue("A = {ligne_info$A} %, IC 95% = {ligne_info$`IC 95%`}")
  } else {
    NULL
  }
  
  # Tracé final
  ggplot2::ggplot(data, ggplot2::aes(x = age)) +
    ggplot2::geom_histogram(binwidth = 1, closed = "right",
                            fill = couleur_default, color = "white", na.rm = TRUE) +
    ggplot2::geom_line(data = df_pred, ggplot2::aes(x = age, y = pred),
                       color = "red", linewidth = 1.2, inherit.aes = FALSE) +
    ggplot2::labs(
      title = "Distribution d’âge et modèle de mortalité",
      subtitle = sous_titre,
      x = "Âge",
      y = paste0("Nb. ", nomsp, " échantillonnés")
    ) +
    theme_aquapop() +
    ggplot2::scale_x_continuous(
      expand = c(0, 0),
      limits = c(0, max_age + 2),
      breaks = 0:(max_age + 2)
    ) +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, max_y))
}
