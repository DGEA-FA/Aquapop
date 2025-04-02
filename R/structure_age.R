#' Génère la structure d'âge en graphique ou tableau
#'
#' Produit un histogramme ou un tableau des âges des spécimens d'une espèce donnée,
#' selon un regroupement facultatif (sexe, marquage, maturité). L'espèce doit être unique.
#'
#' @param data Un `data.frame` contenant les spécimens pour une seule espèce (colonnes `sp`, `age`, etc.)
#' @param groupement Le groupement de couleur à utiliser : `"tous"` (par défaut), `"marquage"`, `"sexe"` ou `"maturite"`
#' @param format Format de sortie : `"plot"` (par défaut), `"data.frame"`, ou `"flextable"`
#'
#' @return Un objet `ggplot`, un `data.frame` ou un `flextable` selon le format choisi.
#' @export
#'
#' @examples
#' structure_age(data = df, groupement = "sexe", format = "plot")
#' structure_age(data = df, groupement = "maturite", format = "data.frame")
#' structure_age(data = df, groupement = "tous", format = "flextable")
structure_age <- function(data,
                          groupement = "tous",
                          format = c("plot", "data.frame", "flextable")) {
  format <- match.arg(format)
  
  # Validation des données
  espece <- unique(data$sp)
  if (length(espece) != 1) stop("Les données doivent contenir une seule espèce.")
  
  info <- get_info_pen(espece)
  if (is.null(info)) stop("Espèce non reconnue.")
  
  nomsp <- info$nom_sp
  
  data <- data |>
    dplyr::mutate(age = as.numeric(age)) |>
    dplyr::filter(!is.na(age))
  
  if (nrow(data) == 0) {
    vide <- tibble::tibble()
    return(switch(format,
                  "plot" = ggplot2::ggplot(),
                  "data.frame" = vide,
                  "flextable" = flextable::flextable(vide)))
  }
  
  max_age <- max(data$age, na.rm = TRUE)
  max_y <- ceiling(max(table(data$age), na.rm = TRUE) * 1.1)
  
  # ----- Format tableau -----
  if (format != "plot") {
    df <- data |>
      dplyr::count(age) |>
      dplyr::rename(n = n) |>
      dplyr::mutate(age = as.integer(age))
    
    if (format == "data.frame") return(df)
    
    return(
      flextable::flextable(df) |>
        flextable::set_caption("Structure d'âge") |>
        flextable::align(align = "center", part = "all")
    )
  }
  
  # ----- Format graphique : sans groupement -----
  if (groupement == "tous") {
    return(
      ggplot2::ggplot(data, ggplot2::aes(x = age)) +
        ggplot2::geom_histogram(binwidth = 1, closed = "right",
                                fill = couleur_default, color = "white", na.rm = TRUE) +
        ggplot2::labs(x = "Âge", y = paste0("Nb. ", nomsp, " échantillonnés")) +
        ggplot2::theme_classic() +
        ggplot2::theme(
          panel.background = ggplot2::element_rect(fill = "white", colour = "black", linewidth = 0.5),
          axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
          axis.line = ggplot2::element_line(colour = "black")
        ) +
        ggplot2::scale_x_continuous(
          expand = c(0, 0),
          limits = c(0, max_age + 2),
          breaks = 0:(max_age + 2)
        ) +
        ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, max_y))
    )
  }
  
  # ----- Format graphique : avec groupement -----
  if (!groupement %in% names(group_labels) || !groupement %in% names(group_colors)) {
    stop("Groupement non reconnu. Choisir parmi 'tous', 'sexe', 'maturite', 'marquage'")
  }
  
  data[[groupement]] <- factor(data[[groupement]], levels = names(group_labels[[groupement]]))
  
  # Ajouter lignes fictives pour forcer la légende complète
  missing_levels <- setdiff(names(group_labels[[groupement]]), unique(data[[groupement]]))
  if (length(missing_levels) > 0) {
    df_add <- tibble::tibble(age = 0, !!groupement := factor(missing_levels, levels = names(group_labels[[groupement]])))
    data <- dplyr::bind_rows(data, df_add)
  }
  
  ggplot2::ggplot(data, ggplot2::aes(x = age, fill = !!rlang::sym(groupement))) +
    ggplot2::geom_histogram(
      binwidth = 1, closed = "right", color = "white", alpha = 1,
      position = ggplot2::position_stack(reverse = TRUE), na.rm = TRUE
    ) +
    ggplot2::labs(x = "Âge", y = paste0("Nb. ", nomsp, " échantillonnés")) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white", colour = "black", linewidth = 0.5),
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.key = ggplot2::element_rect(colour = "white")
    ) +
    ggplot2::scale_x_continuous(
      expand = c(0, 0),
      limits = c(0, max_age + 2),
      breaks = 0:(max_age + 2)
    ) +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, max_y)) +
    ggplot2::scale_fill_manual(
      values = group_colors[[groupement]],
      name = "",
      labels = group_labels[[groupement]],
      drop = FALSE
    )
} 