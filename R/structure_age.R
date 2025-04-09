#' Génère la structure d'âge : graphique, tableau brut et flextable
#'
#' Produit un histogramme ou un tableau des âges des spécimens d'une espèce donnée,
#' selon un regroupement facultatif (sexe, marquage, maturité). L'espèce doit être unique.
#'
#' @param data Un `data.frame` contenant les spécimens pour une seule espèce (colonnes `sp`, `age`, etc.)
#' @param groupement Le groupement de couleur à utiliser : `"tous"`, `"marquage"`, `"sexe"` ou `"maturite"`
#'
#' @return Une liste avec trois éléments : `plot` (ggplot), `data` (data.frame), `flextable` (tableau formaté)
#' @export
structure_age <- function(data,
                          groupement = "tous") {
  # Validation
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
    return(list(
      plot = ggplot2::ggplot(),
      data = vide,
      flextable = flextable::flextable(vide)
    ))
  }
  
  max_age <- max(data$age, na.rm = TRUE)
  max_y <- ceiling(max(table(data$age), na.rm = TRUE) * 1.1)
  
  # Tableau
  df <- data |>
    dplyr::count(age) |>
    dplyr::rename(n = n) |>
    dplyr::mutate(age = as.integer(age))
  
  ft <- flextable::flextable(df) |>
    flextable::set_caption("Structure d'âge") |>
    flextable::align(align = "center", part = "all")
  
  # Graphique
  if (groupement == "tous") {
    plt <- ggplot2::ggplot(data, ggplot2::aes(x = age)) +
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
  } else {
    if (!groupement %in% names(group_labels) || !groupement %in% names(group_colors)) {
      stop("Groupement non reconnu. Choisir parmi 'tous', 'sexe', 'maturite', 'marquage'")
    }
    
    data[[groupement]] <- factor(data[[groupement]], levels = names(group_labels[[groupement]]))
    
    # Forcer les niveaux absents dans la légende
    missing_levels <- setdiff(names(group_labels[[groupement]]), unique(data[[groupement]]))
    if (length(missing_levels) > 0) {
      df_add <- tibble::tibble(age = 0, !!groupement := factor(missing_levels, levels = names(group_labels[[groupement]])))
      data <- dplyr::bind_rows(data, df_add)
    }
    
    plt <- ggplot2::ggplot(data, ggplot2::aes(x = age, fill = !!rlang::sym(groupement))) +
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
  
  return(list(
    plot = plt,
    data = df,
    flextable = ft
  ))
}
