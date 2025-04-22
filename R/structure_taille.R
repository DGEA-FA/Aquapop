#' Génère la structure de taille : graphique, tableau brut et flextable
#'
#' Produit un histogramme de la structure de taille d'une espèce donnée, ainsi que
#' le tableau de données associé (brut et formaté). L'espèce doit être unique dans les données.
#'
#' @param data Un `data.frame` contenant les spécimens pour une seule espèce (colonnes `sp`, `ltm`, etc.)
#' @param groupement Le groupement de couleur à utiliser : `"tous"` (par défaut), `"marquage"`, `"sexe"` ou `"maturite"`
#'
#' @return Une liste avec trois éléments : `plot` (ggplot), `data` (data.frame), `flextable` (tableau formaté)
#' @export
structure_taille <- function(data,
                             groupement = "tous") {
  # Vérifications
  espece <- unique(data$sp)
  if (length(espece) != 1) stop("Les données doivent contenir une seule espèce.")
  
  info <- get_info_pen(espece)
  if (is.null(info)) stop("Espèce non reconnue.")
  
  nomsp <- info$nom_sp
  binwidth <- info$binwidth
  
  data <- data %>%
    dplyr::mutate(ltm = as.numeric(ltm)) %>%
    dplyr::filter(!is.na(ltm))
  
  if (nrow(data) == 0) {
    vide <- tibble::tibble()
    return(list(
      plot = ggplot2::ggplot(),
      data = vide,
      flextable = flextable::flextable(vide)
    ))
  }
  
  # Création des intervalles
  max_ltm <- max(data$ltm, na.rm = TRUE)
  breaks <- seq(0, max_ltm + binwidth, by = binwidth)
  labels <- paste0("[", head(breaks, -1), "-", tail(breaks, -1), "[")
  data$ltm_interval <- cut(data$ltm, breaks = breaks, include.lowest = TRUE, right = FALSE, labels = labels)
  data$ltm_interval <- factor(data$ltm_interval, levels = labels, ordered = TRUE)
  data <- dplyr::filter(data, !is.na(ltm_interval))
  max_y <- ceiling(max(table(data$ltm_interval), na.rm = TRUE) * 1.1)
  
  # Préparation du graphique
  if (groupement == "tous") {
    plt <- ggplot2::ggplot(data, ggplot2::aes(x = ltm_interval)) +
      ggplot2::geom_bar(fill = couleur_default, color = "white", alpha = 1, na.rm = TRUE) +
      ggplot2::labs(x = "Longueur totale maximale (mm)", y = paste0("Nb. ", nomsp, " échantillonnés")) +
      ggplot2::theme_classic() +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "white", colour = "black", linewidth = 0.5),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        axis.line = ggplot2::element_line(colour = "black")
      ) +
      ggplot2::scale_x_discrete(drop = FALSE, limits = labels) +
      ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, max_y))
  } else {
    if (!groupement %in% names(group_labels) || !groupement %in% names(group_colors)) {
      stop("Groupement non reconnu. Choisir parmi 'tous', 'sexe', 'maturite', 'marquage'")
    }
    
    data[[groupement]] <- factor(data[[groupement]], levels = names(group_labels[[groupement]]), ordered = TRUE)
    
    df_legende <- tibble::tibble(
      categorie = factor(names(group_labels[[groupement]]), levels = names(group_labels[[groupement]])),
      label = unname(group_labels[[groupement]]),
      color = unname(group_colors[[groupement]])
    )
    
    plt <- ggplot2::ggplot(data, ggplot2::aes(x = ltm_interval, fill = !!rlang::sym(groupement))) +
      ggplot2::geom_bar(position = ggplot2::position_stack(reverse = TRUE), color = "white", na.rm = TRUE) +
      ggplot2::geom_bar(data = df_legende, ggplot2::aes(x = categorie, fill = categorie),
                        alpha = 1, width = 0, show.legend = TRUE, na.rm = TRUE) +
      ggplot2::labs(x = "Longueur totale maximale (mm)", y = paste0("Nb. ", nomsp, " échantillonnés")) +
      ggplot2::theme_classic() +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "white", colour = "black", linewidth = 0.5),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        axis.line = ggplot2::element_line(colour = "black"),
        legend.key = ggplot2::element_rect(colour = "white")
      ) +
      ggplot2::scale_x_discrete(drop = FALSE, limits = labels) +
      ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, max_y)) +
      ggplot2::scale_fill_manual(
        values = setNames(df_legende$color, df_legende$categorie),
        name = "",
        labels = setNames(df_legende$label, df_legende$categorie),
        drop = FALSE
      )
  }
  
  # Tableau associé
  df <- structure_taille_extraire_donnees(plt, groupement)
  ft <- flextable::flextable(df) |>
    flextable::set_caption("Structure de taille") |>
    style_flextable_aquapop()
  
  return(list(
    plot = plt,
    data = df,
    flextable = ft
  ))
}

#' Extraire les données d'un histogramme ggplot de structure de taille
#'
#' Cette fonction permet de récupérer les données brutes utilisées dans un graphique généré
#' par la fonction `structure_taille()`. Elle est utile pour créer un tableau correspondant
#' à l'histogramme, en associant les couleurs aux catégories (sexe, maturité, marquage, etc.).
#'
#' @param plot Un objet `ggplot` généré par `structure_taille(..., format = "plot")`
#' @param groupement Le groupement utilisé dans le graphique : `"tous"`, `"marquage"`, `"sexe"` ou `"maturite"`
#'
#' @return Un `data.frame` avec les colonnes `categorie`, `count`, et `x` (classe de taille)
structure_taille_extraire_donnees <- function(plot, groupement) {
  # Vérification
  if (!groupement %in% names(group_colors)) {
    stop("Groupement invalide : choisir parmi 'tous', 'marquage', 'sexe', 'maturite'")
  }
  
  # Inverser le dictionnaire de couleurs pour faire : couleur → nom court
  color_map <- group_colors[[groupement]]
  fill_to_category <- setNames(names(color_map), color_map)
  
  # Extraire les données du graphique
  temp <- ggplot_build(plot)$data[[1]] |>
    dplyr::select(fill, count, x) |>
    dplyr::mutate(categorie = fill_to_category[fill])
  
  # Résultat final
  temp |> dplyr::select(categorie, count, x)
}

