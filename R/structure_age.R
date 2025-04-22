#' Structure d'âge des spécimens : graphique, tableau brut et tableau formaté
#'
#' Cette fonction produit la distribution d’âge d’une espèce sous forme de graphique (histogramme),
#' d’un tableau brut (`data.frame`) et d’un tableau formaté (`flextable`). Elle prend en charge un regroupement
#' facultatif par sexe, marquage ou maturité.
#'
#' @param data Un `data.frame` contenant les colonnes suivantes :
#'   - `sp` : code de l’espèce (doit être unique),
#'   - `age` : âge numérique du spécimen,
#'   - la variable de regroupement choisie (`sexe`, `marquage` ou `maturite`), si applicable.
#' @param groupement Type de regroupement à utiliser pour la coloration du graphique :
#'   `"tous"` (par défaut), `"sexe"`, `"maturite"` ou `"marquage"`.
#'
#' @return Une liste nommée contenant trois éléments :
#' \describe{
#'   \item{plot}{Objet `ggplot` représentant l’histogramme de la structure d’âge.}
#'   \item{data}{`data.frame` avec les âges comptés (`age`, `n`).}
#'   \item{flextable}{Tableau formaté avec `flextable`.}
#' }
#' @export
#'
#' @examples
#' df <- data.frame(sp = "SANA", age = c(1, 2, 2, 3, 3, 3))
#' structure_age(df, groupement = "tous")
structure_age <- function(data, groupement = "tous") {
  # --- Validation des entrées avec checkmate et messages personnalisés ---
  # --- Validation des entrées avec checkmate et messages personnalisés ---
  checkmate::assert_data_frame(data, min.rows = 1)
  
  checkmate::assert(
    checkmate::test_subset(c("sp", "age"), colnames(data)),
    "Les colonnes `sp` et `age` sont requises dans le tableau."
  )
  
  checkmate::assert(
    groupement %in% c("tous", "sexe", "maturite", "marquage"),
    "Groupement non reconnu. Choisir parmi : 'tous', 'sexe', 'maturite', 'marquage'."
  )
  
  if (groupement != "tous") {
    if (!(groupement %in% colnames(data))) {
      stop(paste0("La colonne correspondant au groupement '", groupement, "' est manquante dans les données."))
    }
  }
  
  especes_uniques <- unique(data$sp)
  checkmate::assert(
    length(especes_uniques) == 1,
    "Les données doivent contenir une seule espèce (`sp`)."
  )
  
  info_espece <- get_info_pen(especes_uniques)
  checkmate::assert(
    !is.null(info_espece) && is.list(info_espece),
    "Espèce non reconnue."
  )
  
  # --- Nettoyage et filtrage des données ---
  
  nom_espece <- info_espece$nom_sp
  
  data_clean <- data |>
    dplyr::mutate(age = as.numeric(age)) |>
    dplyr::filter(!is.na(age))
  
  if (nrow(data_clean) == 0) {
    tableau_vide <- tibble::tibble(age = numeric(0), n = integer(0))
    return(list(
      plot = ggplot2::ggplot(),
      data = tableau_vide,
      flextable = flextable::flextable(tableau_vide)
    ))
  }
  
  # --- Calcul des bornes pour l’axe ---
  age_max <- max(data_clean$age, na.rm = TRUE)
  frequence_max <- ceiling(max(table(data_clean$age), na.rm = TRUE) * 1.1)
  
  # --- Création du tableau brut et du flextable ---
  age_counts <- data_clean |>
    dplyr::count(age, name = "n") |>
    dplyr::mutate(age = as.integer(age))
  
  tableau_age <- flextable::flextable(age_counts) |>
    flextable::set_caption("Structure d'âge") |>
    style_flextable_aquapop()
  
  # --- Création du graphique ---
  graphique_structure_age <- if (groupement == "tous") {
    ggplot2::ggplot(data_clean, ggplot2::aes(x = age)) +
      ggplot2::geom_histogram(binwidth = 1, closed = "right",
                              fill = couleur_default, color = "white", na.rm = TRUE) +
      ggplot2::labs(x = "Âge", y = paste0("Nb. ", nom_espece, " échantillonnés")) +
      ggplot2::theme_classic() +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "white", colour = "black", linewidth = 0.5),
        axis.line = ggplot2::element_line(colour = "black")
      ) +
      ggplot2::scale_x_continuous(
        expand = c(0, 0),
        limits = c(0, age_max + 2),
        breaks = 0:(age_max + 2)
      ) +
      ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, frequence_max))
  } else {
    # Groupement par couleur (sexe, maturite, marquage)
    niveaux_groupement <- names(group_labels[[groupement]])
    data_clean$groupe <- factor(data_clean[[groupement]], levels = niveaux_groupement)
    
    # Ajout de niveaux manquants pour affichage cohérent
    niveaux_absents <- setdiff(niveaux_groupement, unique(data_clean$groupe))
    if (length(niveaux_absents) > 0) {
      faux_niveaux <- tibble::tibble(age = 0, groupe = factor(niveaux_absents, levels = niveaux_groupement))
      data_clean <- dplyr::bind_rows(data_clean, faux_niveaux)
    }
    
    ggplot2::ggplot(data_clean, ggplot2::aes(x = age, fill = groupe)) +
      ggplot2::geom_histogram(
        binwidth = 1, closed = "right", color = "white",
        position = ggplot2::position_stack(reverse = TRUE), na.rm = TRUE
      ) +
      ggplot2::labs(x = "Âge", y = paste0("Nb. ", nom_espece, " échantillonnés")) +
      ggplot2::theme_classic() +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "white", colour = "black", linewidth = 0.5),
        axis.line = ggplot2::element_line(colour = "black"),
        legend.key = ggplot2::element_rect(colour = "white")
      ) +
      ggplot2::scale_x_continuous(
        expand = c(0, 0),
        limits = c(0, age_max + 2),
        breaks = 0:(age_max + 2)
      ) +
      ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, frequence_max)) +
      ggplot2::scale_fill_manual(
        values = group_colors[[groupement]],
        name = "",
        labels = group_labels[[groupement]],
        drop = FALSE
      )
  }
  
  # --- Retour de la liste ---
  return(list(
    plot = graphique_structure_age,
    data = age_counts,
    flextable = tableau_age
  ))
}
