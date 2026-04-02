#' Structure d'âge des spécimens : graphique, tableau brut et tableau formaté
#'
#' Cette fonction produit la distribution d'âge d'une espèce sous forme de graphique
#' (histogramme), d'un tableau brut (`data.frame`) et d'un tableau formaté
#' (`flextable`). Elle prend en charge un regroupement facultatif par sexe,
#' marquage ou maturité.
#'
#' Si aucune donnée exploitable n'est disponible (ex. : aucun spécimen ou tous
#' les âges sont manquants), la fonction retourne un objet structuré avec
#' `success = FALSE`, sans générer d'erreur.
#'
#' @param data Un `data.frame` contenant les colonnes suivantes :
#'   - `sp` : code de l'espèce (doit être unique),
#'   - `age` : âge numérique du spécimen,
#'   - la variable de regroupement choisie (`sexe`, `marquage` ou `maturite`),
#'     si applicable.
#' @param groupement Type de regroupement à utiliser pour la coloration du
#'   graphique : `"tous"` (par défaut), `"sexe"`, `"maturite"` ou `"marquage"`.
#'
#' @return Une liste nommée contenant :
#' \describe{
#'   \item{success}{Booléen indiquant si le graphique a pu être produit}
#'   \item{plot}{Objet `ggplot` représentant l'histogramme, ou `NULL` si non disponible}
#'   \item{data}{`data.frame` avec les âges comptés}
#'   \item{flextable}{Tableau formaté avec `flextable`, prêt à être affiché ou exporté}
#'   \item{message}{Message explicatif si l'analyse n'est pas disponible}
#' }
#'
#' @importFrom dplyr bind_rows count filter mutate
#' @importFrom ggplot2 scale_y_continuous scale_x_continuous labs geom_histogram aes position_stack scale_fill_manual ggplot
#' @importFrom flextable set_caption flextable
#' @importFrom tibble tibble
#' @importFrom checkmate assert assert_data_frame test_subset
#'
#' @examples
#' df <- data.frame(
#'   sp = "SANA",
#'   age = c(1, 2, 2, 3, 3, 3)
#' )
#'
#' res <- structure_age(df, groupement = "tous")
#'
#' if (res$success) {
#'   res$data
#'   res$flextable
#' }
#'
#' @export
structure_age <- function(data, groupement = "tous") {
  
  # Cas sans ligne ----
  if (nrow(data) == 0) {
    return(list(
      success = FALSE,
      plot = NULL,
      data = NULL,
      flextable = NULL,
      message = "Aucun spécimen valide disponible pour produire la structure d'âge."
    ))
  }
  
  # Validation des entrées ----
  assert_data_frame(data)
  
  assert(
    test_subset(c("sp", "age"), colnames(data)),
    "Les colonnes `sp` et `age` sont requises dans le tableau."
  )
  
  assert(
    groupement %in% c("tous", "sexe", "maturite", "marquage"),
    "Groupement non reconnu. Choisir parmi : 'tous', 'sexe', 'maturite', 'marquage'."
  )
  
  if (groupement != "tous" && !(groupement %in% colnames(data))) {
    stop(
      paste0(
        "La colonne correspondant au groupement '",
        groupement,
        "' est manquante dans les données."
      )
    )
  }
  
  especes_uniques <- as.character(unique(data$sp))
  
  assert(
    length(especes_uniques) == 1,
    "Les données doivent contenir une seule espèce (`sp`)."
  )
  
  info_espece <- get_info_pen(especes_uniques)
  
  assert(
    !is.null(info_espece) && is.list(info_espece),
    "Espèce non reconnue."
  )
  
  # Nettoyage des données ----
  nom_espece <- info_espece$nom_sp
  
  data_clean <- data |>
    mutate(age = as.numeric(age)) |>
    filter(!is.na(age))
  
  # Cas sans donnée exploitable ----
  if (nrow(data_clean) == 0) {
    return(list(
      success = FALSE,
      plot = NULL,
      data = NULL,
      flextable = NULL,
      message = "Aucun spécimen valide disponible pour produire la structure d'âge."
    ))
  }
  
  # Calcul des bornes ----
  age_max <- max(data_clean$age, na.rm = TRUE)
  frequence_max <- ceiling(max(table(data_clean$age), na.rm = TRUE) * 1.1)
  
  # Tableau brut ----
  age_counts <- data_clean |>
    count(age, name = "n") |>
    mutate(age = as.integer(age))
  
  tableau_age <- flextable(age_counts) |>
    set_caption("Structure d'âge") |>
    style_flextable_aquapop()
  
  # Graphique ----
  graphique_structure_age <- if (groupement == "tous") {
    ggplot(data_clean, aes(x = age)) +
      geom_histogram(
        binwidth = 1,
        closed = "right",
        fill = couleur_default,
        color = "white",
        na.rm = TRUE
      ) +
      labs(
        x = "Âge",
        y = paste0("Nb. ", nom_espece, " échantillonnés")
      ) +
      theme_aquapop() +
      scale_x_continuous(
        expand = c(0, 0),
        limits = c(0, age_max + 2),
        breaks = 0:(age_max + 2)
      ) +
      scale_y_continuous(
        expand = c(0, 0),
        limits = c(0, frequence_max)
      )
  } else {
    niveaux_groupement <- names(group_labels[[groupement]])
    data_clean$groupe <- factor(
      data_clean[[groupement]],
      levels = niveaux_groupement
    )
    
    niveaux_absents <- setdiff(
      niveaux_groupement,
      unique(as.character(data_clean$groupe))
    )
    
    if (length(niveaux_absents) > 0) {
      faux_niveaux <- tibble(
        age = 0,
        groupe = factor(niveaux_absents, levels = niveaux_groupement)
      )
      
      data_clean <- bind_rows(data_clean, faux_niveaux)
    }
    
    ggplot(data_clean, aes(x = age, fill = groupe)) +
      geom_histogram(
        binwidth = 1,
        closed = "right",
        color = "white",
        position = position_stack(reverse = TRUE),
        na.rm = TRUE
      ) +
      labs(
        x = "Âge",
        y = paste0("Nb. ", nom_espece, " échantillonnés")
      ) +
      theme_aquapop() +
      scale_x_continuous(
        expand = c(0, 0),
        limits = c(0, age_max + 2),
        breaks = 0:(age_max + 2)
      ) +
      scale_y_continuous(
        expand = c(0, 0),
        limits = c(0, frequence_max)
      ) +
      scale_fill_manual(
        values = group_colors[[groupement]],
        name = "",
        labels = group_labels[[groupement]],
        drop = FALSE
      )
  }
  
  # Retour ----
  list(
    success = TRUE,
    plot = graphique_structure_age,
    data = age_counts,
    flextable = tableau_age,
    message = NULL
  )
}