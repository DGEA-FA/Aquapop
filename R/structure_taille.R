#' Génère la structure de taille des spécimens
#'
#' Produit un histogramme de la structure de taille d'une espèce donnée, ainsi que
#' le tableau de données associé (brut et formaté). Les classes de taille sont
#' construites automatiquement selon les paramètres définis pour l'espèce.
#'
#' Si aucune donnée exploitable n'est disponible (ex. : aucun spécimen ou toutes
#' les longueurs sont manquantes), la fonction retourne un objet structuré avec
#' `success = FALSE`, sans générer d'erreur.
#'
#' @param data Un `data.frame` contenant les spécimens pour une seule espèce.
#'   Doit contenir minimalement les colonnes `sp` et `ltm`.
#' @param groupement Chaîne de caractères indiquant le groupement de couleur à utiliser
#'   dans le graphique. Valeurs possibles : `"tous"`, `"marquage"`, `"sexe"` ou `"maturite"`.
#'
#' @return Une liste contenant :
#' \describe{
#'   \item{success}{Booléen indiquant si le graphique a pu être produit}
#'   \item{plot}{Objet `ggplot` représentant l'histogramme, ou `NULL` si non disponible}
#'   \item{data}{Tableau brut (`data.frame`) correspondant aux données du graphique}
#'   \item{flextable}{Tableau formaté avec `flextable`, prêt à être affiché ou exporté}
#'   \item{message}{Message explicatif si l'analyse n'est pas disponible}
#' }
#'
#' @importFrom flextable flextable set_caption
#' @importFrom rlang sym
#' @importFrom ggplot2 ggplot geom_bar labs aes scale_x_discrete scale_y_continuous position_stack scale_fill_manual ggplot_build
#' @importFrom tibble tibble
#' @importFrom utils head tail
#' @importFrom dplyr filter mutate select
#'
#' @examples
#' data_exemple <- data.frame(
#'   sp = rep("SANA", 5),
#'   ltm = c(300, 320, 340, 360, 380),
#'   sexe = c("F", "M", "F", "M", "F"),
#'   maturite = c("O", "N", "O", "N", "O"),
#'   marquage = c("NMA", "NMA", "NMA", "NMA", "NMA")
#' )
#'
#' res <- structure_taille(data_exemple, groupement = "sexe")
#'
#' if (res$success) {
#'   res$data
#'   res$flextable
#' }
#'
#' @export
structure_taille <- function(data,
                             groupement = "tous") {
  # Cas sans ligne ----
  if (nrow(data) == 0) {
    return(list(
      success = FALSE,
      plot = NULL,
      data = NULL,
      flextable = NULL,
      message = "Aucun spécimen valide disponible pour produire la structure de taille."
    ))
  }
  
  # Vérifications
  espece <- as.character(unique(data$sp))
  if (length(espece) != 1) stop("Les données doivent contenir une seule espèce.")
  
  info <- get_info_pen(espece)
  if (is.null(info)) stop("Espèce non reconnue.")
  
  nomsp <- info$nom_sp
  binwidth <- info$binwidth
  
  data <- data |>
    mutate(ltm = as.numeric(.data$ltm)) |>
    filter(!is.na(.data$ltm))
  
  # Cas sans donnée exploitable ----
  if (nrow(data) == 0) {
    return(list(
      success = FALSE,
      plot = NULL,
      data = NULL,
      flextable = NULL,
      message = "Aucun spécimen valide disponible pour produire la structure de taille."
    ))
  }
  
  # Création des intervalles
  max_ltm <- max(data$ltm, na.rm = TRUE)
  breaks <- seq(0, max_ltm + binwidth, by = binwidth)
  labels <- paste0("[", head(breaks, -1), "-", tail(breaks, -1), "[")
  data$ltm_interval <- cut(data$ltm, breaks = breaks, include.lowest = TRUE, right = FALSE, labels = labels)
  data$ltm_interval <- factor(data$ltm_interval, levels = labels, ordered = TRUE)
  data <- filter(data, !is.na(.data$ltm_interval))
  
  # Cas sans classe de taille exploitable ----
  if (nrow(data) == 0) {
    return(list(
      success = FALSE,
      plot = NULL,
      data = NULL,
      flextable = NULL,
      message = "Aucune classe de taille exploitable n'a pu être produite."
    ))
  }
  
  max_y <- ceiling(max(table(data$ltm_interval), na.rm = TRUE) * 1.1)
  
  # Préparation du graphique
  if (groupement == "tous") {
    plt <- ggplot(data, aes(x = .data$ltm_interval)) +
      geom_bar(fill = couleur_default, color = "white", alpha = 1, na.rm = TRUE) +
      labs(x = "Longueur totale maximale (mm)", y = paste0("Nb. ", nomsp, " échantillonnés")) +
      theme_aquapop() +
      scale_x_discrete(drop = FALSE, limits = labels) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, max_y))
  } else {
    if (!groupement %in% names(group_labels) || !groupement %in% names(group_colors)) {
      stop("Groupement non reconnu. Choisir parmi 'tous', 'sexe', 'maturite', 'marquage'")
    }
    
    data[[groupement]] <- factor(data[[groupement]], levels = names(group_labels[[groupement]]), ordered = TRUE)
    
    df_legende <- tibble(
      categorie = factor(names(group_labels[[groupement]]), levels = names(group_labels[[groupement]])),
      label = unname(group_labels[[groupement]]),
      color = unname(group_colors[[groupement]])
    )
    
    plt <- ggplot(data, aes(x = .data$ltm_interval, fill = !!sym(groupement))) +
      geom_bar(position = position_stack(reverse = TRUE), color = "white", na.rm = TRUE) +
      geom_bar(data = df_legende, aes(x = .data$categorie, fill = .data$categorie),
               alpha = 1, width = 0, show.legend = TRUE, na.rm = TRUE) +
      labs(x = "Longueur totale maximale (mm)", y = paste0("Nb. ", nomsp, " échantillonnés")) +
      theme_aquapop() +
      scale_x_discrete(drop = FALSE, limits = labels) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, max_y)) +
      scale_fill_manual(
        values = setNames(df_legende$color, df_legende$categorie),
        name = "",
        labels = setNames(df_legende$label, df_legende$categorie),
        drop = FALSE
      )
  }
  
  # Tableau associé
  df <- structure_taille_extraire_donnees(plt, groupement)
  ft <- flextable(df) |>
    set_caption("Structure de taille") |>
    style_flextable_aquapop()
  
  return(list(
    success = TRUE,
    plot = plt,
    data = df,
    flextable = ft,
    message = NULL
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
#'
#' @keywords internal
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
    select("fill", "count", "x") |>
    mutate(categorie = fill_to_category[.data$fill])
  
  # Résultat final
  temp |> select("categorie", "count", "x")
}