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
#' @export
#'
#' @examples
#' p <- structure_taille(data = df, groupement = "sexe", format = "plot")
#' get_df_from_plot(p, groupement = "sexe")
get_df_from_plot <- function(plot, groupement) {
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
