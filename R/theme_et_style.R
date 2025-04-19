# Conversion pixels → points (1 pt = 0.75 px)
px_to_pt <- function(px) px / 0.75

#' Thème ggplot standardisé AquaPop (style Quebec.ca)
#'
#' Applique un thème visuel conforme aux lignes directrices de présentation gouvernementale.
#' Utilise la police Google "Open Sans", des marges harmonisées et une palette neutre.
#'
#' @return Un objet `theme` à ajouter à un graphique ggplot2
#' @export
#'
#' @examples
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   theme_aquapop()
theme_aquapop <- function() {
  # Charger la police Open Sans depuis Google Fonts (si pas déjà chargée)
  if (!"open-sans" %in% sysfonts::font_families()) {
    sysfonts::font_add_google("Open Sans", "open-sans")
    showtext::showtext_auto()
  }
  
  ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(
        t = px_to_pt(48), 
        r = px_to_pt(40), 
        # b = px_to_pt(48),
        b = px_to_pt(10), 
        l = px_to_pt(40), 
        unit = "pt"
      ),
      plot.background = ggplot2::element_rect(
        colour = "#c5cad2", linewidth = 1,
        fill = "#FFFFFF"),
      panel.background = ggplot2::element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),
      panel.grid.major.y = ggplot2::element_line(linewidth = 0.5, colour = "#C5CAD2"),
      axis.line.x = ggplot2::element_line(linewidth = 1, colour = "#6B778A"),
      legend.position = "bottom",
      legend.justification = c(0, 0),
      axis.text.x = ggplot2::element_text(
        family = "open-sans", size = 14, color = "#6b778a",
        margin = ggplot2::margin(t = px_to_pt(8), b = px_to_pt(16), unit = "pt"),
        angle = 45, hjust = 1, vjust = 1
      ),
      axis.text.y = ggplot2::element_text(
        family = "open-sans", size = 14, color = "#6b778a",
        margin = ggplot2::margin(r = px_to_pt(8), l = px_to_pt(16), unit = "pt"),
        hjust = 0.5
      ),
      axis.title.x = ggplot2::element_text(
        family = "open-sans", size = 14, color = "#6b778a",
        margin = ggplot2::margin(b = px_to_pt(32), unit = "pt"),
        hjust = 0.5
      ),
      axis.title.y = ggplot2::element_text(
        family = "open-sans", size = 14, color = "#6b778a", angle = 90,
        hjust = 0.5
      ),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(
        margin = ggplot2::margin(r = px_to_pt(8)),
        family = "open-sans", size = 14, color = "#6b778a"
      ),
      legend.key.size = grid::unit(px_to_pt(16), "pt")
    )
}


#' geom_text standardisé AquaPop
#'
#' Fonction utilitaire pour afficher des étiquettes texte harmonisées avec le thème AquaPop.
#'
#' @param ... Arguments passés à `ggplot2::geom_text()`
#'
#' @return Un objet ggplot layer
#' @export
geom_text_aquapop <- function(...) {
  ggplot2::geom_text(
    family = "open-sans",
    size = 4.5, # approx. 14 pt, mais en unités ggplot (≈ mm)
    color = "#6b778a",
    ...
  )
}

#' Style flextable standardisé pour le projet AquaPop
#'
#' Applique un style professionnel et uniforme à un objet flextable, avec
#' police Arial, texte centré, bordures simples et taille de police adaptée aux rapports.
#'
#' @param ft Un objet `flextable`
#'
#' @return Un objet `flextable` stylisé
#' @export
#'
#' @examples
#' library(flextable)
#' flextable(head(iris)) %>% style_flextable_aquapop()
style_flextable_aquapop <- function(ft) {
  ft %>%
    flextable::set_table_properties(layout = "autofit") %>%
    flextable::fontsize(size = 10, part = "all") %>%
    flextable::font(fontname = "Arial", part = "all") %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::border_remove() %>%
    flextable::border_outer()
}
