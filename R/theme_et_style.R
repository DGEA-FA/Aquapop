# Conversion pixels → points (1 pt = 0.75 px)
px_to_pt <- function(px) px / 0.75

#' Thème ggplot standardisé AquaPop (style Quebec.ca)
#'
#' Applique un thème visuel conforme aux lignes directrices de présentation gouvernementale.
#' Utilise la police Google "Open Sans", des marges harmonisées et une palette neutre.
#'
#' @importFrom ggplot2 geom_text element_blank element_text element_line element_rect margin theme_void
#' @importFrom grid unit
#' @importFrom showtext showtext_auto
#' @importFrom sysfonts font_add_google font_families
#' @return Un objet `theme` à ajouter à un graphique ggplot2
#' @export
#'
#' @examples
#' ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
#'   ggplot2::geom_point() +
#'   theme_aquapop()
theme_aquapop <- function() {
  # Charger la police Open Sans depuis Google Fonts (si pas déjà chargée)
  if (!"open-sans" %in% font_families()) {
    font_add_google("Open Sans", "open-sans")
    showtext_auto()
  }
  
  theme_void() +
    theme(
      plot.margin = margin(
        t = px_to_pt(48), 
        r = px_to_pt(40), 
        # b = px_to_pt(48),
        b = px_to_pt(10), 
        l = px_to_pt(40), 
        unit = "pt"
      ),
      plot.background = element_rect(
        colour = "#c5cad2", linewidth = 1,
        fill = "#FFFFFF"),
      panel.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),
      panel.grid.major.y = element_line(linewidth = 0.5, colour = "#C5CAD2"),
      axis.line.x = element_line(linewidth = 1, colour = "#6B778A"),
      legend.position = "bottom",
      legend.justification = c(0, 0),
      axis.text.x = element_text(
        family = "open-sans", size = 14, color = "#6b778a",
        margin = margin(t = px_to_pt(8), b = px_to_pt(16), unit = "pt"),
        angle = 45, hjust = 1, vjust = 1
      ),
      axis.text.y = element_text(
        family = "open-sans", size = 14, color = "#6b778a",
        margin = margin(r = px_to_pt(8), l = px_to_pt(16), unit = "pt"),
        hjust = 0.5
      ),
      axis.title.x = element_text(
        family = "open-sans", size = 14, color = "#6b778a",
        margin = margin(b = px_to_pt(32), unit = "pt"),
        hjust = 0.5
      ),
      axis.title.y = element_text(
        family = "open-sans", size = 14, color = "#6b778a", angle = 90,
        hjust = 0.5
      ),
      legend.title = element_blank(),
      legend.text = element_text(
        margin = margin(r = px_to_pt(8)),
        family = "open-sans", size = 14, color = "#6b778a"
      ),
      legend.key.size = unit(px_to_pt(16), "pt")
    )
}


#' geom_text standardisé AquaPop
#'
#' Fonction utilitaire pour afficher des étiquettes texte harmonisées avec le thème AquaPop.
#'
#' @param ... Arguments passés à `geom_text()`
#'
#' @return Un objet ggplot layer
#' @importFrom ggplot2 geom_text
#' @export
geom_text_aquapop <- function(...) {
  geom_text(
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
#' Les nombres sont formatés avec une virgule comme séparateur décimal.
#'
#' @param ft Un objet `flextable`
#'
#' @return Un objet `flextable` stylisé
#' @export
#' @importFrom flextable flextable set_table_properties fontsize font align border_remove border_outer colformat_double colformat_int colformat_char colformat_lgl colformat_num
#' @examples
#' flextable::flextable(head(iris)) |> style_flextable_aquapop()
style_flextable_aquapop <- function(ft) {
  ft |>
    set_table_properties(layout = "autofit") |>
    fontsize(size = 10, part = "all") |>
    font(fontname = "Arial", part = "all") |>
    align(align = "center", part = "all") |>
    border_remove() |>
    border_outer() |>
    colformat_double(decimal.mark = ",", big.mark = " ", na_str = "-") |>
    colformat_num(decimal.mark = ",", big.mark = " ", na_str = "-") |>
    colformat_int(big.mark = " ", na_str = "-") |>
    colformat_lgl(na_str = "-") |>
    colformat_char(na_str = "-")
}
