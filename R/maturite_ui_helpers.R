#' Créer une carte UI pour une section de modèles de maturité
#'
#' Fonction interne pour structurer visuellement les sections Femelles, Mâles
#' et Combiné dans les modules de maturité.
#'
#' @param title Titre principal de la section.
#' @param subtitle Sous-titre explicatif optionnel.
#' @param ... Contenu UI à insérer dans la carte.
#'
#' @return Un objet UI `shiny.tag`.
#'
#' @keywords internal
#'
#' @importFrom shiny div tags
model_section_card_ui <- function(title, subtitle = NULL, ...) {
  div(
    style = paste(
      "background-color: #ffffff;",
      "border: 1px solid #d9e2ec;",
      "border-radius: 12px;",
      "padding: 18px 20px;",
      "margin: 0 0 22px 0;",
      "box-shadow: 0 1px 3px rgba(0, 0, 0, 0.06);"
    ),
    
    tags$h3(
      style = "margin-top: 0; margin-bottom: 6px;",
      title
    ),
    
    if (!is.null(subtitle)) {
      tags$p(
        style = "margin-top: 0; margin-bottom: 16px; color: #555;",
        subtitle
      )
    },
    
    ...
  )
}