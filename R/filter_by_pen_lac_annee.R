#' Filtrer un jeu de données selon le type de pêche, le numéro de lac et l'année
#'
#' Cette fonction permet de filtrer un jeu de données (issu de `load_station()`, `load_recolte()` ou `load_specimen()`)
#' selon un ou plusieurs critères optionnels : `typ_pech`, `no_lac` et/ou `annee`.
#' Chaque filtre est appliqué uniquement s'il est fourni.
#'
#' @param data Jeu de données (stations, récolte ou spécimens), typiquement produit par une fonction `load_*()`.
#' @param no_lac (optionnel) Numéro(s) de lac à conserver (exactement 5 caractères chacun).
#' @param typ_pech (optionnel) Type de pêche à conserver (un seul parmi `"PENT"`, `"PENOF"`, `"PENDJ"`).
#' @param annee (optionnel) Année(s) à conserver (ex. `2022`, `2021:2023`). Peut être un vecteur.
#'
#' @return Un `data.frame` filtré selon les critères fournis, avec les niveaux de facteurs inutilisés supprimés.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   no_lac = c("01234", "F1234", "01234"),
#'   typ_pech = c("PENT", "PENOF", "PENDJ"),
#'   annee = c(2021, 2022, 2022)
#' )
#'
#' filter_by_pen_lac_annee(df, no_lac = "01234")
#' filter_by_pen_lac_annee(df, typ_pech = "PENT", annee = 2022)
#' }
#'
#' @export
filter_by_pen_lac_annee <- function(data, typ_pech = NULL, no_lac = NULL, annee = NULL) {

  
  # Chargement de checkmate (si nécessaire) -----
  requireNamespace("checkmate")
  
  # Validations explicites -----
  checkmate::assert_data_frame(data)
  checkmate::assert_choice(typ_pech, choices = c("PENT", "PENOF", "PENDJ"), null.ok = TRUE)
  checkmate::assert_character(no_lac, any.missing = FALSE, min.chars = 5, max.chars = 5, null.ok = TRUE)
  
  # Filtrage par type de pêche -----
  if (!is.null(typ_pech) && length(typ_pech) > 0) {
    data <- dplyr::filter(data, .data$typ_pech %in% typ_pech)
  }

  # Filtrage par numéro de lac -----
  if (!is.null(no_lac) && length(no_lac) > 0) {
    data <- dplyr::filter(data, as.character(.data$no_lac) %in% as.character(!!no_lac))
  }

  # Filtrage par année -----
  if (!is.null(annee) && length(annee) > 0) {
    annee_int <- as.integer(annee)  
    data <- dplyr::filter(data, .data$annee %in% annee_int)
  }
  
  # Suppression des niveaux inutilisés -----
  droplevels(data)
}
