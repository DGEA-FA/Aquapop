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
#' @importFrom checkmate assert_character assert_choice assert_data_frame
#' @importFrom dplyr filter
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
 typ_pech_selected <- typ_pech
 no_lac_selected <- no_lac
 annee_selected <- annee
 
  
  # Chargement de checkmate (si nécessaire) -----
  requireNamespace("checkmate")
  
  # Validations explicites -----
  assert_data_frame(data)
  
  # --- Filtrage par type de pêche ---
  if (!is.null(typ_pech_selected)) {
    if (length(typ_pech_selected) != 1) {
      stop("Le filtre typ_pech doit contenir une seule valeur.")
    }
    assert_choice(typ_pech_selected, choices = c("PENT", "PENOF", "PENDJ"))
    data <- data |> filter(typ_pech == typ_pech_selected)
    
  }
  
  
  # --- Filtrage par numéro de lac ---
  if (!is.null(no_lac_selected)) {
    assert_character(no_lac_selected, any.missing = FALSE)
    data <- data |> filter(no_lac %in% no_lac_selected)
    
  }
  

  # --- Filtrage par année --d-
 if (!is.null(annee_selected) &&
     length(annee_selected) > 0) {
   data <- data |> filter(annee %in% as.integer(annee_selected))
 }
  
  # Suppression des niveaux inutilisés -----
  droplevels(data)
}
