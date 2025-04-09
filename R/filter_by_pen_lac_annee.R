#' Filtrer un jeu de données selon no_lac, typ_pech, annee
#'
#' @param data Jeu de données (stations, récolte ou spécimens)
#' @param no_lac Numéro(s) de lac à conserver (optionnel)
#' @param typ_pech  Type(s) de pêche à conserver (optionnel)
#' @param annee Année(s) à conserver (optionnel)
#'
#' @return Un data.frame filtré
#' @export
filter_by_pen_lac_annee <- function(data, typ_pech = NULL, no_lac = NULL, annee = NULL) {
  df <- data

  if (!is.null(typ_pech) && length(typ_pech) > 0) {
    df <- dplyr::filter(df, as.character(.data$typ_pech) %in% as.character(!!typ_pech))
  }

  if (!is.null(no_lac) && length(no_lac) > 0) {
    df <- dplyr::filter(df, as.character(.data$no_lac) %in% as.character(!!no_lac))
  }

  if (!is.null(annee) && length(annee) > 0) {
    annee_int <- as.integer(annee)  
    df <- dplyr::filter(df, .data$annee %in% annee_int)
  }

  droplevels(df)
}
