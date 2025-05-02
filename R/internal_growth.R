#' Fonction de croissance de Von Bertalanffy
#'
#' Calcule la longueur en fonction de l’âge selon le modèle de Von Bertalanffy.
#'
#' @param age Âge du spécimen
#' @param linf Longueur asymptotique (mm)
#' @param k Taux de croissance
#' @param t0 Âge hypothétique à longueur nulle
#'
#' @return Longueur théorique à cet âge
#' @export
vb_function <- function(age, linf, k, t0) {
  linf * (1 - exp(-k * (age - t0)))
}

#' Fonction de croissance de Gompertz
#'
#' Calcule la longueur en fonction de l’âge selon le modèle de Gompertz.
#'
#' @param age Âge du spécimen
#' @param linf Longueur asymptotique (mm)
#' @param k Taux de croissance
#' @param t0 Âge à inflexion
#'
#' @return Longueur théorique à cet âge
#' @export
gompertz_function <- function(age, linf, k, t0) {
  linf * exp(-exp(-k * (age - t0)))
}

#' Fonction de croissance logistique
#'
#' Calcule la longueur en fonction de l’âge selon le modèle logistique.
#'
#' @param age Âge du spécimen
#' @param linf Longueur asymptotique (mm)
#' @param k Taux de croissance
#' @param t0 Âge à inflexion
#'
#' @return Longueur théorique à cet âge
#' @export
logistic_function <- function(age, linf, k, t0) {
  linf / (1 + exp(-k * (age - t0)))
}
