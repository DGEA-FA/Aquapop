#' Calculer le ratio mâles:femelles sous forme simplifiée
#'
#' Cette fonction prend en entrée un nombre de mâles et de femelles, puis retourne un
#' ratio \eqn{M:F} (mâles pour femelles) sous forme réduite à ses plus simples expressions,
#' comme `"3:2"` ou `"1:1"`. Si les deux valeurs sont nulles, la fonction retourne `NA`.
#'
#' @param male_count Nombre d’individus de sexe masculin (entier)
#' @param female_count Nombre d’individus de sexe féminin (entier)
#'
#' @return Une chaîne de caractères représentant le ratio simplifié (ex: `"3:2"`), ou `NA_character_` si les deux valeurs sont nulles.
#'
#' @examples
#' calculate_mf_ratio(6, 4)   # Retourne "3:2"
#' calculate_mf_ratio(5, 5)   # Retourne "1:1"
#' calculate_mf_ratio(0, 0)   # Retourne NA
#' calculate_mf_ratio(0, 7)   # Retourne "0:1"
#'
#' @export
calculate_mf_ratio <- function(male_count, female_count) {
  if (male_count == 0 && female_count == 0) {
    return(NA_character_)  # Retourne NA explicite de type character
  }
  # Simplifier le ratio avec fractions()
  ratio <- MASS::fractions(c(male_count, female_count))
  return(paste0(ratio[1], ":", ratio[2]))
}
