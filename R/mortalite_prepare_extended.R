#' Étendre artificiellement les données de fréquence d'âge avec des zéros
#'
#' Cette fonction ajoute des classes d'âge fictives avec un nombre de captures nul (`number = 0`)
#' au-delà de l'âge maximal observé, jusqu'à trois fois cet âge. Cette étape permet d'améliorer
#' l'ajustement de certains modèles de mortalité (voir Mainguy & Moral, 2021).
#'
#' @param df_corrigee Un `data.frame` contenant les colonnes `age` (âge) et `number` (fréquence),
#' généralement produit par la fonction `mortalite_prepare_corr()`.
#' @param age_max Un entier indiquant l'âge maximal observé (souvent issu de `mortalite_get_age_max()`).
#'
#' @return Un `data.frame` combinant les âges observés avec les âges fictifs ajoutés, avec `number = 0`.
#' Les lignes sont triées par âge croissant.
#'
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows arrange
#' @export
#'
#' @examples
#' df <- data.frame(age = 2:5, number = c(4, 3, 2, 1))
#' mortalite_prepare_extended(df, age_max = 5)
mortalite_prepare_extended <- function(df_corrigee, age_max) {
  # --- Validation des colonnes requises ---
  if (!all(c("age", "number") %in% names(df_corrigee))) {
    stop("Le data.frame `df_corrigee` doit contenir les colonnes `age` et `number`.")
  }
  
  if (!is.numeric(age_max) || length(age_max) != 1 || age_max < 0) {
    stop("`age_max` doit être un nombre numérique positif.")
  }
  
  # --- Génération des âges fictifs jusqu'à 3 × âge max ---
  ages_fictifs <- tibble(
    age    = (age_max + 1):(age_max * 3),
    number = 0
  )
  
  # --- Fusion et tri des données ---
  df_etendue <- bind_rows(df_corrigee, ages_fictifs) |>
    arrange(age)
  
  return(df_etendue)
}
