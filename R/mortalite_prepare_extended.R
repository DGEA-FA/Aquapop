#' Étendre artificiellement les données de fréquence d'âge avec des zéros
#'
#' Cette fonction ajoute des classes d’âge fictives avec un nombre de captures nul (`number = 0`)
#' au-delà de l’âge maximal observé, jusqu’à trois fois cet âge. Cette étape est décrite
#' dans Mainguy et Moral (2021) et permet d’améliorer l’ajustement des modèles.
#'
#' @param df_corrigee Un `data.frame` avec les colonnes `age` et `number` produit par
#'                    `mortalite_prepare_corr()`.
#' @param age_max Âge maximal observé (typiquement obtenu avec `mortalite_get_age_max()`).
#'
#' @return Un `data.frame` contenant les âges observés + les âges étendus avec `number = 0`.
#' @export
#'
#' @examples
#' df_etendue <- mortalite_prepare_extended(df_corrigee, age_max = 10)
#' print(df_etendue)
mortalite_prepare_extended <- function(df_corrigee, age_max) {
  stopifnot(all(c("age", "number") %in% names(df_corrigee)))
  
  # Étendre jusqu’à 3 × âge max avec des zéros
  ages_fictifs <- tibble::tibble(
    age = (age_max + 1):(age_max * 3),
    number = 0
  )
  
  df_etendue <- dplyr::bind_rows(df_corrigee, ages_fictifs) %>%
    dplyr::arrange(age)
  
  return(df_etendue)
}
