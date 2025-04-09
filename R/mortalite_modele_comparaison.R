#' Comparer plusieurs modèles de mortalité (Z) et recommander le meilleur
#'
#' Ajuste cinq modèles (Poisson, NB1, NB2, CMP, GP) sur les fréquences d’âge
#' et retourne un tableau comparatif avec AICc, HNP, estimation de Z et A (%), etc.
#'
#' @param data Un `data.frame` contenant les colonnes `age` et `number`,
#'             tel que produit par la fonction `prepare_age_data_etendue()`.
#'
#' @return Une liste contenant :
#' \describe{
#'   \item{data}{Le tableau comparatif des modèles sous forme de `data.frame`}
#'   \item{flextable}{Le même tableau formaté avec `flextable` pour affichage}
#' }
#'
#' @export
#'
#' @examples
#' df_corr <- prepare_age_data_corrigee(...)
#' df_etendue <- prepare_age_data_etendue(df_corr, age_max = 10)
#' mort <- mortalite_modele_comparaison(df_etendue)
mortalite_modele_comparaison <- function(data) {
  
  # Ajustement des modèles
  result_poisson <- ajuster_modele_mortalite_poisson(data)
  result_nb1     <- ajuster_modele_mortalite_nb1(data)
  result_nb2     <- ajuster_modele_mortalite_nb2(data)
  result_cmp     <- ajuster_modele_mortalite_cmp(data)
  result_gp      <- ajuster_modele_mortalite_gp(data)
  
  # Regroupement
  resultats <- dplyr::bind_rows(result_poisson, result_nb1, result_nb2, result_cmp, result_gp)
  
  # Calcul du Δ AICc et ajustement du commentaire
  resultats <- resultats %>%
    dplyr::mutate(`Δ AICc` = round(aicc - min(aicc, na.rm = TRUE), 2)) %>%
    dplyr::mutate(commentaire = dplyr::case_when(
      ajustement_hnp < 10 & `Δ AICc` == 0 ~
        "Le modèle s’ajuste bien à vos données. Ce modèle est recommandé car son AICc est le plus faible.",
      ajustement_hnp >= 10 & `Δ AICc` == 0 ~
        "Le modèle ne s’ajuste pas bien à vos données. Il s’agit toutefois du meilleur modèle parmi les options disponibles.",
      TRUE ~ commentaire
    ))
  
  # Colonnes finales (data.frame)
  df_final <- resultats %>%
    dplyr::arrange(aicc) %>%
    dplyr::select(
      Méthode = methode,
      `Ajustement HNP (%)` = ajustement_hnp,
      AICc = aicc,
      `Δ AICc`,
      Z, SE, A, `IC 95%`,
      Convergence = convergence,
      Commentaires = commentaire
    )
  
  # Création du flextable
  ft <- flextable::flextable(df_final) |>
    flextable::set_caption("Comparaison des modèles de mortalité") |>
    flextable::fontsize(size = 12, part = "all") |>
    flextable::font(fontname = "Arial", part = "all") |>
    flextable::align(align = "center", part = "all") |>
    flextable::autofit()
  
  # Retourner les deux formats
  return(list(
    data = df_final,
    flextable = ft
  ))
}
