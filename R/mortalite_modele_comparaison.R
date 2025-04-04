#' Comparer plusieurs modèles de mortalité (Z) et recommander le meilleur
#'
#' Ajuste cinq modèles (Poisson, NB1, NB2, CMP, GP) sur les fréquences d’âge
#' et retourne un tableau comparatif avec AICc, HNP, estimation de Z et A (%), etc.
#'
#' @param df_age_etendue Un `data.frame` produit par `prepare_age_data_etendue()`,
#'                       contenant les colonnes `age` et `number`.
#' @param format Format de sortie : `"data.frame"` (défaut) ou `"flextable"`.
#'
#' @return Un tableau comparatif des modèles.
#' @export
#'
#' @examples
#' df_corr <- prepare_age_data_corrigee(...)
#' df_etendue <- prepare_age_data_etendue(df_corr, age_max = 10)
#' mort <- mortalite_modele_comparaison(df_etendue, format = "flextable")
mortalite_modele_comparaison <- function(df_age_etendue, format = c("data.frame", "flextable")) {
  format <- match.arg(format)
  
  # Appel des fonctions d’ajustement spécifiques à chaque modèle
  result_poisson <- ajuster_modele_mortalite_poisson(df_age_etendue)
  result_nb1     <- ajuster_modele_mortalite_nb1(df_age_etendue)
  result_nb2     <- ajuster_modele_mortalite_nb2(df_age_etendue)
  result_cmp     <- ajuster_modele_mortalite_cmp(df_age_etendue)
  result_gp      <- ajuster_modele_mortalite_gp(df_age_etendue)
  
  # Regroupement
  resultats <- dplyr::bind_rows(result_poisson, result_nb1, result_nb2, result_cmp, result_gp)
  
  # Tri et calcul du Δ AICc
  resultats <- resultats %>%
    dplyr::mutate(`Δ AICc` = round(aicc - min(aicc, na.rm = TRUE), 2)) %>%
    dplyr::mutate(commentaire = dplyr::case_when(
      ajustement_hnp < 10 & `Δ AICc` == 0 ~
        "Le modèle s’ajuste bien à vos données. Ce modèle est recommandé car son AICc est le plus faible.",
      ajustement_hnp >= 10 & `Δ AICc` == 0 ~
        "Le modèle ne s’ajuste pas bien à vos données. Il s’agit toutefois du meilleur modèle parmi les options disponibles.",
      TRUE ~ commentaire
    ))
  
  # Colonnes finales
  df_final <- resultats %>%
    dplyr::select(
      Méthode = methode,
      `Ajustement HNP (%)` = ajustement_hnp,
      AICc = aicc,
      `Δ AICc`,
      Z, SE, A, `IC 95%`,
      Convergence = convergence,
      Commentaires = commentaire
    )%>%
    dplyr::arrange(aicc)  # <- Tri ici
  
  # Format de sortie
  if (format == "data.frame") {
    return(df_final)
  } else {
    return(
      flextable::flextable(df_final) |>
        flextable::set_caption("Comparaison des modèles de mortalité") |>
        flextable::fontsize(size = 12, part = "all") |>
        flextable::font(fontname = "Arial", part = "all") |>
        flextable::align(align = "center", part = "all") |>
        flextable::autofit()
    )
  }
}
