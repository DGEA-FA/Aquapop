#' Comparer les modèles de CPUE et recommander le meilleur
#'
#' Cette fonction ajuste cinq modèles (Poisson, NB1, NB2, CMP, GP) sur les données de CPUE
#' et retourne une liste contenant un tableau comparatif brut (`data.frame`) et sa version formatée (`flextable`).
#'
#' @param cpue_data Un `data.frame` produit par `cpue_prepare()` contenant les colonnes `no_station` et `CPUE`.
#'
#' @return Une liste avec deux éléments : `data` (tableau brut) et `flextable` (tableau formaté).
#' @export
cpue_compare_modele <- function(cpue_data) {
  # Ajustement des modèles
  result_poisson <- cpue_fit_modele_poisson(cpue_data)
  result_nb1     <- cpue_fit_modele_nb1(cpue_data)
  result_nb2     <- cpue_fit_modele_nb2(cpue_data)
  result_cmp     <- cpue_fit_modele_cmp(cpue_data)
  result_gp      <- cpue_fit_modele_gp(cpue_data)
  
  results <- dplyr::bind_rows(result_poisson, result_nb1, result_nb2, result_cmp, result_gp)
  results_bien_ajuste <- results %>% dplyr::filter(ajustement_hnp < 10)
  
  if (nrow(results_bien_ajuste) > 0) {
    min_AICc <- min(results_bien_ajuste$aicc, na.rm = TRUE)
    results_bien_ajuste <- results_bien_ajuste %>%
      dplyr::mutate(delta_aicc = aicc - min_AICc) %>%
      dplyr::arrange(aicc)
  } else {
    min_AICc <- min(results$aicc, na.rm = TRUE)
    results <- results %>%
      dplyr::mutate(delta_aicc = aicc - min_AICc) %>%
      dplyr::arrange(aicc)
  }
  
  results <- results %>%
    dplyr::left_join(
      dplyr::select(results_bien_ajuste, methode, delta_aicc),
      by = "methode"
    ) %>%
    dplyr::mutate(delta_aicc = tidyr::replace_na(delta_aicc, NA_real_)) %>%
    dplyr::arrange(ajustement_hnp >= 10, aicc)
  
  # Mise à jour des commentaires
  if (nrow(results_bien_ajuste) > 0) {
    best <- results_bien_ajuste %>% dplyr::filter(delta_aicc == 0)
    results <- results %>%
      dplyr::mutate(commentaire = ifelse(
        methode == best$methode,
        paste0(commentaire, " Ce modèle est recommandé car son AICc est le plus faible."),
        commentaire
      ))
  } else {
    best <- results %>% dplyr::filter(delta_aicc == 0)
    results <- results %>%
      dplyr::mutate(commentaire = ifelse(
        methode == best$methode,
        paste0(commentaire, " Il s’agit toutefois du meilleur modèle parmi les options disponibles."),
        commentaire
      ))
  }
  
  # Colonnes finales
  df_final <- results %>%
    dplyr::select(
      Méthode = methode,
      `Ajustement (résultat du test HNP)` = ajustement_hnp,
      AICc = aicc,
      `Delta_AICc` = delta_aicc,
      CPUE = cpue_moyenne,
      `IC 95%` = ic_95,
      Commentaires = commentaire,
      Convergence = convergence
    ) %>%
    as.data.frame()
  
  # Déterminer le titre
  titre_caption <- "Comparaison des modèles : tous les spécimens"
  if ("Group" %in% names(cpue_data) && any(grepl("Femelles", cpue_data$Group))) {
    titre_caption <- "Comparaison des modèles : femelles reproductrices actives"
  }
  
  # Créer le flextable
  ft_final <- flextable::flextable(df_final) |>
    flextable::set_caption(titre_caption) |>
    style_flextable_aquapop()
  
  return(list(
    data = df_final,
    flextable = ft_final
  ))
}
