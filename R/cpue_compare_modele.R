#' Comparer les modèles de CPUE et recommander le meilleur
#'
#' Cette fonction ajuste cinq modèles (Poisson, NB1, NB2, CMP, GP) sur les données de CPUE
#' et retourne une liste contenant un tableau comparatif brut (`data.frame`) et sa version formatée (`flextable`).
#'
#' @importFrom flextable set_caption
#' @param cpue_data Un `data.frame` produit par `cpue_prepare()` contenant les colonnes `no_station` et `CPUE`.
#'
#' @return Une liste avec deux éléments : `data` (tableau brut) et `flextable` (tableau formaté).
#' 
#' @importFrom flextable flextable
#' @importFrom tidyr replace_na
#' @importFrom dplyr mutate select left_join arrange mutate filter bind_rows
#' 
#' @export
cpue_compare_modele <- function(cpue_data) {
  # Ajustement des modèles
  result_poisson <- cpue_fit_modele_poisson(cpue_data)
  result_nb1     <- cpue_fit_modele_nb1(cpue_data)
  result_nb2     <- cpue_fit_modele_nb2(cpue_data)
  result_cmp     <- cpue_fit_modele_cmp(cpue_data)
  result_gp      <- cpue_fit_modele_gp(cpue_data)
  
  results <- bind_rows(result_poisson, result_nb1, result_nb2, result_cmp, result_gp)
  results_bien_ajuste <- results |> filter(ajustement_hnp < 10)
  
  if (nrow(results_bien_ajuste) > 0) {
    min_AICc <- min(results_bien_ajuste$aicc, na.rm = TRUE)
    results_bien_ajuste <- results_bien_ajuste |>
      mutate(delta_aicc = aicc - min_AICc) |>
      arrange(aicc)
  } else {
    min_AICc <- min(results$aicc, na.rm = TRUE)
    results <- results |>
      mutate(delta_aicc = aicc - min_AICc) |>
      arrange(aicc)
  }
  
  results <- results |>
    left_join(
      select(results_bien_ajuste, methode, delta_aicc),
      by = "methode"
    ) |>
    mutate(delta_aicc = replace_na(delta_aicc, NA_real_)) |>
    arrange(ajustement_hnp >= 10, aicc)
  
  # Mise à jour des commentaires
  if (nrow(results_bien_ajuste) > 0) {
    best <- results_bien_ajuste |> filter(delta_aicc == 0)
    results <- results |>
      mutate(commentaire = ifelse(
        methode == best$methode,
        paste0(commentaire, " Ce modèle est recommandé car son AICc est le plus faible."),
        commentaire
      ))
  } else {
    best <- results |> filter(delta_aicc == 0)
    results <- results |>
      mutate(commentaire = ifelse(
        methode == best$methode,
        paste0(commentaire, " Il s’agit toutefois du meilleur modèle parmi les options disponibles."),
        commentaire
      ))
  }
  
  # Colonnes finales
  df_final <- results |>
    select(
      Méthode = methode,
      `Ajustement (résultat du test HNP)` = ajustement_hnp,
      AICc = aicc,
      `Delta_AICc` = delta_aicc,
      CPUE = cpue_moyenne,
      `IC 95%` = ic_95,
      Commentaires = commentaire,
      Convergence = convergence
    ) |>
    as.data.frame()
  
  # Déterminer le titre
  titre_caption <- "Comparaison des modèles : tous les spécimens"
  if ("Group" %in% names(cpue_data) && any(grepl("Femelles", cpue_data$Group))) {
    titre_caption <- "Comparaison des modèles : femelles reproductrices actives"
  }
  
  # Créer le flextable
  ft_final <- flextable(df_final) |>
    set_caption(titre_caption) |>
    style_flextable_aquapop()
  
  return(list(
    data = df_final,
    flextable = ft_final
  ))
}
