#' Comparer les modèles CPUE et recommander le meilleur
#'
#' Cette fonction ajuste cinq modèles (Poisson, NB1, NB2, CMP, GP) sur les données de CPUE.
#' Elle retourne un tableau comparatif des ajustements, des aicc, de la moyenne de CPUE
#' et des intervalles de confiance, tout en identifiant le meilleur modèle recommandé.
#'
#' @param cpue_data Un `data.frame` produit par `cpue_prepare()`, contenant au minimum
#' la colonne `cpue` et les identifiants des stations (`no_station`).
#'
#' @return Une liste contenant :
#' \describe{
#'   \item{data}{Un `data.frame` comparatif des modèles.}
#'   \item{flextable}{Une version formatée (`flextable`) pour exportation.}
#' }
#'
#' @importFrom dplyr mutate select left_join arrange filter bind_rows
#' @importFrom tidyr replace_na
#' @importFrom flextable flextable set_caption
#'
#' @export
cpue_compare_modele <- function(cpue_data) {
  # --- Ajustement des cinq modèles ---
  result_poisson <- cpue_fit_modele_poisson(cpue_data)
  result_nb1     <- cpue_fit_modele_nb1(cpue_data)
  result_nb2     <- cpue_fit_modele_nb2(cpue_data)
  result_cmp     <- cpue_fit_modele_cmp(cpue_data)
  result_gp      <- cpue_fit_modele_gp(cpue_data)
  
  # --- Fusion des résultats ---
  resultats_tous <- bind_rows(result_poisson, result_nb1, result_nb2, result_cmp, result_gp)
  
  # --- Sélection des modèles bien ajustés selon le test HNP (< 10 %) ---
  resultats_bien_ajustes <- resultats_tous |>
    filter(ajustement_hnp < 10)
  
  # --- Calcul du delta AICc ---
  if (nrow(resultats_bien_ajustes) > 0) {
    min_aicc_reference <- min(resultats_bien_ajustes$aicc, na.rm = TRUE)
    
    resultats_final <- resultats_tous |>
      mutate(
        delta_aicc = if_else(
          ajustement_hnp < 10,
          aicc - min_aicc_reference,
          NA_real_
        )
      ) |>
      arrange(ajustement_hnp >= 10, aicc)
    
  } else {
    min_aicc_reference <- min(resultats_tous$aicc, na.rm = TRUE)
    
    resultats_final <- resultats_tous |>
      mutate(delta_aicc = aicc - min_aicc_reference) |>
      arrange(aicc)
  }
  
  # --- Mise à jour des commentaires pour le modèle recommandé ---
  if (nrow(resultats_bien_ajustes) > 0) {
    best_methodes <- resultats_final |>
      filter(delta_aicc == 0) |>
      pull(methode)
    
    resultats_final <- resultats_final |>
      mutate(
        commentaire = if_else(
          methode %in% best_methodes,
          paste0(commentaire, " Ce modèle est recommandé car son aicc est le plus faible."),
          commentaire
        )
      )
  } else {
    best_methodes <- resultats_final |>
      filter(delta_aicc == 0) |>
      pull(methode)
    
    resultats_final <- resultats_final |>
      mutate(
        commentaire = if_else(
          methode %in% best_methodes,
          paste0(commentaire, " Il s’agit toutefois du meilleur modèle parmi les options disponibles."),
          commentaire
        )
      )
  }
  
  # --- Sélection et renommage des colonnes finales ---
  tableau_final <- resultats_final |>
    select(
      methode = methode,
      ajustement_hnp = ajustement_hnp,
      aicc = aicc,
      delta_aicc = delta_aicc,
      cpue = cpue_moyenne,
      ic95 = ic_95,
      commentaires = commentaire,
      convergence = convergence
    ) |>
    as.data.frame()
  
  # --- Formatage numérique conditionnel ---
  tableau_final <- tableau_final |>
    mutate(
      ajustement_hnp = ifelse(ajustement_hnp == 0, "0", format(round(ajustement_hnp, 2), nsmall = 2)),
      cpue = ifelse(cpue == 0, "0", format(round(cpue, 2), nsmall = 2)),
      aicc           = ifelse(aicc == 0, "0", format(round(aicc, 2), nsmall = 2)),
      delta_aicc     = ifelse(delta_aicc == 0, "0", format(round(delta_aicc, 2), nsmall = 2))
    )
  
  
  tableau_final <- set_variable_labels(
    tableau_final,
    methode = "Méthode",
    ajustement_hnp = "Ajustement HNP",
    aicc = "AICc",
    delta_aicc = "Δ AICc",
    cpue = "CPUE moyenne",
    ic95 = "IC 95 %",
    commentaires = "Commentaires",
    convergence = "Convergence"
  )
  

  # --- Création du titre dynamique ---
  titre_caption <- "Comparaison des modèles : tous les spécimens"
  if ("group" %in% names(cpue_data) && any(grepl("Femelles", cpue_data$group))) {
    titre_caption <- "Comparaison des modèles : femelles reproductrices actives"
  }
  
  # --- Création de la table formatée ---
  ft_final <- flextable(tableau_final) |>
    set_caption(titre_caption) |>
    labelled_data() |>
    style_flextable_aquapop()
  
  # --- Retour des résultats ---
  return(list(
    data = tableau_final,
    flextable = ft_final
  ))
}
