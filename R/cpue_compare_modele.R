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
#' @importFrom dplyr mutate select rename left_join arrange filter bind_rows if_else pull
#' @importFrom tidyr replace_na
#' @importFrom flextable flextable set_caption set_header_labels colformat_double
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
    filter(.data$ajustement_hnp < 10)
  
  # --- Calcul du delta AICc ---
  if (nrow(resultats_bien_ajustes) > 0) {
    min_aicc_reference <- min(resultats_bien_ajustes$aicc, na.rm = TRUE)
    
    resultats_final <- resultats_tous |>
      mutate(
        delta_aicc = if_else(
          .data$ajustement_hnp < 10,
          .data$aicc - min_aicc_reference,
          NA_real_
        )
      ) |>
      arrange(.data$ajustement_hnp >= 10, .data$aicc)
    
  } else {
    min_aicc_reference <- min(resultats_tous$aicc, na.rm = TRUE)
    
    resultats_final <- resultats_tous |>
      mutate(delta_aicc = .data$aicc - min_aicc_reference) |>
      arrange(.data$aicc)
  }
  
  # --- Mise à jour des commentaires pour le modèle recommandé ---
  if (nrow(resultats_bien_ajustes) > 0) {
    best_methodes <- resultats_final |>
      filter(.data$delta_aicc == 0) |>
      pull(.data$methode)
    
    resultats_final <- resultats_final |>
      mutate(
        commentaire = if_else(
          .data$methode %in% best_methodes,
          paste0(.data$commentaire, " Ce modèle est recommandé car son aicc est le plus faible."),
          .data$commentaire
        )
      )
  } else {
    best_methodes <- resultats_final |>
      filter(.data$delta_aicc == 0) |>
      pull(.data$methode)
    
    resultats_final <- resultats_final |>
      mutate(
        commentaire = if_else(
          .data$methode %in% best_methodes,
          paste0(.data$commentaire, " Il s'agit toutefois du meilleur modèle parmi les options disponibles."),
          .data$commentaire
        )
      )
  }
  
  # --- Sélection et renommage des colonnes finales ---
  tableau_final <- resultats_final |>
    select(
      "methode",
      "ajustement_hnp",
      "aicc",
      "delta_aicc",
      "cpue_moyenne",
      "ic_95",
      "convergence",
      "commentaire"
    ) |>
    rename(
      cpue = "cpue_moyenne",
      ic95 = "ic_95",
      commentaires = "commentaire"
    ) |>
    as.data.frame()
  

  # --- Création du titre dynamique ---
  titre_caption <- "Comparaison des modèles : tous les spécimens"
  if ("group" %in% names(cpue_data) && any(grepl("Femelles", cpue_data$group))) {
    titre_caption <- "Comparaison des modèles : femelles reproductrices actives"
  }
  
  # --- Création de la table formatée ---
  ft_final <- tableau_final |>
    mutate(convergence = ifelse(.data$convergence, "\u2713", "\u2717")) |>
    flextable() |>
    set_caption(titre_caption) |>
    set_header_labels(
      methode = "Modèle",
      ajustement_hnp = "Ajustement HNP",
      aicc = "AICc",
      delta_aicc = "Δ AICc",
      cpue = "CPUE moyenne",
      ic95 = "IC 95%",
      convergence = "Convergence",
      commentaires = "Commentaires"
    ) |>
    style_flextable_aquapop() |>
    colformat_double(j = c("aicc", "ajustement_hnp", "delta_aicc", "cpue"), digits = 2, decimal.mark = ",", na_str = "-", big.mark =  " ") 
  
  # --- Retour des résultats ---
  return(list(
    data = tableau_final,
    flextable = ft_final
  ))
}
