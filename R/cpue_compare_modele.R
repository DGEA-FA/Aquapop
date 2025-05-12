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
  # --- Étape 1 : Ajustement des cinq modèles ---
  result_poisson <- cpue_fit_modele_poisson(cpue_data)
  result_nb1     <- cpue_fit_modele_nb1(cpue_data)
  result_nb2     <- cpue_fit_modele_nb2(cpue_data)
  result_cmp     <- cpue_fit_modele_cmp(cpue_data)
  result_gp      <- cpue_fit_modele_gp(cpue_data)
  
  # --- Étape 2 : Fusion des résultats ---
  resultats_tous <- bind_rows(result_poisson, result_nb1, result_nb2, result_cmp, result_gp)
  
  # --- Étape 3 : Sélection des modèles bien ajustés selon le test HNP (< 10 %) ---
  resultats_bien_ajustes <- resultats_tous |> filter(ajustement_hnp < 10)
  
  # --- Étape 4 : Calcul du delta AICc pour le(s) meilleur(s) modèle(s) ---
  if (nrow(resultats_bien_ajustes) > 0) {
    min_aicc <- min(resultats_bien_ajustes$aicc, na.rm = TRUE)
    resultats_bien_ajustes <- resultats_bien_ajustes |>
      mutate(delta_aicc = aicc - min_aicc) |>
      arrange(aicc)
  } else {
    min_aicc <- min(resultats_tous$aicc, na.rm = TRUE)
    resultats_tous <- resultats_tous |>
      mutate(delta_aicc = aicc - min_aicc) |>
      arrange(aicc)
  }
  
  # --- Étape 5 : Fusion du delta AICc dans tous les résultats ---
  resultats_final <- resultats_tous |>
    left_join(
      select(resultats_bien_ajustes, methode, delta_aicc),
      by = "methode"
    ) |>
    mutate(delta_aicc = replace_na(delta_aicc, NA_real_)) |>
    arrange(ajustement_hnp >= 10, aicc)
  
  # --- Étape 6 : Mise à jour des commentaires pour le modèle recommandé ---
  if (nrow(resultats_bien_ajustes) > 0) {
    best <- filter(resultats_bien_ajustes, delta_aicc == 0)
    resultats_final <- resultats_final |>
      mutate(commentaire = ifelse(
        methode == best$methode,
        paste0(commentaire, " Ce modèle est recommandé car son aicc est le plus faible."),
        commentaire
      ))
  } else {
    best <- filter(resultats_final, delta_aicc == 0)
    resultats_final <- resultats_final |>
      mutate(commentaire = ifelse(
        methode == best$methode,
        paste0(commentaire, " Il s’agit toutefois du meilleur modèle parmi les options disponibles."),
        commentaire
      ))
  }
  
  # --- Étape 7 : Sélection et renommage des colonnes finales ---
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
  
  # --- Étape 8 : Création du titre dynamique ---
  titre_caption <- "Comparaison des modèles : tous les spécimens"
  if ("group" %in% names(cpue_data) && any(grepl("Femelles", cpue_data$group))) {
    titre_caption <- "Comparaison des modèles : femelles reproductrices actives"
  }
  
  # --- Étape 9 : Création de la table formatée ---
  ft_final <- flextable(tableau_final) |>
    set_caption(titre_caption) |>
    style_flextable_aquapop()
  
  # --- Étape 10 : Retour des résultats ---
  return(list(
    data = tableau_final,
    flextable = ft_final
  ))
}
