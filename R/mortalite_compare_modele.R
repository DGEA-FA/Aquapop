#' Comparer plusieurs modèles de mortalité (Z) et recommander le meilleur
#'
#' Cette fonction ajuste cinq modèles statistiques (Poisson, NB1, NB2, CMP, GP)
#' sur les fréquences d’âge, puis retourne un tableau comparatif incluant :
#' le critère d'information corrigé (AICc), le pourcentage d’ajustement HNP,
#' l’estimation de Z, l’intervalle de confiance de A (%), et les commentaires
#' interprétatifs pour guider la sélection du meilleur modèle.
#'
#' @param data Un `data.frame` contenant les colonnes `age` et `number`,
#'   tel que produit par la fonction `mortalite_prepare_extended()`.
#'
#' @return Une liste contenant :
#' \describe{
#'   \item{data}{Le tableau comparatif des modèles sous forme de `data.frame`}
#'   \item{flextable}{Le même tableau formaté avec `flextable` pour affichage ou exportation}
#' }
#'
#' @export
#' @importFrom dplyr select arrange case_when mutate bind_rows
#' @importFrom flextable flextable set_caption
mortalite_compare_modele <- function(data) {
  # --- Étape 1 : Ajustement des modèles ---
  result_poisson <- mortalite_fit_modele_poisson(data)
  result_nb1     <- mortalite_fit_modele_nb1(data)
  result_nb2     <- mortalite_fit_modele_nb2(data)
  result_cmp     <- mortalite_fit_modele_cmp(data)
  result_gp      <- mortalite_fit_modele_gp(data)
  
  # --- Étape 2 : Regroupement ---
  resultats <- bind_rows(result_poisson, result_nb1, result_nb2, result_cmp, result_gp)
  
  # --- Étape 3 : Calcul du Δ AICc et commentaires interprétatifs ---
  resultats <- resultats |>
    mutate(delta_aic = round(aicc - min(aicc, na.rm = TRUE), 2)) |>
    mutate(commentaire = case_when(
      ajustement_hnp < 10 & delta_aic == 0 ~
        "Le modèle s’ajuste bien à vos données. Ce modèle est recommandé car son AICc est le plus faible.",
      ajustement_hnp >= 10 & delta_aic == 0 ~
        "Le modèle ne s’ajuste pas bien à vos données. Il s’agit toutefois du meilleur modèle parmi les options disponibles.",
      TRUE ~ commentaire
    ))
  
  # --- Étape 4 : Colonnes finales (data.frame) ---
  df_final <- resultats |>
    arrange(aicc) |>
    select(
      methode,
      ajustement_hnp,
      aicc,
      delta_aic,
      .data$Z,
      .data$SE,
      .data$A,
      ic95,
      convergence,
      commentaire
    )
  
  # --- Étape 5 : Création du tableau formaté ---
  ft <- flextable(df_final) |>
    set_caption("Comparaison des modèles de mortalité") |>
    style_flextable_aquapop()
  
  # --- Étape 6 : Retourner les deux formats ---
  return(list(
    data = df_final,
    flextable = ft
  ))
}
