#' Comparer plusieurs modèles de mortalité (Z) et recommander le meilleur
#'
#' Ajuste cinq modèles (Poisson, NB1, NB2, CMP, GP) sur les fréquences d’âge
#' et retourne un tableau comparatif avec AICc, HNP, estimation de Z et A (%), etc.
#'
#' @importFrom flextable set_caption flextable
#' @importFrom dplyr select arrange case_when mutate bind_rows
#' @param data Un `data.frame` contenant les colonnes `age` et `number`,
#'             tel que produit par la fonction `mortalite_prepare_extended()`.
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
#' df_corr <- mortalite_prepare_corr(...)
#' df_etendue <- mortalite_prepare_extended(df_corr, age_max = 10)
#' mortalite_compare_modele_res <- mortalite_compare_modele(data = df_etendue)
mortalite_compare_modele <- function(data) {
  
  # Ajustement des modèles
  result_poisson <- mortalite_fit_modele_poisson(data)
  result_nb1     <- mortalite_fit_modele_nb1(data)
  result_nb2     <- mortalite_fit_modele_nb2(data)
  result_cmp     <- mortalite_fit_modele_cmp(data)
  result_gp      <- mortalite_fit_modele_gp(data)
  
  # Regroupement
  resultats <- bind_rows(result_poisson, result_nb1, result_nb2, result_cmp, result_gp)
  
  # Calcul du Δ AICc et ajustement du commentaire
  resultats <- resultats |>
    mutate(`Δ AICc` = round(aicc - min(aicc, na.rm = TRUE), 2)) |>
    mutate(commentaire = case_when(
      ajustement_hnp < 10 & `Δ AICc` == 0 ~
        "Le modèle s’ajuste bien à vos données. Ce modèle est recommandé car son AICc est le plus faible.",
      ajustement_hnp >= 10 & `Δ AICc` == 0 ~
        "Le modèle ne s’ajuste pas bien à vos données. Il s’agit toutefois du meilleur modèle parmi les options disponibles.",
      TRUE ~ commentaire
    ))
  
  # Colonnes finales (data.frame)
  df_final <- resultats |>
    arrange(aicc) |>
    select(
      Méthode = methode,
      `Ajustement HNP (%)` = ajustement_hnp,
      AICc = aicc,
      `Δ AICc`,
      Z, SE, A, `IC 95%`,
      Convergence = convergence,
      Commentaires = commentaire
    )
  
  # Création du flextable
  ft <- flextable(df_final) |>
    set_caption("Comparaison des modèles de mortalité") |>
    style_flextable_aquapop()
  
  # Retourner les deux formats
  return(list(
    data = df_final,
    flextable = ft
  ))
}
