#' Comparer plusieurs modèles de mortalité (Z) et recommander le meilleur
#'
#' Cette fonction ajuste cinq modèles statistiques (Poisson, NB1, NB2, CMP, GP)
#' sur les fréquences d'âge, puis retourne un tableau comparatif incluant :
#' le critère d'information corrigé (AICc), le pourcentage d'ajustement HNP,
#' l'estimation de Z, l'intervalle de confiance de A (%), et les commentaires
#' interprétatifs pour guider la sélection du meilleur modèle.
#'
#' La fonction retourne toujours une liste structurée contenant :
#' \itemize{
#'   \item `success` : indicateur logique de réussite
#'   \item `message` : message informatif si la comparaison est impossible ou partielle
#'   \item `data` : tableau comparatif des modèles
#'   \item `flextable` : tableau formaté pour affichage ou exportation
#' }
#'
#' @param data Un `data.frame` contenant les colonnes `age` et `number`,
#'   tel que produit par la fonction `mortalite_prepare_extended()`.
#'
#' @return Une liste contenant :
#' \describe{
#'   \item{success}{Un booléen indiquant si la comparaison a pu être réalisée.}
#'   \item{message}{Un message informatif si la comparaison est impossible ou partielle, sinon `NULL`.}
#'   \item{data}{Le tableau comparatif des modèles sous forme de `data.frame`, ou `NULL`.}
#'   \item{flextable}{Le même tableau formaté avec `flextable`, ou `NULL`.}
#' }
#'
#' @importFrom dplyr arrange bind_rows case_when mutate select
#' @importFrom flextable flextable set_caption
#'
#' @export
mortalite_compare_modele <- function(data) {
  # Validation de base ====
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    return(list(
      success = FALSE,
      message = "Aucune donnée n'est disponible pour comparer les modèles de mortalité.",
      data = NULL,
      flextable = NULL
    ))
  }
  
  if (!all(c("age", "number") %in% names(data))) {
    return(list(
      success = FALSE,
      message = "Le tableau doit contenir les colonnes `age` et `number`.",
      data = NULL,
      flextable = NULL
    ))
  }
  
  # Ajustement des modèles ====
  result_poisson <- mortalite_fit_modele_poisson(data)
  result_nb1 <- mortalite_fit_modele_nb1(data)
  result_nb2 <- mortalite_fit_modele_nb2(data)
  result_cmp <- mortalite_fit_modele_cmp(data)
  result_gp <- mortalite_fit_modele_gp(data)
  
  # Regroupement ====
  resultats <- bind_rows(
    result_poisson,
    result_nb1,
    result_nb2,
    result_cmp,
    result_gp
  )
  
  if (nrow(resultats) == 0) {
    return(list(
      success = FALSE,
      message = "Aucun modèle de mortalité n'a pu être ajusté.",
      data = NULL,
      flextable = NULL
    ))
  }
  
  # Calcul du delta AIC ====
  if (all(is.na(resultats$aicc))) {
    resultats <- resultats |>
      mutate(delta_aic = NA_real_)
  } else {
    meilleur_aicc <- min(resultats$aicc, na.rm = TRUE)
    
    resultats <- resultats |>
      mutate(
        delta_aic = case_when(
          is.na(aicc) ~ NA_real_,
          TRUE ~ round(aicc - meilleur_aicc, 2)
        )
      )
  }
  
  # Commentaires interprétatifs ====
  resultats <- resultats |>
    mutate(
      commentaire = case_when(
        convergence %in% FALSE ~ commentaire,
        !is.na(ajustement_hnp) & !is.na(delta_aic) & ajustement_hnp < 10 & delta_aic == 0 ~
          "Le modèle s'ajuste bien à vos données. Ce modèle est recommandé car son AICc est le plus faible.",
        !is.na(ajustement_hnp) & !is.na(delta_aic) & ajustement_hnp >= 10 & delta_aic == 0 ~
          "Le modèle ne s'ajuste pas bien à vos données. Il s'agit toutefois du meilleur modèle parmi les options disponibles.",
        TRUE ~ commentaire
      )
    )
  
  # Colonnes finales ====
  df_final <- resultats |>
    arrange(aicc) |>
    select(
      methode,
      ajustement_hnp,
      aicc,
      delta_aic,
      Z,
      SE,
      A,
      ic95,
      convergence,
      commentaire
    )
  
  # Tableau formaté ====
  ft <- flextable(df_final) |>
    set_caption("Comparaison des modèles de mortalité") |>
    style_flextable_aquapop()
  
  # Message global ====
  message <- NULL
  
  if (all(df_final$convergence %in% FALSE)) {
    message <- "Aucun des modèles de mortalité n'a convergé pour ce jeu de données."
  } else if (all(is.na(df_final$aicc))) {
    message <- "Les modèles ont été ajustés partiellement, mais aucun AICc n'a pu être calculé."
  }
  
  list(
    success = TRUE,
    message = message,
    data = df_final,
    flextable = ft
  )
}