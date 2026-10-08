#' Comparer plusieurs modèles de mortalité (Z) et recommander le meilleur
#'
#' Cette fonction ajuste cinq modèles statistiques (Poisson, NB1, NB2, CMP, GP)
#' sur les fréquences d'âge, puis retourne un tableau comparatif incluant :
#' le critère d'information corrigé (AICc), le pourcentage d'ajustement HNP,
#' l'estimation de Z, l'intervalle de confiance de A (%), le poids d'Akaike et
#' les commentaires interprétatifs pour guider la sélection du meilleur modèle.
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
#' @importFrom flextable flextable set_caption set_header_labels colformat_double
#'
#' @export
mortalite_compare_modele <- function(data, pp_selected) {
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
  
  table_poisson <- result_poisson$tableau
  table_nb1 <- result_nb1$tableau
  table_nb2 <- result_nb2$tableau
  table_cmp <- result_cmp$tableau
  table_gp <- result_gp$tableau
  
  
  # Regroupement ====
  resultats <- bind_rows(
    result_poisson$tableau,
    result_nb1$tableau,
    result_nb2$tableau,
    result_cmp$tableau,
    result_gp$tableau
  )|>
    mutate(age_depart = pp_selected)
  
  if (nrow(resultats) == 0) {
    return(list(
      success = FALSE,
      message = "Aucun modèle de mortalité n'a pu être ajusté.",
      data = NULL,
      flextable = NULL
    ))
  }
  
  # --- Sélection des modèles bien ajustés selon le test HNP (< 10 %) ---
  resultats_bien_ajustes <- resultats |>
    filter(.data$ajustement_hnp < 10)
  
  # Calcul du delta AIC et poids d'Akaike ====
  if (all(is.na(resultats$aicc))) {
    resultats <- resultats |>
      mutate(delta_aic = NA_real_,
             aiccwt = NA_real_
             )
  } else {
    meilleur_aicc <- min(resultats$aicc, na.rm = TRUE)
    
    resultats <- resultats |>
      mutate(
        delta_aic = if_else(
          is.na(.data$aicc),
          NA_real_,
          .data$aicc - meilleur_aicc
        )
      )
    
    somme_aiccwt <- sum(
      exp(-0.5 * resultats$delta_aic),
      na.rm = TRUE
    )
    
    resultats <- resultats |>
      mutate(
        aiccwt = if_else(
          is.na(.data$delta_aic),
          NA_real_,
          exp(-0.5 * .data$delta_aic) / somme_aiccwt
        ),
        delta_aic = round(.data$delta_aic, 2),
        aiccwt = round(.data$aiccwt, 4)
      )
  }
  
  
  # Commentaires interprétatifs ====
  if (nrow(resultats_bien_ajustes) > 0) {
    
    resultats <- resultats |>
    mutate(
      commentaire = case_when(
        
        .data$ajustement_hnp < 10 & .data$delta_aic < 2 ~ paste0(
          .data$commentaire, " Soutien élevé."
        ),
        
        .data$ajustement_hnp < 10 & .data$delta_aic < 7 ~ paste0(
          .data$commentaire, " Soutien moyen."
        ),
        .data$ajustement_hnp < 10 ~ paste0(
          .data$commentaire, " Soutien faible."
        ),
        
        TRUE ~ .data$commentaire
        
      )
    )
    
  } else {
        
    resultats <- resultats |>
      mutate(
        commentaire = if_else(
          .data$delta_aicc < 2,
          paste0(.data$commentaire, " Il fait toutefois partie des meilleurs modèles disponibles."),
          .data$commentaire
        )
      )
  }    
    
 
  
  
  # Colonnes finales ====
  df_final <- resultats |>
    arrange(.data$aicc) |>
    select(
      "methode",
      "age_depart",
      "ajustement_hnp",
      "aicc",
      "delta_aic",
      "Z",
      "SE",
      "A",
      "ic95",
      "aiccwt",
      "convergence",
      "commentaire"
    )
  
  # Tableau formaté ====
  ft <- flextable(df_final) |>
    set_caption("Comparaison des modèles de mortalité") |>
    set_header_labels(
      methode   = "Modèle",
      age_depart = "Âge de départ",
      ajustement_hnp = "Ajustement HNP",
      aicc  = "AICc",
      delta_aic     = "Δ AICc",
      Z   = "Z",
      SE = "SE",
      A  = "A (%)",
      ic95   = "IC 95%",
      aiccwt = "Poids d’Akaike",
      convergence = "Convergence",
      commentaire  = "Commentaires"
    ) |>
    style_flextable_aquapop() |>
    colformat_double(j = "aicc", digits = 2,decimal.mark = ",", big.mark = " ", na_str = "-" ) |>
    colformat_double(j = "aiccwt", digits = 3,decimal.mark = ",", big.mark = " ", na_str = "-" )  
  

  # Message global ====
  message <- NULL
  
  if (all(df_final$convergence %in% FALSE)) {
    message <- "Aucun des modèles de mortalité n'a convergé pour ce jeu de données."
  } else if (all(is.na(df_final$aicc))) {
    message <- "Les modèles ont été ajustés partiellement, mais aucun AICc n'a pu être calculé."
  }
  
  # Graphique HNP ====
  
  graph_hnp_par_modele <- list(
    poisson = result_poisson$graph_hnp,
    nb1 = result_nb1$graph_hnp,
    nb2 = result_nb2$graph_hnp,
    cmp = result_cmp$graph_hnp,
    gp = result_gp$graph_hnp
  )
  
  best_model <- mortalite_select_best_modele(resultats)
  
  graph_hnp <- if (!is.null(best_model)) {
    graph_hnp_par_modele[[best_model]]
  } else {
    NULL
  }
  
  
  return(list(
    success = TRUE,
    message = message,
    data = df_final,
    flextable = ft,
    best_model = best_model,
    graph_hnp = graph_hnp,
    graph_hnp_par_modele = graph_hnp_par_modele
  ))
  
}

