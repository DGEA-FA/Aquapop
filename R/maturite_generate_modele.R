#' Ajuste un modèle de maturité (L50 ou A50)
#'
#' Cette fonction ajuste un modèle de maturité en fonction du type de modèle (`TLO`, `ADD`, `COM`, `INT`) 
#' et du lien (`logit`, `probit`, `cloglog`) spécifiés. Elle retourne un tableau de résultats, 
#' un graphique de l'ogive de reproduction, un commentaire sur l'ajustement, et les prédictions associées.
#'
#' @param data Jeu de données contenant les colonnes `maturite`, `sexe`, et `ltm` ou `age`
#' @param variable Variable quantitative à utiliser : `"ltm"` (par défaut) ou `"age"`
#' @param modele Type de modèle : `"TLO"`, `"ADD"`, `"COM"` ou `"INT"`
#' @param lien Lien à utiliser dans le glm : `"probit"` (par défaut), `"logit"`, `"cloglog"`
#'
#' @return Une liste avec :
#' \describe{
#'   \item{success}{Indique si le modèle a pu être ajusté}
#'   \item{message}{Message explicatif si l'analyse n'est pas disponible}
#'   \item{graphique}{Ogive de reproduction}
#'   \item{donnees_ogive}{Données prédictives}
#' }
#'
#' @export
#' @importFrom stats glm binomial predict anova coef update plogis pnorm
#' @importFrom glue glue
#' @importFrom dplyr bind_rows filter mutate pull
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon geom_point labs annotate theme
#' @importFrom FSA Summarize

maturite_generate_modele <- function(data,
                                     variable = c("ltm", "age"),
                                     modele = c("TLO", "ADD", "COM", "INT"),
                                     lien = c("probit", "logit", "cloglog"),
                                     sexe = NULL,
                                     modele_glm = NULL) {
  
  # Validation des arguments optionnels ----
  if (is.null(modele) || is.null(lien) || is.null(variable)) {
    return(list(
      success = FALSE,
      message = "Modèle non disponible pour ce groupe.",
      commentaire = NULL,
      table_resultats = NULL,
      table_resultats_flextable = NULL,
      graphique = NULL,
      donnees_ogive = NULL
    ))
  }
  
  variable <- match.arg(variable)
  modele <- match.arg(modele)
  lien <- match.arg(lien)
  
  if (!all(c(variable, "maturite", "sexe") %in% names(data))) {
    stop(glue("❌ Le jeu de données doit contenir les colonnes `{variable}`, `maturite` et `sexe`."))
  }
  
  # Validation minimale des cas limites ----
  validation_res <- maturite_validate_data(
    specimen_data = data,
    variable = variable)
  
  if (!isTRUE(validation_res$success)) {
    return(list(
      success = FALSE,
      table_resultats = NULL,
      table_resultats_flextable = NULL,
      commentaire = NULL,
      message = validation_res$message,
      graphique = NULL,
      donnees_ogive = NULL
    ))
  }
  
  donnees_modeles <- maturite_prepare(data, variable = variable)
  
  if (!is.null(sexe)) {
    
    donnees_modeles <- donnees_modeles |>
      dplyr::filter(.data$sexe == .env$sexe)
    
  }
  
  # Ajustement du modèle ----------------------------------------------------
  
  if (is.null(modele_glm)) {
  
    # Refit seulement lorsque le modèle glm n'est pas fourni.
    # Normalement les modules L50/A50 transmettent déjà modele_glm.
      
        modele_glm <- glm(
        formula = maturite_get_formule(
          modele = modele,
          variable = variable
        ),
        family = binomial(link = lien),
        data = donnees_modeles
      )
  }  
  
  # Application des fonctions d'ajustement et d'évaluation du modèle
  
  stats_mod <- maturite_extract_resultats_modele(
    mod = modele_glm,
    id = paste(modele, lien, sep = "_")
  )
  
  if (modele == "TLO") {
    
    point50 <- stats_mod$point50
    point50_inf <- stats_mod$point50_IC95_inf
    point50_sup <- stats_mod$point50_IC95_sup
    
  } else {
    
    point50 <- list(
      fem = stats_mod$point50_F,
      male = stats_mod$point50_M
    )
    
    point50_inf <- NA_real_
    
    point50_sup <- NA_real_
  }
  
  # Calcul des prédictions pour la courbe ---------------------------------------
  if (modele == "TLO") {
    x_min <- min(donnees_modeles[[variable]], na.rm = TRUE)
    x_max <- max(donnees_modeles[[variable]], na.rm = TRUE)
    donnees_prediction <- data.frame(temp = seq(from = x_min, to = x_max, by = 1))
    names(donnees_prediction) <- variable
  } else {
    form <- stats::as.formula(glue("{variable} ~ sexe"))
    var_minmax <- Summarize(form, data = donnees_modeles)
    donnees_prediction <- bind_rows(
      data.frame(sexe = "F", temp = seq(from = var_minmax$min[var_minmax$sexe == "F"], to = var_minmax$max[var_minmax$sexe == "F"], by = 1)),
      data.frame(sexe = "M", temp = seq(from = var_minmax$min[var_minmax$sexe == "M"], to = var_minmax$max[var_minmax$sexe == "M"], by = 1))
    )
    names(donnees_prediction)[names(donnees_prediction) == "temp"] <- variable
  }
  
  prediction_model <- predict(modele_glm, donnees_prediction, full = TRUE, type = "link", se.fit = TRUE)
  
  donnees_ogive <- donnees_prediction |>
    mutate(
      maturite = switch(lien,
                        probit = pnorm(prediction_model$fit),
                        logit = plogis(prediction_model$fit),
                        cloglog = 1 - exp(-exp(prediction_model$fit))),
      lim_inf = switch(lien,
                       probit = pnorm(prediction_model$fit - 1.96 * prediction_model$se.fit),
                       logit = plogis(prediction_model$fit - 1.96 * prediction_model$se.fit),
                       cloglog = 1 - exp(-exp(prediction_model$fit - 1.96 * prediction_model$se.fit))),
      lim_sup = switch(lien,
                       probit = pnorm(prediction_model$fit + 1.96 * prediction_model$se.fit),
                       logit = plogis(prediction_model$fit + 1.96 * prediction_model$se.fit),
                       cloglog = 1 - exp(-exp(prediction_model$fit + 1.96 * prediction_model$se.fit)))
    )
  

  # Graphique -------------------------------------------------------------------
  couleur_ogive <- couleur_default
  
  if (!is.null(sexe)) {
    
    couleur_ogive <- switch(
      sexe,
      "F" = group_colors$sexe["F"],
      "M" = group_colors$sexe["M"],
      couleur_default
    )
    
  }
  
  color_by_sex <- "sexe" %in% names(donnees_ogive)
  
  titre_graphique <- "Ogive de reproduction"
  
  if (identical(sexe, "F")) {
    titre_graphique <- "Ogive de reproduction – Femelles"
  }
  
  if (identical(sexe, "M")) {
    titre_graphique <- "Ogive de reproduction – Mâles"
  }
    

  graphique <- ggplot(data = donnees_ogive, aes(x = .data[[variable]], y = .data$maturite)) +
    { if (color_by_sex) geom_line(aes(color = .data$sexe)) else geom_line(color = couleur_ogive) } +
    geom_ribbon(aes(ymin = .data$lim_inf, ymax = .data$lim_sup), alpha = 0.1, fill = couleur_default) +
    { 
      if (color_by_sex) scale_color_manual(
        values = group_colors$sexe,
        labels = group_labels$sexe
      ) else NULL 
    } +

    labs(
      x = ifelse(variable == "ltm",
                 "Longueur maximale (mm)",
                 "Âge"),
      y = "Proportion reproducteurs actifs",
      title = titre_graphique,
      color = "Sexe"
    ) +
    
    theme_aquapop()
  
  if (modele == "TLO") {
    graphique <- graphique +
      annotate("segment", x = point50, xend = point50, y = 0, yend = 0.5, color = couleur_ogive, lty = 2) +
      annotate("segment", x = x_min, xend = point50, y = 0.5, yend = 0.5, color = couleur_ogive, lty = 2)
  }
  
  if (modele %in% c("COM", "ADD", "INT")) {
    if (modele == "COM") {
      vmin_m <- min(donnees_modeles |> filter(.data$sexe == "M") |> pull(.data[[variable]]))
      vmin_f <- min(donnees_modeles |> filter(.data$sexe == "F") |> pull(.data[[variable]]))
    } else {
      form <- stats::as.formula(glue("{variable} ~ sexe"))
      var_minmax <- Summarize(form, data = donnees_modeles)
      vmin_m <- var_minmax |> filter(.data$sexe == "M") |> pull(.data$min)
      vmin_f <- var_minmax |> filter(.data$sexe == "F") |> pull(.data$min)
    }
    
    graphique <- graphique +
      annotate("segment", x = point50$male, xend = point50$male, y = 0, yend = 0.5, color = group_colors$sexe["M"], lty = 2) +
      annotate("segment", x = vmin_m, xend = point50$male, y = 0.5, yend = 0.5, color = group_colors$sexe["M"], lty = 2) +
      annotate("segment", x = point50$fem, xend = point50$fem, y = 0, yend = 0.5, color = group_colors$sexe["F"], lty = 2) +
      annotate("segment", x = vmin_f, xend = point50$fem, y = 0.5, yend = 0.5, color = group_colors$sexe["F"], lty = 2)
    
  }
  
  graphique <- graphique +
    {
      if (color_by_sex) {
        
        geom_point(
          data = donnees_modeles,
          mapping = aes(
            x = .data[[variable]],
            y = as.numeric(.data$maturite) - 1,
            color = .data$sexe),
          alpha = 0.5)
        
      } else {
        
        geom_point(
          data = donnees_modeles,
          mapping = aes(
            x = .data[[variable]],
            y = as.numeric(.data$maturite) - 1),
          color = couleur_ogive,
          alpha = 0.5)
      }
    }
  
  graphique <- graphique +
    labs(
      subtitle = glue(
        "Modèle : {modele} • Lien : {lien}"
      )
    )
  
  return(list(
    success = TRUE,
    message = NULL,
    graphique = graphique,
    donnees_ogive = donnees_ogive
  ))
}
