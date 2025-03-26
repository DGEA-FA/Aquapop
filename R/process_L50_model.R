#' Analyse des modèles L50 pour la maturité sexuelle avec visualisation
#'
#' @description Cette fonction extrait et analyse un modèle L50 spécifique en fonction du sexe,
#' en calculant l'intervalle de confiance et en générant un graphique de l'ogive de maturité.
#'
#' @param modele_id Une chaîne de caractères correspondant à l'identifiant du modèle (ex: "M_logit", "F_probit").
#' @param data Un dataframe contenant les colonnes `ltm` (longueur), `sexe`, et `maturite`.
#' @param liste_modeles Une liste contenant les modèles ajustés pour chaque sexe et type de régression.
#'
#' @return Une liste contenant :
#' \itemize{
#'   \item `L50` : La longueur à 50% de maturité.
#'   \item `LI` : Limite inférieure de l'intervalle de confiance.
#'   \item `LS` : Limite supérieure de l'intervalle de confiance.
#'   \item `DATAogive` : Un dataframe contenant les probabilités de maturité avec intervalle de confiance.
#'   \item `minitable` : Un dataframe résumé des résultats principaux (L50, intervalle, coefficients).
#'   \item `plot` : Un ggplot représentant l'ogive de maturité avec IC et points d'observation.
#' }
#' @export
process_L50_model <- function(modele_id, 
                              data, 
                              liste_modeles) {
  
  library(dplyr)
  library(ggplot2)
  library(glue)
  
  # Vérification des paramètres
  if (missing(data) || is.null(data)) stop("Veuillez fournir un dataframe 'data'.")
  if (missing(liste_modeles) || is.null(liste_modeles)) stop("Veuillez fournir une liste de modèles 'liste_modeles'.")
  if (!(modele_id %in% names(liste_modeles))) stop(glue::glue("Le modèle '{modele_id}' n'existe pas dans liste_modeles."))
  
  # Identifier le sexe à partir du nom du modèle
  sexe_modele <- ifelse(grepl("^M_", modele_id), "M", "F")
  
  # Récupération du modèle
  modele_objet <- liste_modeles[[modele_id]]
  
  # Calcul des intervalles de confiance pour L50
  l50_ic <- confint_L(modele_objet, method = "montecarlo", interval_type = "bca", nboot = 100000)
  
  L50 <- round(l50_ic[2], digits = 0)
  LI <- round(l50_ic[1], digits = 0)
  LS <- round(l50_ic[3], digits = 0)
  
  # Déterminer les valeurs min/max de `ltm` pour le sexe correspondant
  ltm_range <- data %>%
    filter(sexe == sexe_modele) %>%
    summarise(ltm_min = min(ltm, na.rm = TRUE), ltm_max = max(ltm, na.rm = TRUE))
  
  if (nrow(ltm_range) == 0) {
    stop(glue::glue("Aucune donnée pour le sexe '{sexe_modele}' dans 'data'."))
  }
  
  ltm_min <- ltm_range$ltm_min
  ltm_max <- ltm_range$ltm_max
  
  # Création du dataframe de prédiction
  newDF <- data.frame(sexe = sexe_modele, ltm = seq(from = ltm_min, to = ltm_max, by = 1))
  newDFpred <- predict(modele_objet, newDF, full = TRUE, type = "link", se.fit = TRUE)
  
  mat <- plogis(newDFpred$fit)
  LI_pred <- plogis(newDFpred$fit - (1.96 * newDFpred$se.fit))
  LS_pred <- plogis(newDFpred$fit + (1.96 * newDFpred$se.fit))
  
  DATAogive <- cbind(newDF, maturite = mat, LI = LI_pred, LS = LS_pred)
  
  # Extraction des coefficients du modèle
  b0 <- round(coef(modele_objet)[["(Intercept)"]], digits = 3)
  b1 <- round(coef(modele_objet)[["ltm"]], digits = 3)
  
  # Création d'une table récapitulative
  minitable <- data.frame(
    Sexe = ifelse(sexe_modele == "M", "Mâle", "Femelle"),
    L50 = L50,
    Intervalle = glue("[{LI}-{LS}]"),
    b0 = b0,
    b1 = b1
  )
  
  # === GÉNÉRATION DU GRAPHIQUE === #
  
  plot <- ggplot(data = DATAogive, aes(x = ltm, y = maturite)) +
    geom_line(color = ifelse(sexe_modele == "M", "#bdbdbd", "#636363")) +
    geom_ribbon(aes(ymin = LI, ymax = LS), alpha = 0.1, fill = ifelse(sexe_modele == "M", "#bdbdbd", "#636363")) +
    annotate("segment", x = L50, xend = L50, y = 0, yend = 0.5, color = "black", lty = 2) +
    annotate("segment", x = ltm_min, xend = L50, y = 0.5, yend = 0.5, color = "black", lty = 2) +
    geom_point(data = data %>% filter(sexe == sexe_modele),
               mapping = aes(x = ltm, y = as.numeric(maturite)-1, color = sexe), alpha = 0.5) +
    scale_color_manual(values = c("M" = "blue", "F" = "#636363")) +
    theme_classic() +
    labs(x = "Longueur totale maximale (mm)", y = "Proportion reproducteur actif", 
         title = glue("Ogive de maturité - {ifelse(sexe_modele == 'M', 'Mâle', 'Femelle')}"),
         color = "Sexe") +
    theme(panel.background = element_rect(fill = "white", colour = "black"),
          legend.position = "none")
  plot
  
  return(list(
    L50 = L50,
    LI = LI,
    LS = LS,
    DATAogive = DATAogive,
    minitable = minitable,
    plot = plot  # Ajout du ggplot dans la sortie
    
  ))
}
