courbe_croissance_plot <- function(dfspecimen, sp_pen, tablemodele, modele) {
  # Filtrer les données pour l'espèce spécifiée
  init <- dfspecimen %>% filter(sp == sp_pen)
  
  # Supprimer les enregistrements avec des valeurs manquantes pour ltm et age
  init <- init %>% filter(!is.na(ltm) & !is.na(age))
  
  # Sélectionner uniquement les colonnes nécessaires
  init <- init %>% select(ltm, age, no_specimen)
  
  # Renommer les rangées séquentiellement de 1 à n
  rownames(init) <- seq(nrow(init))
  
  # Sélectionner le modèle spécifique
  model <- tablemodele %>% filter(methode == modele)

  # Calcul des âges minimum et maximum pour le graphique
  dfbase <- FSA::Summarize(ltm ~ age, data = init)
  agemin <- min(dfbase$age)
  agemax <- max(dfbase$age)
  ageGRAPH <- c(0, (ceiling(agemax / 5) * 5) + 1)
  ageGRAPHbreak <- c(ageGRAPH[1]:ageGRAPH[2])
  
  # Initialisation des paramètres du modèle
  sv0 <- list(
    Linf = model$l_inf,
    K = model$k,
    t0 = model$t0
  )
  
  # Définir les fonctions pour chaque modèle
  vBert <- function(age, Linf, K, t0) Linf * (1 - exp(-K * (age - t0)))
  Gompt <- function(age, Linf, K, t0) Linf * exp(-exp(-K * (age - t0)))
  Logis <- function(age, Linf, K, t0) Linf / (1 + exp(-K * (age - t0)))
  
  # Choix du modèle à ajuster
  fit0 <- switch(modele,
                 "Von Bertalanffy" = nls(ltm ~ vBert(age, Linf, K, t0), data = init, start = sv0),
                 "Gompertz" = nls(ltm ~ Gompt(age, Linf, K, t0), data = init, start = sv0),
                 "Logistique" = nls(ltm ~ Logis(age, Linf, K, t0), data = init, start = sv0))
  
  # Génération des prédictions avec intervalles de confiance
  preds <- data.frame(age = ageGRAPHbreak,
                      investr::predFit(fit0, data.frame(age = ageGRAPHbreak),
                                       interval = "confidence"))
  
 
  # Création du graphique avec ggplot2
  ggplot() +
    geom_ribbon(data = preds, aes(x = age, ymin = lwr, ymax = upr), fill = "gray80") +
    geom_point(data = init, aes(y = ltm, x = age), size = 2, alpha = 0.1) +
    geom_line(data = preds, aes(x = age, y = fit), linewidth = 1, linetype = "dashed") +
    geom_line(data = filter(preds, age >= agemin, age <= agemax), aes(x = age, y = fit), linewidth = 1) +
    scale_y_continuous(name = "Longueur totale maximale (mm)", expand = c(0, 0)) +
    scale_x_continuous(name = "Âge (année)", breaks = ageGRAPHbreak, limits = ageGRAPH, expand = c(0, 0)) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          plot.caption = element_text(size = 10)) +
    annotate("segment", x = -Inf, xend = Inf, y = sv0$Linf, yend = sv0$Linf, linewidth = 0.5, color = "red", linetype = 2) +
    labs(caption = paste0("Modèle : ", modele, "\n", 
                          "Equation : L(âge) = ", round(sv0$Linf, 2), 
                          " / (1 + exp(-", round(sv0$K, 3), 
                          " * (âge - ", round(sv0$t0, 3), ")))"))  
}
