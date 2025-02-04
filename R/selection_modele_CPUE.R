selection_modele_CPUE <- function(capture, specimen, espece, station, filtre_specimen = NULL) {
  # Sélectionner les stations valides et au hasard
  stations_valides <- station %>%
    dplyr::filter(st_hasard == "O", st_valide %in% c("O", NA)) %>%
    dplyr::select(no_station)
  
  
  # Assurer que datacapt contient bien toutes les stations valides, y compris celles avec 0 captures
  datacapt <- capture %>%
    dplyr::filter(sp == espece) %>%
    dplyr::select(no_station, nb_capture, nb_pese) %>%
    right_join(stations_valides, by = "no_station") %>%  # Ajouter toutes les stations valides
    mutate(nb_capture = replace_na(nb_capture, 0),  # Remplacer NA par 0 pour les stations vides
           nb_pese = replace_na(nb_pese, 0))
  
  # # Joindre datacapt avec specimen (filtré pour l'espèce sélectionnée)
  # alldata <- specimen %>%
  #   dplyr::filter(sp == espece) %>%
  #   right_join(datacapt, by = "no_station")  # Conserver toutes les stations valides
  
  # Filtrage des spécimens pour l'espèce concernée
  dataspec <- specimen %>% filter(sp == espece) %>% droplevels()
  
  # Appliquer un filtre spécifique sur les spécimens si nécessaire (ex: femelles matures)
  if (!is.null(filtre_specimen)) {
    dataspec <- dataspec %>% filter(!!filtre_specimen)
  }
  
  # Joindre datacapt avec dataspec (filtré pour l'espèce sélectionnée)
  alldata <- dataspec %>%
    right_join(datacapt, by = "no_station")  # Conserver toutes les stations valides
  
  
  
  # Vérifier que no_station n'est pas vide (par précaution)
  alldata <- alldata %>% dplyr::filter(no_station != "")
  
  # Remplacer les valeurs NA dans la colonne sexe par "IND"
  alldata$sexe[is.na(alldata$sexe)] <- "IND"
  
    #Tous
    temp <- alldata %>%  dplyr::group_by(no_station) %>%
      summarise(CPUE = length(which(no_specimen != 0)), #Count the number of non-zero elements of each column
                Group = "Tous")
    
    message("Calcul en cours : Modèle Poisson...")
    
    # Poisson -----------------------------------------------------------------
    model.p <- glm(CPUE ~ 1, family = poisson, data = temp)
    
    message("Test HNP : Modèle Poisson (2 simulations initiales)...")
    set.seed(2023)
    nb_iterations_p <- 2  # Nombre initial de simulations HNP
    
    hnp_p <- replicate(nb_iterations_p, hnp(model.p, resid.type = "pearson", how.many.out = TRUE, plot.sim = FALSE), simplify = FALSE)
    
    # Calculer la proportion de résidus hors enveloppe
    summary_hnp_p <- sapply(hnp_p, function(x) x$out / x$total * 100)
    ajustement.p <- mean(summary_hnp_p) %>% as.numeric() %>% round(digits = 2)
    
    # Si ajustement entre 10% et 15%, on fait 3 simulations supplémentaires
    if (ajustement.p >= 10 & ajustement.p < 15) {
      message("Ajustement entre 10% et 15% : Ajout de 3 simulations HNP supplémentaires...")
      nb_iterations_p <- 5  # On passe à 5 simulations
      hnp_extra <- replicate(3, hnp(model.p, resid.type = "pearson", how.many.out = TRUE, plot.sim = FALSE), simplify = FALSE)
      summary_hnp_extra <- sapply(hnp_extra, function(x) x$out / x$total * 100)
      ajustement.p <- mean(c(summary_hnp_p, summary_hnp_extra)) %>% as.numeric() %>% round(digits = 2)
    }
    
    message("Modèle Poisson terminé.")
    
    # Vérification de la convergence
    converged.p <- ifelse("converged" %in% names(model.p), model.p$converged, TRUE)
    
    # Sélection du modèle selon AICc
    AICc.p <- AICc(model.p)
    
    # Présentation des résultats
    commentaires.p <- ifelse(ajustement.p < 10, 
                             "Le modèle de Poisson s'ajuste bien à vos données.",
                             ifelse(ajustement.p < 15, 
                                    "Le modèle de Poisson a un ajustement marginalement acceptable.",
                                    "Le modèle de Poisson ne s'ajuste pas bien à vos données. Vous devriez utiliser un autre modèle."))
    
    # Calcul des IC
    predM.p <- predict(model.p, newdata = data.frame(Moyenne = c("moyenne")), full = TRUE, se.fit = TRUE, type = "link")
    CPUEfinal.p <- exp(predM.p$fit) %>% round(digits = 2)
    linf <- exp(predM.p$fit - 1.96 * predM.p$se.fit) %>% round(digits = 2)
    lsup <- exp(predM.p$fit + 1.96 * predM.p$se.fit) %>% round(digits = 2)
    
    resultCPUE.p <- data.frame(
      "Méthode" = "Poisson",
      Ajustement = ajustement.p,
      AICc = AICc.p,
      CPUE = CPUEfinal.p,
      "IC95" = paste0("(", linf, "-", lsup, ")"),
      Commentaires = commentaires.p,
      Convergence = converged.p,      
      modeltemp = "model.p",
      Nb_iterations_HNP = nb_iterations_p  # Ajout du nombre d'itérations HNP
    )
    
    
    
    # NB1 ---------------------------------------------------------------------
    
    message("Calcul en cours : Modèle NB1...")
    
    # Ajustement du modèle NB1
    library(glmmTMB)
    model.NB1 <- glmmTMB(CPUE ~ 1, family = nbinom1, data = temp)
    
    # Vérification de l'ajustement avec HNP (2 simulations initiales)
    message("Test HNP : Modèle NB1 (2 simulations initiales)...")
    set.seed(2023)
    nb_iterations_NB1 <- 2  # Nombre initial de simulations HNP
    
    hnp_NB1 <- replicate(nb_iterations_NB1, 
                         hnp(model.NB1, newclass = TRUE, diagfun = residuals, 
                             simfun = function(n, obj) simulate(obj)[[1]], 
                             fitfun = function(response) {
                               fit <- try(glmmTMB(response ~ 1, family = nbinom1, data = temp), silent = TRUE)
                               return(fit)
                             }, 
                             how.many.out = TRUE, plot.sim = FALSE), 
                         simplify = FALSE)
    
    # Calculer la proportion de résidus hors enveloppe
    summary_hnp_NB1 <- sapply(hnp_NB1, function(x) x$out / x$total * 100)
    ajustement.NB1 <- mean(summary_hnp_NB1) %>% as.numeric() %>% round(digits = 2)
    
    # Si ajustement entre 10% et 15%, on fait 3 simulations supplémentaires
    if (ajustement.NB1 >= 10 & ajustement.NB1 < 15) {
      message("Ajustement entre 10% et 15% : Ajout de 3 simulations HNP supplémentaires...")
      nb_iterations_NB1 <- 5  # On passe à 5 simulations
      hnp_extra <- replicate(3, 
                             hnp(model.NB1, newclass = TRUE, diagfun = residuals, 
                                 simfun = function(n, obj) simulate(obj)[[1]], 
                                 fitfun = function(response) {
                                   fit <- try(glmmTMB(response ~ 1, family = nbinom1, data = temp), silent = TRUE)
                                   return(fit)
                                 }, 
                                 how.many.out = TRUE, plot.sim = FALSE), 
                             simplify = FALSE)
      summary_hnp_extra <- sapply(hnp_extra, function(x) x$out / x$total * 100)
      ajustement.NB1 <- mean(c(summary_hnp_NB1, summary_hnp_extra)) %>% as.numeric() %>% round(digits = 2)
    }
    
    message("Modèle NB1 terminé.")
    
    # Vérification de la convergence
    converged.NB1 <- ifelse("converged" %in% names(model.NB1), model.NB1$converged, TRUE)
    
    # Sélection du modèle selon AICc
    AICc.NB1 <- AICc(model.NB1)
    
    # Prédiction et calcul de la CPUE avec IC
    newdata <- data.frame(Moyenne = c("moyenne"))
    predM.NB1 <- predict(model.NB1, newdata, full = TRUE, se.fit = TRUE, type = "link")
    
    # Calcul uniforme des IC (intervalle de confiance 95%)
    CPUEfinal.NB1 <- exp(predM.NB1$fit) %>% round(digits = 2)
    linf <- exp(predM.NB1$fit - 1.96 * predM.NB1$se.fit) %>% round(digits = 2)
    lsup <- exp(predM.NB1$fit + 1.96 * predM.NB1$se.fit) %>% round(digits = 2)
    
    # Déterminer le commentaire selon l'ajustement
    commentaires.NB1 <- ifelse(
      ajustement.NB1 < 10,
      "Le modèle NB1 s'ajuste bien à vos données.",
      ifelse(
        ajustement.NB1 < 15,
        "Le modèle NB1 a un ajustement marginalement acceptable.",
        "Le modèle NB1 ne s'ajuste pas bien à vos données. Vous devriez utiliser un autre modèle."
      )
    )
    
    # Stocker les résultats
    resultCPUE.NB1 <- data.frame(
      "Méthode" = "NB1",
      Ajustement = ajustement.NB1,
      AICc = AICc.NB1,
      CPUE = CPUEfinal.NB1,
      "IC95" = paste0("(", linf, "-", lsup, ")"),
      Commentaires = commentaires.NB1,
      Convergence = converged.NB1,
      modeltemp = "model.NB1",
      Nb_iterations_HNP = nb_iterations_NB1  # Ajout du nombre d'itérations HNP
    )
    
    
    # NB2 ---------------------------------------------------------------------
    message("Calcul en cours : Modèle NB2...")
    
    # Ajustement du modèle NB2
    model.NB2 <- MASS::glm.nb(CPUE ~ 1, data = temp)
    
    # Vérification de l'ajustement avec HNP (2 simulations initiales)
    message("Test HNP : Modèle NB2 (2 simulations initiales)...")
    set.seed(2023)
    nb_iterations_NB2 <- 2  # Nombre initial de simulations HNP
    
    hnp_NB2 <- replicate(nb_iterations_NB2, 
                         hnp(model.NB2, resid.type = "pearson", how.many.out = TRUE, plot.sim = FALSE),
                         simplify = FALSE)
    
    # Calculer la proportion de résidus hors enveloppe
    summary_hnp_NB2 <- sapply(hnp_NB2, function(x) x$out / x$total * 100)
    ajustement.NB2 <- mean(summary_hnp_NB2) %>% as.numeric() %>% round(digits = 2)
    
    # Si ajustement entre 10% et 15%, on fait 3 simulations supplémentaires
    if (ajustement.NB2 >= 10 & ajustement.NB2 < 15) {
      message("Ajustement entre 10% et 15% : Ajout de 3 simulations HNP supplémentaires...")
      nb_iterations_NB2 <- 5  # On passe à 5 simulations
      hnp_extra <- replicate(3, 
                             hnp(model.NB2, resid.type = "pearson", how.many.out = TRUE, plot.sim = FALSE),
                             simplify = FALSE)
      summary_hnp_extra <- sapply(hnp_extra, function(x) x$out / x$total * 100)
      ajustement.NB2 <- mean(c(summary_hnp_NB2, summary_hnp_extra)) %>% as.numeric() %>% round(digits = 2)
    }
    
    message("Modèle NB2 terminé.")
    
    # Vérification de la convergence
    converged.NB2 <- ifelse("converged" %in% names(model.NB2), model.NB2$converged, TRUE)
    
    # Sélection du modèle selon AICc
    AICc.NB2 <- AICc(model.NB2)
    
    # Prédiction et calcul de la CPUE avec IC
    newdata <- data.frame(Moyenne = c("moyenne"))
    predM.NB2 <- predict(model.NB2, newdata, full = TRUE, se.fit = TRUE, type = "link")
    
    # Calcul uniforme des IC (intervalle de confiance 95%)
    CPUEfinal.NB2 <- exp(predM.NB2$fit) %>% round(digits = 2)
    linf <- exp(predM.NB2$fit - 1.96 * predM.NB2$se.fit) %>% round(digits = 2)
    lsup <- exp(predM.NB2$fit + 1.96 * predM.NB2$se.fit) %>% round(digits = 2)
    
    # Déterminer le commentaire selon l'ajustement
    commentaires.NB2 <- ifelse(
      ajustement.NB2 < 10,
      "Le modèle NB2 s'ajuste bien à vos données.",
      ifelse(
        ajustement.NB2 < 15,
        "Le modèle NB2 a un ajustement marginalement acceptable.",
        "Le modèle NB2 ne s'ajuste pas bien à vos données. Vous devriez utiliser un autre modèle."
      )
    )
    
    # Stocker les résultats
    resultCPUE.NB2 <- data.frame(
      "Méthode" = "NB2",
      Ajustement = ajustement.NB2,
      AICc = AICc.NB2,
      CPUE = CPUEfinal.NB2,
      "IC95" = paste0("(", linf, "-", lsup, ")"),
      Commentaires = commentaires.NB2,
      Convergence = converged.NB2,
      modeltemp = "model.NB2",
      Nb_iterations_HNP = nb_iterations_NB2  # Ajout du nombre d'itérations HNP
    )
    
    
    # CMP ---------------------------------------------------------------------
    
    message("Calcul en cours : Modèle CMP...")
    
    # Ajustement du modèle CMP (Conway-Maxwell-Poisson)
    model.CMP <- glmmTMB(CPUE ~ 1, family = glmmTMB::compois(link = "log"), data = temp)
    
    # Vérification de l'ajustement avec HNP (2 simulations initiales)
    message("Test HNP : Modèle CMP (2 simulations initiales)...")
    set.seed(2023)
    nb_iterations_CMP <- 2  # Nombre initial de simulations HNP
    
    hnp_CMP <- replicate(nb_iterations_CMP, 
                         hnp(model.CMP, newclass = TRUE, diagfun = function(obj) residuals(obj, type = "pearson"),
                             simfun = function(n, obj) simulate(obj)[[1]], fitfun = function(response) {
                               fit <- try(glmmTMB(response ~ 1, family = glmmTMB::compois(link = "log"), data = temp), silent = TRUE)
                               while (class(fit) == "try-error") {
                                 response2 <- simulate(model.CMP)[[1]]
                                 fit <- try(glmmTMB(response2 ~ 1, family = glmmTMB::compois(link = "log"), data = temp), silent = TRUE)
                               }
                               return(fit)
                             }, how.many.out = TRUE, plot.sim = FALSE),
                         simplify = FALSE)
    
    # Calculer la proportion de résidus hors enveloppe
    summary_hnp_CMP <- sapply(hnp_CMP, function(x) x$out / x$total * 100)
    ajustement.CMP <- mean(summary_hnp_CMP) %>% as.numeric() %>% round(digits = 2)
    
    # Si ajustement entre 10% et 15%, on fait 3 simulations supplémentaires
    if (ajustement.CMP >= 10 & ajustement.CMP < 15) {
      message("Ajustement entre 10% et 15% : Ajout de 3 simulations HNP supplémentaires...")
      nb_iterations_CMP <- 5  # On passe à 5 simulations
      hnp_extra <- replicate(3, 
                             hnp(model.CMP, newclass = TRUE, diagfun = function(obj) residuals(obj, type = "pearson"),
                                 simfun = function(n, obj) simulate(obj)[[1]], fitfun = function(response) {
                                   fit <- try(glmmTMB(response ~ 1, family = glmmTMB::compois(link = "log"), data = temp), silent = TRUE)
                                   while (class(fit) == "try-error") {
                                     response2 <- simulate(model.CMP)[[1]]
                                     fit <- try(glmmTMB(response2 ~ 1, family = glmmTMB::compois(link = "log"), data = temp), silent = TRUE)
                                   }
                                   return(fit)
                                 }, how.many.out = TRUE, plot.sim = FALSE),
                             simplify = FALSE)
      summary_hnp_extra <- sapply(hnp_extra, function(x) x$out / x$total * 100)
      ajustement.CMP <- mean(c(summary_hnp_CMP, summary_hnp_extra)) %>% as.numeric() %>% round(digits = 2)
    }
    
    message("Modèle CMP terminé.")
    
    # Vérification de la convergence
    converged.CMP <- ifelse("converged" %in% names(model.CMP), model.CMP$converged, TRUE)
    
    # Sélection du modèle selon AICc
    AICc.CMP <- AICc(model.CMP)
    
    # Prédiction et calcul de la CPUE avec IC
    newdata <- data.frame(Moyenne = c("moyenne"))
    predM.CMP <- predict(model.CMP, newdata, full = TRUE, se.fit = TRUE, type = "link")
    
    # Calcul uniforme des IC (intervalle de confiance 95%)
    CPUEfinal.CMP <- exp(predM.CMP$fit) %>% round(digits = 2)
    linf <- exp(predM.CMP$fit - 1.96 * predM.CMP$se.fit) %>% round(digits = 2)
    lsup <- exp(predM.CMP$fit + 1.96 * predM.CMP$se.fit) %>% round(digits = 2)
    
    # Déterminer le commentaire selon l'ajustement
    commentaires.CMP <- ifelse(
      ajustement.CMP < 10,
      "Le modèle CMP s'ajuste bien à vos données.",
      ifelse(
        ajustement.CMP < 15,
        "Le modèle CMP a un ajustement marginalement acceptable.",
        "Le modèle CMP ne s'ajuste pas bien à vos données. Vous devriez utiliser un autre modèle."
      )
    )
    
    # Stocker les résultats
    resultCPUE.CMP <- data.frame(
      "Méthode" = "CMP",
      Ajustement = ajustement.CMP,
      AICc = AICc.CMP,
      CPUE = CPUEfinal.CMP,
      "IC95" = paste0("(", linf, "-", lsup, ")"),
      Commentaires = commentaires.CMP,
      Convergence = converged.CMP,
      modeltemp = "model.CMP",
      Nb_iterations_HNP = nb_iterations_CMP  # Ajout du nombre d'itérations HNP
    )
    
    
    # GP ---------------------------------------------------------------------
    
    message("Calcul en cours : Modèle GP...")
    
    # Ajustement du modèle GP (Generalized Poisson)
    model.GP <- glmmTMB(CPUE ~ 1, family = glmmTMB::genpois(link = "log"), data = temp)
    
    # Vérification de l'ajustement avec HNP (2 simulations initiales)
    message("Test HNP : Modèle GP (2 simulations initiales)...")
    set.seed(2023)
    nb_iterations_GP <- 2  # Nombre initial de simulations HNP
    
    hnp_GP <- replicate(nb_iterations_GP, 
                        hnp(model.GP, newclass = TRUE, diagfun = function(obj) residuals(obj, type = "pearson"),
                            simfun = function(n, obj) simulate(obj)[[1]], fitfun = function(response) {
                              fit <- try(glmmTMB(response ~ 1, family = glmmTMB::genpois(link = "log"), data = temp), silent = TRUE)
                              while (class(fit) == "try-error") {
                                response2 <- simulate(model.GP)[[1]]
                                fit <- try(glmmTMB(response2 ~ 1, family = glmmTMB::genpois(link = "log"), data = temp), silent = TRUE)
                              }
                              return(fit)
                            }, how.many.out = TRUE, plot.sim = FALSE),
                        simplify = FALSE)
    
    # Calculer la proportion de résidus hors enveloppe
    summary_hnp_GP <- sapply(hnp_GP, function(x) x$out / x$total * 100)
    ajustement.GP <- mean(summary_hnp_GP) %>% as.numeric() %>% round(digits = 2)
    
    # Si ajustement entre 10% et 15%, on fait 3 simulations supplémentaires
    if (ajustement.GP >= 10 & ajustement.GP < 15) {
      message("Ajustement entre 10% et 15% : Ajout de 3 simulations HNP supplémentaires...")
      nb_iterations_GP <- 5  # On passe à 5 simulations
      hnp_extra <- replicate(3, 
                             hnp(model.GP, newclass = TRUE, diagfun = function(obj) residuals(obj, type = "pearson"),
                                 simfun = function(n, obj) simulate(obj)[[1]], fitfun = function(response) {
                                   fit <- try(glmmTMB(response ~ 1, family = glmmTMB::genpois(link = "log"), data = temp), silent = TRUE)
                                   while (class(fit) == "try-error") {
                                     response2 <- simulate(model.GP)[[1]]
                                     fit <- try(glmmTMB(response2 ~ 1, family = glmmTMB::genpois(link = "log"), data = temp), silent = TRUE)
                                   }
                                   return(fit)
                                 }, how.many.out = TRUE, plot.sim = FALSE),
                             simplify = FALSE)
      summary_hnp_extra <- sapply(hnp_extra, function(x) x$out / x$total * 100)
      ajustement.GP <- mean(c(summary_hnp_GP, summary_hnp_extra)) %>% as.numeric() %>% round(digits = 2)
    }
    
    message("Modèle GP terminé.")
    
    # Vérification de la convergence
    converged.GP <- ifelse("converged" %in% names(model.GP), model.GP$converged, TRUE)
    
    # Sélection du modèle selon AICc
    AICc.GP <- AICc(model.GP)
    
    # Prédiction et calcul de la CPUE avec IC
    newdata <- data.frame(Moyenne = c("moyenne"))
    predM.GP <- predict(model.GP, newdata, full = TRUE, se.fit = TRUE, type = "link")
    
    # Calcul uniforme des IC (intervalle de confiance 95%)
    CPUEfinal.GP <- exp(predM.GP$fit) %>% round(digits = 2)
    linf <- exp(predM.GP$fit - 1.96 * predM.GP$se.fit) %>% round(digits = 2)
    lsup <- exp(predM.GP$fit + 1.96 * predM.GP$se.fit) %>% round(digits = 2)
    
    # Déterminer le commentaire selon l'ajustement
    commentaires.GP <- ifelse(
      ajustement.GP < 10,
      "Le modèle GP s'ajuste bien à vos données.",
      ifelse(
        ajustement.GP < 15,
        "Le modèle GP a un ajustement marginalement acceptable.",
        "Le modèle GP ne s'ajuste pas bien à vos données. Vous devriez utiliser un autre modèle."
      )
    )
    
    # Stocker les résultats
    resultCPUE.GP <- data.frame(
      "Méthode" = "GP",
      Ajustement = ajustement.GP,
      AICc = AICc.GP,
      CPUE = CPUEfinal.GP,
      "IC95" = paste0("(", linf, "-", lsup, ")"),
      Commentaires = commentaires.GP,
      Convergence = converged.GP,
      modeltemp = "model.GP",
      Nb_iterations_HNP = nb_iterations_GP  # Ajout du nombre d'itérations HNP
    )
    
    
# Mise en page des resultats ----------------------------------------------
    
    # Construire le tableau CLEAN avec tous les modèles
    CLEAN <- rbind(
      resultCPUE.p,
      resultCPUE.NB2,
      resultCPUE.NB1,
      resultCPUE.CMP,
      resultCPUE.GP
    )
    
    # Vérification de la convergence des modèles et calcul de l'AICc
    models_list <- list(
      "model.p" = model.p,
      "model.NB2" = model.NB2,
      "model.NB1" = model.NB1,
      "model.CMP" = model.CMP,
      "model.GP" = model.GP
    )
    
    # Mise à jour de CLEAN avec convergence et AICc
    CLEAN <- CLEAN %>%
      mutate(
        # Vérification de la convergence des modèles
        Convergence = sapply(modeltemp, function(m) {
          if (m %in% names(models_list)) {
            mod <- models_list[[m]]
            
            # Pour glmmTMB : la convergence est stockée dans $fit$convergence
            if (inherits(mod, "glmmTMB")) {
              return(mod$fit$convergence == 0)  # 0 signifie que le modèle a convergé
            }
            
            # Pour glm et glm.nb (MASS), pas d'attribut explicite, on suppose TRUE
            if (inherits(mod, "glm") || inherits(mod, "negbin")) {
              return(TRUE)  
            }
            
            # Cas général : si la propriété 'converged' existe
            if ("converged" %in% names(mod)) {
              return(mod$converged)
            }
            
            return(NA)  # Si inconnu, mieux vaut retourner NA
          } else {
            return(NA)  # Cas improbable où le modèle est absent
          }
        }),
        
        # Calcul de l'AICc
        AICc = sapply(modeltemp, function(m) {
          if (m %in% names(models_list)) {
            return(AICc(models_list[[m]]))
          } else {
            return(NA_real_)
          }
        }),
        
        # Ajouter un avertissement si le modèle n'a pas convergé
        Commentaires = ifelse(
          Convergence == FALSE, 
          paste0(Commentaires, " ⚠ ATTENTION : Le modèle n'a pas convergé."),
          Commentaires
        )
      )
    
    # Sélection des modèles bien ajustés pour le calcul de ΔAICc
    CLEAN_bien_ajuste <- CLEAN %>% filter(Ajustement < 10)
    
    if (nrow(CLEAN_bien_ajuste) > 0) {
      min_AICc <- min(CLEAN_bien_ajuste$AICc, na.rm = TRUE)
      CLEAN_bien_ajuste <- CLEAN_bien_ajuste %>%
        mutate(Delta_AICc = AICc - min_AICc) %>%
        arrange(AICc)
    } else {
      # Si aucun modèle n'est bien ajusté, on prend tous les modèles
      min_AICc <- min(CLEAN$AICc, na.rm = TRUE)
      CLEAN <- CLEAN %>%
        mutate(Delta_AICc = AICc - min_AICc) %>%
        arrange(AICc)
    }
    
    # Réintégration des modèles mal ajustés avec priorité aux modèles bien ajustés
    CLEAN <- CLEAN %>%
      left_join(select(CLEAN_bien_ajuste, Méthode, Delta_AICc), by = "Méthode") %>%
      mutate(Delta_AICc = replace_na(Delta_AICc, NA)) %>%
      arrange(Ajustement >= 10, AICc)  # Priorité aux modèles bien ajustés, puis tri par AICc
    
    # Sélection du meilleur modèle parmi ceux bien ajustés
    if (nrow(CLEAN_bien_ajuste) > 0) {
      best_model <- CLEAN_bien_ajuste %>% filter(Delta_AICc == 0)
      best_model_name <- best_model$Méthode
      CLEAN <- CLEAN %>%
        mutate(Commentaires = ifelse(
          Méthode == best_model_name,
          paste0(Commentaires, "Ce modèle est recommandé car son AICc est le plus faible."),
          Commentaires
        ))
    } else {
      # Si aucun modèle ne s'ajuste bien, prendre le meilleur modèle global
      best_model <- CLEAN %>% filter(Delta_AICc == 0)
      best_model_name <- best_model$Méthode
      CLEAN <- CLEAN %>%
        mutate(Commentaires = ifelse(
          Méthode == best_model_name,
          paste0(Commentaires, "Il s’agit toutefois du meilleur modèle parmi les options disponibles."),
          Commentaires
        ))
    }
    
    # Sélection finale des colonnes pour affichage
    CLEAN <- CLEAN %>%
      select("Méthode", "Ajustement", "AICc", "Delta_AICc", "CPUE", "IC95", "Commentaires", "Convergence")
    
    # Retourner le tableau final
    CLEAN <- CLEAN %>% rename('IC 95%' = IC95,
                    "Ajustement (résultat du test HNP)" = Ajustement) %>% as.data.frame()
    CLEAN
  }
