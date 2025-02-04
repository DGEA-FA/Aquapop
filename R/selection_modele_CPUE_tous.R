selection_modele_CPUE_tous <- function(capture, specimen, espece, station) {
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
  
  # Joindre datacapt avec specimen (filtré pour l'espèce sélectionnée)
  alldata <- specimen %>%
    dplyr::filter(sp == espece) %>%
    right_join(datacapt, by = "no_station")  # Conserver toutes les stations valides
  
  # Vérifier que no_station n'est pas vide (par précaution)
  alldata <- alldata %>% dplyr::filter(no_station != "")
  
  # Remplacer les valeurs NA dans la colonne sexe par "IND"
  alldata$sexe[is.na(alldata$sexe)] <- "IND"
  
    #Tous
    temp <- alldata %>%  dplyr::group_by(no_station) %>%
      summarise(CPUE = length(which(no_specimen != 0)), #Count the number of non-zero elements of each column
                Group = "Tous")
    
    # Poisson -----------------------------------------------------------------
  
    # Ajustement du modèle de Poisson
    model.p <- glm(CPUE ~ 1, family = poisson, data = temp)
    
    # Vérification de l'ajustement avec hnp
    library(hnp)
    set.seed(2023)  # Reproductibilité des simulations
    
    hnp_p <- list()
    for (i in 1:1) {  # Remettre à 100 si nécessaire
      hnp_p[[i]] <- hnp(
        model.p,
        resid.type = "pearson",
        how.many.out = TRUE,
        plot.sim = FALSE
      )
    }
    
    # Calcul de l'ajustement
    summary_hnp_p <- sapply(hnp_p, function(x) x$out / x$total * 100)
    ajustement.p <- mean(summary_hnp_p) %>% as.numeric() %>% round(digits = 2)
    
    # Prédiction et calcul de la CPUE avec IC
    newdata <- data.frame(Moyenne = c("moyenne"))
    predM.p <- predict(model.p, newdata, full = TRUE, se.fit = TRUE, type = "link")
    
    # Calcul uniforme des IC (intervalle de confiance 95%)
    CPUEfinal.p <- exp(predM.p$fit) %>% round(digits = 2)
    linf <- exp(predM.p$fit - 1.96 * predM.p$se.fit) %>% round(digits = 2)
    lsup <- exp(predM.p$fit + 1.96 * predM.p$se.fit) %>% round(digits = 2)
    
    # Déterminer le commentaire selon l'ajustement
    commentaires.p <- ifelse(
      ajustement.p < 10,
      "Le modèle de Poisson s'ajuste bien à vos données.",
      "Le modèle de Poisson ne s'ajuste pas bien à vos données. Vous devriez utiliser un autre modèle."
    )
    
    # Stocker les résultats
    resultCPUE.p <- data.frame(
      "Méthode" = "Poisson",
      Ajustement = ajustement.p,
      CPUE = CPUEfinal.p,
      "IC95" = paste0("(", linf, "-", lsup, ")"),
      Commentaires = commentaires.p,
      modeltemp = "model.p"
    )
    
    # NB1 ---------------------------------------------------------------------
    
    # Ajustement du modèle NB1
    library(glmmTMB)
    model.NB1 <- glmmTMB(CPUE ~ 1, family = nbinom1, data = temp)
    
    # Vérification de l'ajustement avec hnp
    dfun <- function(obj) residuals(obj, type = "pearson")
    sfun <- function(n, obj) simulate(obj)[[1]]
    
    ffun_nb1 <- function(response) {
      fit <- try(glmmTMB(response ~ 1, family = nbinom1, data = temp), silent = TRUE)
      while (class(fit) == "try-error") {
        response2 <- sfun(1, model.NB1)
        fit <- try(glmmTMB(response2 ~ 1, family = nbinom1, data = temp), silent = TRUE)
      }
      return(fit)
    }
    
    set.seed(2023)  # Reproductibilité des simulations
    hnp_NB1 <- list()
    for (i in 1:1) {  # Remettre à 100 si nécessaire
      hnp_NB1[[i]] <- hnp(
        model.NB1,
        newclass = TRUE,
        diagfun = dfun,
        simfun = sfun,
        fitfun = ffun_nb1,
        how.many.out = TRUE,
        plot.sim = FALSE
      )
    }
    
    # Calcul de l'ajustement
    summary_hnp_NB1 <- sapply(hnp_NB1, function(x) x$out / x$total * 100)
    ajustement.NB1 <- mean(summary_hnp_NB1) %>% as.numeric() %>% round(digits = 2)
    
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
      "Le modèle NB1 ne s'ajuste pas bien à vos données. Vous devriez utiliser un autre modèle."
    )
    
    # Stocker les résultats
    resultCPUE.NB1 <- data.frame(
      "Méthode" = "NB1",
      Ajustement = ajustement.NB1,
      CPUE = CPUEfinal.NB1,
      "IC95" = paste0("(", linf, "-", lsup, ")"),
      Commentaires = commentaires.NB1,
      modeltemp = "model.NB1"
    )
    
    # NB2 ---------------------------------------------------------------------
    # Ajustement du modèle NB2
    model.NB2 <- MASS::glm.nb(CPUE ~ 1, data = temp)
    
    # Vérification de l'ajustement avec hnp
    set.seed(2023)  # Reproductibilité des simulations
    hnp_NB2 <- list()
    for (i in 1:1) {  # Remettre à 100 si nécessaire
      hnp_NB2[[i]] <- hnp(
        model.NB2,
        resid.type = "pearson",
        how.many.out = TRUE,
        plot.sim = FALSE
      )
    }
    
    # Calcul de l'ajustement
    summary_hnp_NB2 <- sapply(hnp_NB2, function(x) x$out / x$total * 100)
    ajustement.NB2 <- mean(summary_hnp_NB2) %>% as.numeric() %>% round(digits = 2)
    
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
      "Le modèle NB2 ne s'ajuste pas bien à vos données. Vous devriez utiliser un autre modèle."
    )
    
    # Stocker les résultats
    resultCPUE.NB2 <- data.frame(
      "Méthode" = "NB2",
      Ajustement = ajustement.NB2,
      CPUE = CPUEfinal.NB2,
      "IC95" = paste0("(", linf, "-", lsup, ")"),
      Commentaires = commentaires.NB2,
      modeltemp = "model.NB2"
    )
    
    # CMP ---------------------------------------------------------------------
    
    # Ajustement du modèle CMP (Conway-Maxwell-Poisson)
    library(glmmTMB)
    model.CMP <- glmmTMB(CPUE ~ 1, family = glmmTMB::compois(link = "log"), data = temp)
    
    
    # Vérification de l'ajustement avec hnp
    dfun <- function(obj) {residuals(obj, type = "pearson") }
    sfun <- function(n, obj) { simulate(obj)[[1]] }
    
    ffun_CMP <- function(response) {
      fit <- try(glmmTMB(response ~ 1, family = glmmTMB::compois(link = "log"), data = temp), silent = TRUE)
      while (class(fit) == "try-error") {
        response2 <- sfun(1, model.CMP)
        fit <- try(glmmTMB(response2 ~ 1, family = glmmTMB::compois(link = "log"), data = temp), silent = TRUE)
      }
      return(fit)
    }
    
    set.seed(2023)  # Reproductibilité des simulations
    hnp_CMP <- list()
    for (i in 1:1) {  # Remettre à 100 si nécessaire
      hnp_CMP[[i]] <- hnp(
        model.CMP,
        newclass = TRUE,
        diagfun = dfun,
        simfun = sfun,
        fitfun = ffun_CMP,
        how.many.out = TRUE,
        plot.sim = FALSE
      )
    }
    
    # Calcul de l'ajustement
    summary_hnp_CMP <- sapply(hnp_CMP, function(x) x$out / x$total * 100)
    ajustement.CMP <- mean(summary_hnp_CMP) %>% as.numeric() %>% round(digits = 2)
    
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
      "Le modèle CMP ne s'ajuste pas bien à vos données. Vous devriez utiliser un autre modèle."
    )
    
    # Stocker les résultats
    resultCPUE.CMP <- data.frame(
      "Méthode" = "CMP",
      Ajustement = ajustement.CMP,
      CPUE = CPUEfinal.CMP,
      "IC95" = paste0("(", linf, "-", lsup, ")"),
      Commentaires = commentaires.CMP,
      modeltemp = "model.CMP"
    )
    
    # GP ---------------------------------------------------------------------
    
    # Ajustement du modèle GP (Generalized Poisson)
    library(glmmTMB)
    model.GP <- glmmTMB(CPUE ~ 1, family = glmmTMB::genpois(link = "log"), data = temp)
    
    # Vérification de l'ajustement avec hnp
    dfun <- function(obj) residuals(obj, type = "pearson")
    sfun <- function(n, obj) simulate(obj)[[1]]
    
    ffun_GP <- function(response) {
      fit <- try(glmmTMB(response ~ 1, family = glmmTMB::genpois(link = "log"), data = temp), silent = TRUE)
      while (class(fit) == "try-error") {
        response2 <- sfun(1, model.GP) 
        fit <- try(glmmTMB(response2 ~ 1, family = glmmTMB::genpois(link = "log"), data = temp), silent = TRUE)
      }
      return(fit)
    }
    
    set.seed(2023)  # Reproductibilité des simulations
    hnp_GP <- list()
    for (i in 1:1) {  # Remettre à 100 si nécessaire
      hnp_GP[[i]] <- hnp(
        model.GP,
        newclass = TRUE,
        diagfun = dfun,
        simfun = sfun,
        fitfun = ffun_GP,
        how.many.out = TRUE,
        plot.sim = FALSE
      )
    }
    
    # Calcul de l'ajustement
    summary_hnp_GP <- sapply(hnp_GP, function(x) x$out / x$total * 100)
    ajustement.GP <- mean(summary_hnp_GP) %>% as.numeric() %>% round(digits = 2)
    
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
      "Le modèle GP ne s'ajuste pas bien à vos données. Vous devriez utiliser un autre modèle."
    )
    
    # Stocker les résultats
    resultCPUE.GP <- data.frame(
      "Méthode" = "GP",
      Ajustement = ajustement.GP,
      CPUE = CPUEfinal.GP,
      "IC95" = paste0("(", linf, "-", lsup, ")"),
      Commentaires = commentaires.GP,
      modeltemp = "model.GP"
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
    
    CLEAN <- CLEAN %>%
      mutate(
        # Vérification de la convergence des modèles
        Convergence = sapply(modeltemp, function(m) {
          if (m %in% names(models_list)) {
            mod <- models_list[[m]]
            if ("converged" %in% names(mod)) {
              return(mod$converged)
            } else {
              return(TRUE)  # Si la propriété n'existe pas, on suppose TRUE
            }
          } else {
            return(NA)  # Cas improbable où le modèle est absent
          }
        }),
        
        # Calcul de l'AICc
        AICc = sapply(modeltemp, function(m) {
          if (m %in% names(models_list)) {
            return(AICc(models_list[[m]]))
          } else {
            return(NA)
          }
        }),
        
        # Ajouter un commentaire si le modèle n'a pas convergé
        Commentaires = ifelse(
          Convergence == FALSE, 
          paste0(Commentaires, " ATTENTION : Le modèle n'a pas convergé."),
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
    CLEAN
    

    
    
  }
