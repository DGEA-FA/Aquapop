selection_modele_CPUE_Fmature <-
  function(capture, specimen, espece, station) {
    datacapt <-
      capture %>%  dplyr::filter(sp == espece)  %>% droplevels()
    datacapt <-
      datacapt %>% dplyr::select("no_station", "nb_capture",  "nb_pese")
    dataspec <- specimen %>%
      dplyr::filter(sp == espece) %>%
      filter(maturite == "O" & sexe %in% c("F"))  %>% droplevels()
    alldata <-
      merge(dataspec, datacapt, all.x = TRUE) #ch ligne 1 specimen ou on a ajoute n total de capture et de poissons peses par station.On part de ca pour tous les autres filtres
    
    #alldata <- alldata %>% dplyr::filter(TYPE_MAILL %in% c("G",NA))  %>% droplevels() #on veut juste les filets expérimentaux
    #UPDATE: LES TECHS SPECIFIENT QUEL COTE DU FILET EXP EST SUR LE BORD DE LA RIVE. DONC NON, PAS DE FILTRE POUR LE TYPE DENGIN
    
    
    alldata <-
      alldata %>% dplyr::filter(st_hasard == "O") #slmt les stations hasard  tirage aleatoire
    alldata <-
      alldata %>% dplyr::filter(st_valide %in% c("O", NA)) #slmt les stations valides. Filet entortillés par ex.
    #Pourrait etre valide mais hasard non, et tout les combinaisons donc imp les 2 oui.
    #Parfois NA partout donc on selectionne oui, sauf si explicitement N
    
    
    alldata <-
      alldata %>% dplyr::filter(no_station != "") #retirer station NA où oubli d'écrire la station
    
    #ICI CONSIDERER LES STATIONS VIDES
    nstation <- length(unique(station$no_station)) %>% as.numeric()
    listallstation <- unique(station$no_station)
    dfvide <- data.frame(no_station = listallstation)
    
    alldata <-
      merge(dfvide, alldata, by = "no_station",  all.x = TRUE) # on ajoute ligne pour stations vides
    
    #Tous
    temp <- alldata %>%  dplyr::group_by(no_station) %>%
      summarise(CPUE = length(which(no_specimen != 0)), #Count the number of non-zero elements of each column
                Group = "Tous")
    # writexl::write_xlsx(temp,file.path("C:", "Users", "carol", "OneDrive", "EmploiMFFP", "Hiver2022-2023", "Dynapop_dossierCommun", "ExemplepourJM.xlsx"  ))
    
    abondancefix <- sum(temp$CPUE) %>% as.numeric()
    
    # poisson -----------------------------------------------------------------
    
    
    ## Poisson (p), on teste un premier modele pour CPUElac
    
    model.p <-
      glm(CPUE ~ 1, family = poisson, data = temp)#modele glm initial pour le calcul
    
    library(hnp)
    
    set.seed(2023) # fonction set.seed(2023) sert à reproduire toujours la même valeur car hnp fonctionne
    #à partir de simulations. Utiliser 10 ou 100 simulations pour le même modèle va produire une estimation semblable à
    #la première, mais différente aussi.
    
    hnp_p <- list()
    
    for (i in 1:1) {
      #devrait etre 100...
      
      hnp_p[[i]] <-
        hnp(
          model.p,
          resid.type = "pearson",
          how.many.out = TRUE,
          plot.sim = FALSE
        )
      
    }
    
    summary_hnp_p <- sapply(hnp_p, function(x)
      x$out / x$total * 100)
    
    ajustement.p <-
      mean(summary_hnp_p) %>% as.numeric() %>% round(digits = 2)
    
    #presentation des resultats
    newdata <- data.frame(Moyenne = c("moyenne"))
    method.p <- "Poisson"
    predM.p <-
      predict(model.p,
              newdata,
              full = TRUE,
              se.fit = TRUE,
              type = "link")
    CPUEfinal.p <-
      exp(predM.p$fit)  %>% round(digits = 2) #JM dit de pas faire round a lunite (digits=0) pour la moyenne, mais plutot 2
    CPUEfinal.p
    confint.p <-
      confint(model.p) #ON DEVRAIT PRENDRE CA COMME lwr and upr limites ! Ca donne aussi un IC95%
    commentaires.p <- NA
    if (ajustement.p < 10) {
      commentaires.p <-
        "Le modèle de Poisson s'ajuste bien à vos données."
    }
    if (ajustement.p > 10) {
      commentaires.p <-
        "Le modèle de Poisson ne s'ajuste pas bien à vos données. Vous devriez utiliser un autre modèle."
    }
    # linf <- (CPUEfinal.p + confint.p[1]) %>% round(digits = 2)
    # lsup <- (CPUEfinal.p - confint.p[2]) %>% round(digits = 2)
    #
    linf <-
      exp(predM.p$fit - (1.96 * predM.p$se.fit)) %>%   round(digits = 2) #JM dit de pas faire round a lunite (digits=0) pour la moyenne, mais plutot 2
    lsup <-
      exp(predM.p$fit + (1.96 * predM.p$se.fit)) %>%   round(digits = 2) #JM dit de pas faire round a lunite (digits=0) pour la moyenne, mais plutot 2
    
    resultCPUE.p <- data.frame(
      "Méthode" = method.p,
      Ajustement = ajustement.p,
      CPUE = CPUEfinal.p,
      "IC95" = paste0("(", linf, "-", lsup, ")"),
      Commentaires = commentaires.p,
      modeltemp = "model.p"
    )
    
    
    # NB1 ---------------------------------------------------------------------
    
    library(glmmTMB)
    model.NB1 <-
      glmmTMB(CPUE ~ 1, family = nbinom1, data = temp)#modele glm initial pour le calcul
    
    
    dfun <- function(obj) {
      residuals(obj, type = "pearson")
    }
    sfun <- function(n, obj) {
      simulate(obj)[[1]]
    }
    
    ffun_nb1 <- function(response) {
      fit <- try(glmmTMB(response ~ 1, family = nbinom1, data = temp),
                 silent = TRUE)
      while (class(fit) == "try-error") {
        response2 <- sfun(1, m.df_EXT.NB1)
        fit <-
          try(glmmTMB(response2 ~ 1, family = nbinom1, data = temp),
              silent = TRUE)
      }
      return(fit)
    }
    
    
    set.seed(2023) # fonction set.seed(2023) sert à reproduire toujours la même valeur car hnp fonctionne
    #à partir de simulations. Utiliser 10 ou 100 simulations pour le même modèle va produire une estimation semblable à
    #la première, mais différente aussi.
    
    hnp_NB1 <- list()
    
    for (i in 1:1) {
      #remettre a 100
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
    
    summary_hnp_NB1 <- sapply(hnp_NB1, function(x)
      x$out / x$total * 100)
    
    ajustement.NB1 <-
      mean(summary_hnp_NB1) %>% as.numeric() %>% round(digits = 2)
    
    #presentation des resultats
    newdata <- data.frame(Moyenne = c("moyenne"))
    method.NB1 <- "NB1"
    predM.NB1 <-
      predict(
        model.NB1,
        newdata,
        full = TRUE,
        se.fit = TRUE,
        type = "link"
      )
    CPUEfinal.NB1 <-
      exp(predM.NB1$fit)  %>% round(digits = 2) #JM dit de pas faire round a lunite (digits=0) pour la moyenne, mais plutot 2
    CPUEfinal.NB1
    confint.NB1 <-
      confint(model.NB1) #ON DEVRAIT PRENDRE CA COMME lwr and upr limites ! Ca donne aussi un IC95%
    commentaires.NB1 <- NA
    if (ajustement.NB1 < 10) {
      commentaires.NB1 <- "Le modèle NB1 s'ajuste bien à vos données."
    }
    if (ajustement.NB1 > 10) {
      commentaires.NB1 <-
        "Le modèle NB1 ne s'ajuste pas bien à vos données. Vous devriez utiliser un autre modèle."
    }
    # linf <- (CPUEfinal.NB1 + confint.NB1[1]) %>% round(digits = 2)
    # lsup <- (CPUEfinal.NB1 - confint.NB1[2]) %>% round(digits = 2)
    
    linf <-
      exp(predM.NB1$fit - (1.96 * predM.NB1$se.fit)) %>%   round(digits = 2) #JM dit de pas faire round a lunite (digits=0) pour la moyenne, mais plutot 2
    lsup <-
      exp(predM.NB1$fit + (1.96 * predM.NB1$se.fit)) %>%   round(digits = 2) #JM dit de pas faire round a lunite (digits=0) pour la moyenne, mais plutot 2
    
    
    resultCPUE.NB1 <- data.frame(
      "Méthode" = method.NB1,
      Ajustement = ajustement.NB1,
      CPUE = CPUEfinal.NB1,
      "IC95" = paste0("(", linf, "-", lsup, ")"),
      Commentaires = commentaires.NB1,
      modeltemp = "model.NB1"
    )
    

    # NB2 ---------------------------------------------------------------------
    
    
    # NB2 (NB2), on teste un 2e modele
    model.NB2 <-  MASS::glm.nb(CPUE ~ 1, data = temp)
    set.seed(2023)
    
    hnp_NB2 <- list()
    
    for (i in 1:1) {
      #devrait etre 100...
      
      hnp_NB2[[i]] <-
        hnp(
          model.NB2,
          resid.type = "pearson",
          how.many.out = TRUE,
          plot.sim = FALSE
        )
      
    }
    
    summary_hnp_NB2 <- sapply(hnp_NB2, function(x)
      x$out / x$total * 100)
    ajustement.NB2 <-
      mean(summary_hnp_NB2) %>% as.numeric()  %>% round(digits = 2)
    commentaires.NB2 <- NA
    
    if (ajustement.NB2 < 10) {
      commentaires.NB2 <- "Le modèle de NB2 s'ajuste bien à vos données."
    }
    if (ajustement.NB2 > 10) {
      commentaires.NB2 <-
        "Le modèle de NB2 ne s'ajuste pas bien à vos données. Vous devriez utiliser un autre modèle."
    }
    
    #presentation des resultats
    newdata <- data.frame(Moyenne = c("moyenne"))
    method.NB2 <- "NB2"
    predM.NB2 <-
      predict(
        model.NB2,
        newdata,
        full = TRUE,
        se.fit = TRUE,
        type = "link"
      )
    CPUEfinal.NB2 <-
      exp(predM.NB2$fit)  %>% round(digits = 2) #JM dit de pas faire round a lunite (digits=0) pour la moyenne, mais plutot 2
    confint.NB2 <-
      confint(model.NB2) #ON DEVRAIT PRENDRE CA COMME lwr and upr limites ! Ca donne aussi un IC95%
    
    # linf <- (CPUEfinal.NB2 + confint.NB2[1]) %>% round(digits = 2)
    # lsup <- (CPUEfinal.NB2 - confint.NB2[2]) %>% round(digits = 2)
    linf <-
      exp(predM.NB2$fit - (1.96 * predM.NB2$se.fit)) %>%   round(digits = 2) #JM dit de pas faire round a lunite (digits=0) pour la moyenne, mais plutot 2
    lsup <-
      exp(predM.NB2$fit + (1.96 * predM.NB2$se.fit)) %>%   round(digits = 2) #JM dit de pas faire round a lunite (digits=0) pour la moyenne, mais plutot 2
    
    
    resultCPUE.NB2 <- data.frame(
      "Méthode" = method.NB2,
      Ajustement = ajustement.NB2,
      CPUE = CPUEfinal.NB2,
      "IC95" = paste0("(", linf, "-", lsup, ")"),
      Commentaires = commentaires.NB2,
      modeltemp = "model.NB2"
    )
    
    
    # CMP ---------------------------------------------------------------------
    
    library(glmmTMB)
    model.CMP <-
      glmmTMB::glmmTMB(CPUE ~ 1,
                       family = glmmTMB::compois(link = "log"),
                       data = temp)#modele glm initial pour le calcul
    
    
    dfun <- function(obj) {
      residuals(obj, type = "pearson")
    }
    sfun <- function(n, obj) {
      simulate(obj)[[1]]
    }
    
    ffun_CMP <- function(response) {
      fit <- try(glmmTMB(response ~ 1, family = nbinom1, data = temp),
                 silent = TRUE)
      while (class(fit) == "try-error") {
        response2 <- sfun(1, m.df_EXT.CMP)
        fit <-
          try(glmmTMB(response2 ~ 1, family = nbinom1, data = temp),
              silent = TRUE)
      }
      return(fit)
    }
    
    
    set.seed(2023) # fonction set.seed(2023) sert à reproduire toujours la même valeur car hnp fonctionne
    #à partir de simulations. Utiliser 10 ou 100 simulations pour le même modèle va produire une estimation semblable à
    #la première, mais différente aussi.
    
    hnp_CMP <- list()
    
    for (i in 1:1) {
      #remettre a 100
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
    
    summary_hnp_CMP <- sapply(hnp_CMP, function(x)
      x$out / x$total * 100)
    
    ajustement.CMP <-
      mean(summary_hnp_CMP) %>% as.numeric() %>% round(digits = 2)
    
    #presentation des resultats
    newdata <- data.frame(Moyenne = c("moyenne"))
    method.CMP <- "CMP"
    predM.CMP <-
      predict(
        model.CMP,
        newdata,
        full = TRUE,
        se.fit = TRUE,
        type = "link"
      )
    CPUEfinal.CMP <-
      exp(predM.CMP$fit)  %>% round(digits = 2) #JM dit de pas faire round a lunite (digits=0) pour la moyenne, mais plutot 2
    CPUEfinal.CMP
    confint.CMP <-
      confint(model.CMP) #ON DEVRAIT PRENDRE CA COMME lwr and upr limites ! Ca donne aussi un IC95%
    commentaires.CMP <- NA
    if (ajustement.CMP < 10) {
      commentaires.CMP <- "Le modèle CMP s'ajuste bien à vos données."
    }
    if (ajustement.CMP > 10) {
      commentaires.CMP <-
        "Le modèle CMP ne s'ajuste pas bien à vos données. Vous devriez utiliser un autre modèle."
    }
    # linf <- (CPUEfinal.CMP + confint.CMP[1]) %>% round(digits = 2)
    # lsup <- (CPUEfinal.CMP - confint.CMP[2]) %>% round(digits = 2)
    #
    linf <-
      exp(predM.CMP$fit - (1.96 * predM.CMP$se.fit)) %>%   round(digits = 2) #JM dit de pas faire round a lunite (digits=0) pour la moyenne, mais plutot 2
    lsup <-
      exp(predM.CMP$fit + (1.96 * predM.CMP$se.fit)) %>%   round(digits = 2) #JM dit de pas faire round a lunite (digits=0) pour la moyenne, mais plutot 2
    
    resultCPUE.CMP <- data.frame(
      "Méthode" = method.CMP,
      Ajustement = ajustement.CMP,
      CPUE = CPUEfinal.CMP,
      "IC95" = paste0("(", linf, "-", lsup, ")"),
      Commentaires = commentaires.CMP,
      modeltemp = "model.CMP"
    )
    

    
    # GP ---------------------------------------------------------------------
    
    library(glmmTMB)
    model.GP <-
      glmmTMB::glmmTMB(CPUE ~ 1,
                       family = glmmTMB::genpois(link = "log"),
                       data = temp)#modele glm initial pour le calcul
    
    
    dfun <- function(obj) {
      residuals(obj, type = "pearson")
    }
    sfun <- function(n, obj) {
      simulate(obj)[[1]]
    }
    
    ffun_GP <- function(response) {
      fit <- try(glmmTMB(response ~ 1, family = nbinom1, data = temp),
                 silent = TRUE)
      while (class(fit) == "try-error") {
        response2 <- sfun(1, m.df_EXT.GP)
        fit <-
          try(glmmTMB(response2 ~ 1, family = nbinom1, data = temp),
              silent = TRUE)
      }
      return(fit)
    }
    
    
    set.seed(2023) # fonction set.seed(2023) sert à reproduire toujours la même valeur car hnp fonctionne
    #à partir de simulations. Utiliser 10 ou 100 simulations pour le même modèle va produire une estimation semblable à
    #la première, mais différente aussi.
    
    hnp_GP <- list()
    
    for (i in 1:1) {
      #remettre a 100
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
    
    summary_hnp_GP <- sapply(hnp_GP, function(x)
      x$out / x$total * 100)
    
    ajustement.GP <-
      mean(summary_hnp_GP) %>% as.numeric() %>% round(digits = 2)
    
    #presentation des resultats
    newdata <- data.frame(Moyenne = c("moyenne"))
    method.GP <- "GP"
    predM.GP <-
      predict(model.GP,
              newdata,
              full = TRUE,
              se.fit = TRUE,
              type = "link")
    CPUEfinal.GP <-
      exp(predM.GP$fit)  %>% round(digits = 2) #JM dit de pas faire round a lunite (digits=0) pour la moyenne, mais plutot 2
    CPUEfinal.GP
    confint.GP <-
      confint(model.GP) #ON DEVRAIT PRENDRE CA COMME lwr and upr limites ! Ca donne aussi un IC95%
    commentaires.GP <- NA
    if (ajustement.GP < 10) {
      commentaires.GP <- "Le modèle GP s'ajuste bien à vos données."
    }
    if (ajustement.GP > 10) {
      commentaires.GP <-
        "Le modèle GP ne s'ajuste pas bien à vos données. Vous devriez utiliser un autre modèle."
    }
    # linf <- (CPUEfinal.GP + confint.GP[1]) %>% round(digits = 2)
    #lsup <- (CPUEfinal.GP - confint.GP[2]) %>% round(digits = 2)
    
    linf <-
      exp(predM.GP$fit - (1.96 * predM.GP$se.fit)) %>%   round(digits = 2) #JM dit de pas faire round a lunite (digits=0) pour la moyenne, mais plutot 2
    lsup <-
      exp(predM.GP$fit + (1.96 * predM.GP$se.fit)) %>%   round(digits = 2) #JM dit de pas faire round a lunite (digits=0) pour la moyenne, mais plutot 2
    
    
    
    resultCPUE.GP <- data.frame(
      "Méthode" = method.GP,
      Ajustement = ajustement.GP,
      CPUE = CPUEfinal.GP,
      "IC95" = paste0("(", linf, "-", lsup, ")"),
      Commentaires = commentaires.GP,
      modeltemp = "model.GP"
    )
    
    # presentation des resultats ----------------------------------------------
    
    CLEAN <-
      rbind(resultCPUE.p,
            resultCPUE.NB2,
            resultCPUE.NB1,
            resultCPUE.CMP,
            resultCPUE.GP)
    
    
    #adequation de lajustement de chaque mod, ici pour M-2009 mp est inadequat donc on devrait pas le considerer pour le reste
    #fxn hnp avec iteration, residu en dehors de lenveloppe simule (un bon modele en a <5% par ex. )
    #PRESENTER OUI, mais bien identifier WARNING en fxn du CLASSEMENT DE LIDENTIFIANT comme JM : Le modele sajuste mal a vos donnees, vous devriez utiliser un autre modele!
    
    
    #COMPROMIS SLMT SI LES 2 modeles sont CHILL.
    #Donc les bios ont au moins les results des 2 modeles, et compromis en plus si classement de lajustement CHILL
    
    #I want the indices of all elements that match the minimum value.
    indice <- CLEAN$Ajustement
    indice <- which(indice == min(indice))
    n_indice <- length(unique(indice)) %>% as.numeric()
    
    
    modeles_egalite <- c(CLEAN[indice, ]$modeltemp)
    modeles_egalite_nom <- c(CLEAN[indice, ]$'Méthode')
    
    #  CLEAN <- cbind(Ajustement = c(0.9,0.9,13.3,15.6),modeltemp = c("mod1","mod2","mod3","mod4")) %>% as.data.frame()
    #   CLEAN$Ajustement <- as.numeric(CLEAN$Ajustement)
    #   indice <- CLEAN$Ajustement
    #   indice <- which(indice == min(indice))
    #   modeles_egalite_nom <- c(CLEAN[indice,]$'modeltemp')
    # paste0("Comme les modèles ",paste(glue::glue(list(modeles_egalite_nom)))," s'ajustent bien à vos données, compromis recommandé.")
    # #
    #
    
    if (n_indice >= 2) {
      sorti <- model.sel(mget(modeles_egalite))
      compromis <- model.avg(sorti , revised.var = TRUE)
      
      newdata <- data.frame(moyenne = c("moyenne"))
      method.compromis <- "Compromis"
      
      predM.compromis <-
        predict(
          compromis,
          newdata,
          full = TRUE,
          se.fit = TRUE,
          type = "link"
        )
      CPUEfinal.compromis <-
        exp(predM.compromis$fit)  %>% round(digits = 2) #JM dit de pas faire round a lunite (digits=0) pour la moyenne, mais plutot 2
      
      
      #confint.compromis <- confint(compromis) #ON DEVRAIT PRENDRE CA COMME lwr and upr limites ! Ca donne aussi un IC95%
      #linf <- (CPUEfinal.compromis - confint.compromis[1]) %>% round(digits = 2)
      #lsup <- (CPUEfinal.compromis + confint.compromis[2]) %>% round(digits = 2)
      
      #MAIS CA NE FONCTIONNE PAS DONC JE FAIS LA VIEILLE METHODE ?
      
      linf <-
        exp(predM.compromis$fit - (1.96 * predM.compromis$se.fit)) %>%   round(digits = 2) #JM dit de pas faire round a lunite (digits=0) pour la moyenne, mais plutot 2
      lsup <-
        exp(predM.compromis$fit + (1.96 * predM.compromis$se.fit)) %>%   round(digits = 2) #JM dit de pas faire round a lunite (digits=0) pour la moyenne, mais plutot 2
      
      
      
      
      
      resultCPUE.compromis <- data.frame(
        "Méthode" = method.compromis,
        Ajustement = 0,
        CPUE = CPUEfinal.compromis,
        "IC95" = paste0("(", linf, "-", lsup, ")"),
        Commentaires = paste0(
          "Comme les modèles ",
          paste(glue::glue(list(
            modeles_egalite_nom
          ))),
          " s'ajustent bien à vos données, compromis recommandé."
        ),
        modeltemp = "temps"
      )
      
      CLEAN <- rbind(CLEAN, resultCPUE.compromis)
    }
    
    
    
    # if (ajustement.NB2 < 10 && ajustement.p < 10) {
    #   sorti <- model.sel(model.NB2, model.p )
    #   compromis <- model.avg(sorti , revised.var = TRUE      )
    #
    #   newdata <- data.frame(moyenne=c("moyenne"))
    #   method.compromis <- "Compromis NB2 et Poisson"
    #
    #   predM.compromis <-  predict(compromis,newdata, full = TRUE, se.fit = TRUE, type = "link")
    #   CPUEfinal.compromis <- exp(predM.compromis$fit)  %>% round(digits = 2) #JM dit de pas faire round a lunite (digits=0) pour la moyenne, mais plutot 2
    #
    #
    #   #confint.compromis <- confint(compromis) #ON DEVRAIT PRENDRE CA COMME lwr and upr limites ! Ca donne aussi un IC95%
    #    #linf <- (CPUEfinal.compromis - confint.compromis[1]) %>% round(digits = 2)
    #   #lsup <- (CPUEfinal.compromis + confint.compromis[2]) %>% round(digits = 2)
    #
    #   #MAIS CA NE FONCTIONNE PAS DONC JE FAIS LA VIEILLE METHODE ?
    #
    #   linf <-   exp(predM.compromis$fit-(1.96*predM.compromis$se.fit)) %>%   round(digits = 2) #JM dit de pas faire round a lunite (digits=0) pour la moyenne, mais plutot 2
    #   lsup <-  exp(predM.compromis$fit+(1.96*predM.compromis$se.fit)) %>%   round(digits = 2) #JM dit de pas faire round a lunite (digits=0) pour la moyenne, mais plutot 2
    #
    #
    #
    #
    #
    #   resultCPUE.compromis <- data.frame("Méthode" = method.compromis,
    #                                      Ajustement = NA,
    #                                      CPUE = CPUEfinal.compromis,
    #                                      "IC95" = paste0("(",linf,"-",lsup,")"),
    #                                      Commentaires = "Comme les modèles Poisson et NB2 s'ajustent bien à vos données, compromis recommandé.")
    #
    #   CLEAN <- rbind(CLEAN, resultCPUE.compromis)
    # }
    
    CLEAN$Ajustement <- as.numeric(CLEAN$Ajustement)
    CLEAN <-
      CLEAN %>% dplyr::select("Méthode" , "Ajustement", "CPUE", "IC95", "Commentaires")
    
    CLEAN <- CLEAN %>% dplyr::arrange(Ajustement)
    CLEAN
  }
