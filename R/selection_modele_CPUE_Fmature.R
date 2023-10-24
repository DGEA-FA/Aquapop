selection_modele_CPUE_Fmature <- function(capture, specimen, espece, station) {
  
  datacapt <- capture %>%  dplyr::filter(sp==espece)  %>% droplevels() 
  datacapt <- datacapt %>% dplyr::select("no_station", "nb_capture",  "nb_pese"   )
  dataspec <- specimen %>%  
    dplyr::filter(sp==espece) %>%
    filter(maturite=="O" & sexe %in% c("F"))  %>% droplevels() 
  alldata <- merge(dataspec, datacapt, all.x = TRUE) #ch ligne 1 specimen ou on a ajoute n total de capture et de poissons peses par station.On part de ca pour tous les autres filtres
  
  #alldata <- alldata %>% dplyr::filter(TYPE_MAILL %in% c("G",NA))  %>% droplevels() #on veut juste les filets expérimentaux
  #UPDATE: LES TECHS SPECIFIENT QUEL COTE DU FILET EXP EST SUR LE BORD DE LA RIVE. DONC NON, PAS DE FILTRE POUR LE TYPE DENGIN
  
  
  alldata <- alldata %>% dplyr::filter(st_hasard=="O") #slmt les stations hasard  tirage aleatoire
  alldata <- alldata %>% dplyr::filter(st_valide %in% c("O",NA)) #slmt les stations valides. Filet entortillés par ex. 
  #Pourrait etre valide mais hasard non, et tout les combinaisons donc imp les 2 oui. 
  #Parfois NA partout donc on selectionne oui, sauf si explicitement N
  
  
  alldata <- alldata %>% dplyr::filter(no_station!="") #retirer station NA où oubli d'écrire la station

  #ICI CONSIDERER LES STATIONS VIDES
  nstation <- length(unique(station$no_station)) %>% as.numeric()
  listallstation <- unique(station$no_station)
  dfvide <- data.frame(no_station = listallstation)
  
  alldata <- merge(dfvide, alldata, by="no_station",  all.x = TRUE) # on ajoute ligne pour stations vides 
  
  #Tous
  temp <- alldata %>%  dplyr::group_by(no_station) %>% 
    summarise(CPUE = length(which(no_specimen != 0)), #Count the number of non-zero elements of each column
              Group="Tous")
 # writexl::write_xlsx(temp,file.path("C:", "Users", "carol", "OneDrive", "EmploiMFFP", "Hiver2022-2023", "Dynapop_dossierCommun", "ExemplepourJM.xlsx"  ))
  
  abondancefix <- sum(temp$CPUE) %>% as.numeric()
  
  ## Poisson (p), on teste un premier modele pour CPUElac 
  
  model.p <-  glm(CPUE~1, family = poisson, data = temp)#modele glm initial pour le calcul
  
  library(hnp)
  
  set.seed(2023) # fonction set.seed(2023) sert à reproduire toujours la même valeur car hnp fonctionne 
  #à partir de simulations. Utiliser 10 ou 100 simulations pour le même modèle va produire une estimation semblable à
  #la première, mais différente aussi.
  
  hnp_p <- list()
  
  for(i in 1:100) {
    
    hnp_p[[i]] <- hnp(model.p,resid.type="pearson",how.many.out=TRUE,plot.sim=FALSE)
    
  }
  
  summary_hnp_p <- sapply(hnp_p,function(x) x$out/x$total*100)
  
  ajustement.p <- mean(summary_hnp_p) %>% as.numeric() %>% round(digits = 2)
  
  #presentation des resultats
  newdata <- data.frame(Moyenne=c("moyenne"))
  method.p <- "Poisson" 
  predM.p <-  predict(model.p,newdata, full = TRUE, se.fit = TRUE, type = "link") 
  CPUEfinal.p <- exp(predM.p$fit)  %>% round(digits = 2) #JM dit de pas faire round a lunite (digits=0) pour la moyenne, mais plutot 2
  CPUEfinal.p
  confint.p <- confint(model.p) #ON DEVRAIT PRENDRE CA COMME lwr and upr limites ! Ca donne aussi un IC95%
  commentaires.p <- NA
  if (ajustement.p <10) {
    commentaires.p <- "Le modèle de Poisson s'ajuste bien à vos données."
  }
  if (ajustement.p > 10) {
    commentaires.p <- "Le modèle de Poisson ne s'ajuste pas bien à vos données. Vous devriez utiliser un autre modèle."
  }
  linf <- (CPUEfinal.p - confint.p[1]) %>% round(digits = 2)
  lsup <- (CPUEfinal.p + confint.p[2]) %>% round(digits = 2)
  
  resultCPUE.p <- data.frame("Méthode" = method.p,
                             Ajustement = ajustement.p,
                             CPUE = CPUEfinal.p,
                             "IC95" = paste0("(",linf,"-",lsup,")"),
                             Commentaires = commentaires.p)
  
  
  
  
  
  
  # NB2 (nb2), on teste un 2e modele
  model.nb2 <-  MASS::glm.nb(CPUE~1, data = temp)
  set.seed(2023)
  
  hnp_nb2 <- list()
  
  for(i in 1:100) {
    
    hnp_nb2[[i]] <- hnp(model.nb2, resid.type="pearson", how.many.out=TRUE, plot.sim=FALSE)
    
  }
  
  summary_hnp_nb2 <- sapply(hnp_nb2,function(x) x$out/x$total*100)
  ajustement.nb2 <- mean(summary_hnp_nb2) %>% as.numeric()  %>% round(digits = 2)
  commentaires.nb2 <- NA
  
  if (ajustement.nb2 <10) {
    commentaires.nb2 <- "Le modèle de NB2 s'ajuste bien à vos données."
  }
  if (ajustement.nb2 > 10) {
    commentaires.nb2 <- "Le modèle de NB2 ne s'ajuste pas bien à vos données. Vous devriez utiliser un autre modèle."
  }
  
  #presentation des resultats
  newdata <- data.frame(Moyenne=c("moyenne"))
  method.nb2 <- "NB2" 
  predM.nb2 <-  predict(model.nb2, newdata, full = TRUE, se.fit = TRUE, type = "link") 
  CPUEfinal.nb2 <- exp(predM.nb2$fit)  %>% round(digits = 2) #JM dit de pas faire round a lunite (digits=0) pour la moyenne, mais plutot 2
  confint.nb2 <- confint(model.nb2) #ON DEVRAIT PRENDRE CA COMME lwr and upr limites ! Ca donne aussi un IC95%
  
  linf <- (CPUEfinal.nb2 - confint.nb2[1]) %>% round(digits = 2)
  lsup <- (CPUEfinal.nb2 + confint.nb2[2]) %>% round(digits = 2)
  
  
  resultCPUE.nb2 <- data.frame("Méthode" = method.nb2,
                               Ajustement = ajustement.nb2,
                               CPUE = CPUEfinal.nb2,
                               "IC95" = paste0("(",linf,"-",lsup,")"),
                               Commentaires = commentaires.nb2)
  
  CLEAN <- rbind(resultCPUE.p, resultCPUE.nb2)
  
  
  
  #adequation de lajustement de chaque mod, ici pour M-2009 mp est inadequat donc on devrait pas le considerer pour le reste
  #fxn hnp avec iteration, residu en dehors de lenveloppe simule (un bon modele en a <5% par ex. )
  #PRESENTER OUI, mais bien identifier WARNING en fxn du CLASSEMENT DE LIDENTIFIANT comme JM : Le modele sajuste mal a vos donnees, vous devriez utiliser un autre modele! 
  
  
  #COMPROMIS SLMT SI LES 2 modeles sont CHILL. 
  #Donc les bios ont au moins les results des 2 modeles, et compromis en plus si classement de lajustement CHILL 
  
  if (ajustement.nb2 < 10 && ajustement.p < 10) {
    sorti <- model.sel(model.nb2, model.p )
    compromis <- model.avg(sorti , revised.var = TRUE      )
    
    newdata <- data.frame(moyenne=c("moyenne"))
    method.compromis <- "Compromis NB2 et Poisson" 
    
    predM.compromis <-  predict(compromis,newdata, full = TRUE, se.fit = TRUE, type = "link") 
    CPUEfinal.compromis <- exp(predM.compromis$fit)  %>% round(digits = 2) #JM dit de pas faire round a lunite (digits=0) pour la moyenne, mais plutot 2
    
    
    #confint.compromis <- confint(compromis) #ON DEVRAIT PRENDRE CA COMME lwr and upr limites ! Ca donne aussi un IC95%
     #linf <- (CPUEfinal.compromis - confint.compromis[1]) %>% round(digits = 2)
    #lsup <- (CPUEfinal.compromis + confint.compromis[2]) %>% round(digits = 2)
    
    #MAIS CA NE FONCTIONNE PAS DONC JE FAIS LA VIEILLE METHODE ?
    
    linf <-   exp(predM.compromis$fit-(1.96*predM.compromis$se.fit)) %>%   round(digits = 2) #JM dit de pas faire round a lunite (digits=0) pour la moyenne, mais plutot 2
    lsup <-  exp(predM.compromis$fit+(1.96*predM.compromis$se.fit)) %>%   round(digits = 2) #JM dit de pas faire round a lunite (digits=0) pour la moyenne, mais plutot 2
    
    
   
    
    
    resultCPUE.compromis <- data.frame("Méthode" = method.compromis,
                                       Ajustement = NA,
                                       CPUE = CPUEfinal.compromis,
                                       "IC95" = paste0("(",linf,"-",lsup,")"),
                                       Commentaires = "Comme les modèles Poisson et NB2 s'ajustent bien à vos données, compromis recommandé.")
    
    CLEAN <- rbind(CLEAN, resultCPUE.compromis)
  }
  
#  CLEAN <- CLEAN %>% select("Méthode" , "Ajustement", "CPUE", "IC95", "Commentaires")
  CLEAN
}
  