biomasse_table <- function(capture, specimen, espece) {
  
  datacapt <- capture %>%  dplyr::filter(sp==espece)  %>% droplevels() 
  datacapt <- datacapt %>% dplyr::select("no_station", "nb_capture",  "nb_pese"   )
  
  dataspec <- specimen %>%  dplyr::filter(sp==espece)  %>% droplevels() 
  alldata <- merge(dataspec, datacapt, all.x = TRUE) #ch ligne 1 specimen ou on a ajoute n total de capture et de poissons peses par station.On part de ca pour tous les autres filtres
  
  alldata <- alldata %>% dplyr::filter(type_maill %in% c("G",NA))  %>% droplevels() #on veut juste les filets expérimentaux
  
  # alldata <- alldata %>% dplyr::filter(ST_HASARD=="O") #slmt les stations hasard ?
  #alldata <- alldata %>% dplyr::filter(ST_VALIDE=="O") #slmt les stations valides? 
  
  
  alldata <- alldata %>% dplyr::filter(no_station!="") #retirer station NA où oubli d'écrire la station
  alldata$sexe[is.na(alldata$sexe)] <- "IND"
  
  nstation <- length(unique(capture$no_station)) %>% as.numeric()
  
  #Tous
  temp <- alldata %>%  dplyr::group_by(no_station) %>% 
    summarise(BPUE = sum(masse, na.rm = TRUE),
              Group="Tous")
  
  biomassefix <- sum(temp$BPUE, na.rm = TRUE) %>% as.numeric()
  biomassefix <- biomassefix/1000
  biomassefix <- biomassefix %>% round(digits=1)
  
  #on teste un premier modele pour BPUElac
  mp <-  glm(BPUE~1, family = poisson, data = temp)#modele glm initial pour le calcul
  
  #on teste un 2e modele
  mnb2 <-  MASS::glm.nb(BPUE~1, data = temp)
  
  #compromis entre les 2
  compromis <- model.avg(model.sel(mnb2, mp#, revised.var = TRUE
  )) 
  
  #Result 
  newdata <- data.frame(moyenne=c("moyenne"))
  predM <-  predict(compromis,newdata, full = TRUE, se.fit = TRUE, type = "link") 
  
  BPUEfinal <-exp(predM$fit)
  BPUEfinal <- BPUEfinal/1000
  BPUEfinal <- BPUEfinal  %>% round(digits=1)
  
  lowerlimBPUE <-   exp(predM$fit-(1.96*predM$se.fit))
  lowerlimBPUE <- lowerlimBPUE/1000
  lowerlimBPUE <- lowerlimBPUE %>% round(digits=1)
  upperlimBPUE <-  exp(predM$fit+(1.96*predM$se.fit))
  upperlimBPUE <- upperlimBPUE/1000
  upperlimBPUE <- upperlimBPUE %>% round(digits=1)
  
  
  method <- "compromis mp et mnb2"
  resultBPUE <-  cbind(newdata,  as.numeric(BPUEfinal),  as.numeric(lowerlimBPUE), as.numeric(upperlimBPUE), method)  
  names(resultBPUE)[2] <- "moy"
  names(resultBPUE)[3] <- "LL"
  names(resultBPUE)[4] <- "UL"
  names(resultBPUE)[5] <- "methode"
  
  
  Tous <- as.data.frame(c(
    Biomasse = biomassefix, 
    Perc = NA,
    BPUE = BPUEfinal,
    IC95 = paste0("(",lowerlimBPUE,"-",upperlimBPUE,")")))
  
  
  colnames(Tous) <- "Tous"
  
  # MFIND -------------------------------------------------------------------
  temp <- alldata %>%  dplyr::group_by(no_station, sexe) %>% droplevels() %>% 
    summarise(massesum = sum(masse, na.rm = TRUE))
  
  temp <- temp %>%  dplyr::group_by(sexe) %>% summarise(Biomasse = round(sum(massesum, na.rm = TRUE)/1000, digits=1),
                                                        BPUE = round((sum(massesum, na.rm = TRUE)/nstation)/1000,digits =1))
  
  temp <- temp %>% mutate(Perc = Biomasse*100/biomassefix,
                          IC95 =NA)
  temp <- temp %>% mutate(Perc = round(Perc, digits = 0))
  
  
  temp <- temp %>% mutate(sexe=plyr::mapvalues(sexe, from=c("F","M", "IND"), to=c("Femelle","Mâle", "Sexe inconnu")))
  temp <- temp %>% dplyr::select(c(sexe,Biomasse,Perc,BPUE,IC95))
  
  temp <- t(temp) %>% as.data.frame()
  colnames(temp) <- temp[1,]
  temp <- temp[-1, ] 
  #temp <- as.numeric(temp$Femelle)
  #temp$Femelle <- as.numeric(temp$Femelle)
  MFIND <- temp #CLEAN
  
  
 # MFIND <- mutate_all(temp, function(x) as.numeric(as.character(x))) #CLEAN
  
  # FMmature ----------------------------------------------------------------

  temp <- alldata %>% filter(maturite=="O" & sexe %in% c("M","F")) %>% droplevels() #slmt poissons matures de sexe connu
  temp <- temp %>% dplyr::group_by(no_station, sexe) %>% 
    summarise(massesum = sum(masse, na.rm = TRUE))
  
  temp <- temp %>%  dplyr::group_by(sexe) %>% summarise(Biomasse = round(sum(massesum, na.rm = TRUE)/1000,digits=1),
                                                        BPUE = round((sum(massesum, na.rm = TRUE)/nstation)/1000,digits =1))
  
  temp <- temp %>% mutate(Perc = Biomasse*100/biomassefix,
                          IC95 =NA)
  temp <- temp %>% mutate(Perc = round(Perc, digits = 0))
  
  
  temp <- temp %>% mutate(sexe=plyr::mapvalues(sexe, from=c("F","M"), to=c("Repro. actifs ♀","Repro. actifs ♂")))
  temp <- temp %>% dplyr::select(c(sexe,Biomasse,Perc,BPUE,IC95))
  
  temp <- t(temp) %>% as.data.frame()
  colnames(temp) <- temp[1,]
  temp <- temp[-1, ] 
  #FMmature <- mutate_all(temp, function(x) as.numeric(as.character(x)))  #CLEAN
  FMmature <- temp #CLEAN
  
  # Immature ----------------------------------------------------------------
  
  #Imm. ou reprod. inactifs
  temp <- alldata %>% filter(maturite=="N") %>% droplevels() #slmt poissons Imm. ou reprod. inactifss
  temp <- temp %>% dplyr::group_by(no_station, maturite) %>% 
    summarise(massesum = sum(masse, na.rm = TRUE))
  
  temp <- temp %>%  dplyr::group_by(maturite) %>% summarise(Biomasse = round(sum(massesum, na.rm = TRUE)/1000,digits=1),
                                                            BPUE = round((sum(massesum, na.rm = TRUE)/nstation)/1000,digits =1))
  
  temp <- temp %>% mutate(Perc = Biomasse*100/biomassefix,
                          IC95 =NA)
  temp <- temp %>% mutate(Perc = round(Perc, digits = 0))
  
  temp <- temp %>% dplyr::select(c(maturite, Biomasse, Perc, BPUE, IC95))
  
  temp <- temp %>% dplyr::select(-c(maturite))
  
  temp <- t(temp) %>% as.data.frame()
  colnames(temp) <- "Imm. ou reprod. inactifs"
  
 # Immature <- mutate_all(temp, function(x) as.numeric(as.character(x)))  #CLEAN
  Immature <- temp #CLEAN
  
  
  
  
  # Statut reproducteur inconnu ----------------------------------------------------------------
  
  temp <- alldata %>% filter(is.na(maturite)) %>% droplevels() 
  temp <- temp %>% dplyr::group_by(no_station) %>% 
    summarise(massesum = sum(masse, na.rm = TRUE))
  
  temp <- temp %>% summarise(Biomasse = round(sum(massesum, na.rm = TRUE)/1000,digits=1),
                                                            BPUE = round((sum(massesum, na.rm = TRUE)/nstation)/1000,digits =1))
  
  temp <- temp %>% mutate(Perc = Biomasse*100/biomassefix,
                          IC95 =NA)
  temp <- temp %>% mutate(Perc = round(Perc, digits = 0))
  
  temp <- temp %>% dplyr::select(c(Biomasse, Perc, BPUE, IC95))

  temp <- t(temp) %>% as.data.frame()
  colnames(temp) <- "Statut reprod. inconnu"
  
  inconnu <- temp #CLEAN
  
  
  
  
  
  
  
  
  
  
  CLEAN <- cbind(Tous, MFIND, FMmature, Immature, inconnu)
  
  #IDfix <- unique(capture$ID) %>% as.character()
  #CLEAN <- CLEAN %>% mutate(ID= paste(IDfix))
  
  CLEAN <-  CLEAN %>% dplyr::select(c("Tous", "Femelle", "Mâle","Sexe inconnu", "Repro. actifs ♀","Repro. actifs ♂", "Imm. ou reprod. inactifs","Statut reprod. inconnu"))
  CLEAN <- t(CLEAN) %>% as.data.frame()
  CLEAN <- CLEAN %>% mutate(Groupe =c("Tous", "Femelle", "Mâle","Sexe inconnu", "Repro. actifs ♀","Repro. actifs ♂", "Imm. ou reprod. inactifs","Statut reprod. inconnu"))
  CLEAN <- CLEAN %>%  dplyr::select(c(Groupe, everything())) #Moving the last column to the start
  CLEAN$Perc[CLEAN$Groupe=="Tous"] <- 100
  CLEAN$Perc <- format(round(as.numeric(CLEAN$Perc), digits =0), nsmall = 0)
  

  
  CLEAN
}