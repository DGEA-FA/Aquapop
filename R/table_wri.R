table_wri <- function(data, espece) {
  
  init <- data %>% dplyr::filter(sp==espece)# selectionner slmt le data necessaire
  init <- subset(init ,!is.na(masse) & !is.na(ltm)) # this data frame needed to be “cleaned” by removing all records where mesures of either length or weight were missing
  init <- init  %>% dplyr::select(no_specimen,sp,ltm,masse,age,sexe,maturite)
  
  
  slope <- TableWeightRef$slope[TableWeightRef$sp==espece] %>% as.numeric() #isoler la valeur de la pente
  int <- TableWeightRef$int[TableWeightRef$sp==espece] %>% as.numeric() #isoler la valeur de l'intercept
  ltmmin <- TableWeightRef$min.TL[TableWeightRef$sp==espece] %>% as.numeric() # isoler la limite minimum de taille
  
  init <- init %>% filter(ltm>=ltmmin) #filter les donnees pour retirer les specimens dont la taille est hors limite
  init <- mutate(init , prediction=10^(int + (slope * log10(ltm)))) #valeur de masse predite
  init <- mutate(init , Wri=masse*100/prediction) #valeur de masse relative
  
  
  #tous selon le guide de normalisation tome 2
  
  if (unique(init$sp %>% droplevels()) %>% as.character()=="SANA") {
    breakClass <- c(0,300,500,650,800,1000)
  } else if (unique(init$sp %>% droplevels()) %>% as.character() =="SAFO"){
    breakClass <- c(0,150,250,325,400,500)
  } else if (unique(init$sp %>% droplevels()) %>% as.character() =="SAVI"){
    breakClass <- c(0,250,380,510,630,760)
  } 
  
  
  Classename <- c("Sous-stock","Stock","Qualité", "Préférée","Mémorable","Trophée")
  init  <- mutate(init, gcat=lencat(ltm, breaks=breakClass,as.fact=TRUE)) #classe de taille, voir p.30 de Ogle 2016 si questions
  init <- mutate(init, Classe=plyr::mapvalues(gcat, from=breakClass,to=Classename)) #classe de taille texte 
  
  
  if (unique(init$sp %>% droplevels()) %>% as.character()=="SANA") {
    init  <- mutate(init , range=plyr::mapvalues(gcat, from=breakClass,to=c("<300","300-499","500-649","650-799","800-999",">=1000"))) 
  } else if (unique(init$sp %>% droplevels()) %>% as.character() =="SAFO"){
    init  <- mutate(init , range=plyr::mapvalues(gcat, from=breakClass,to=c("<150","150-249","250-324","325-399","400-499",">=500"))) 
  } else if (unique(init$sp %>% droplevels()) %>% as.character() =="SAVI"){
    init  <- mutate(init , range=plyr::mapvalues(gcat, from=breakClass,to=c("<250","250-379","380-509","510-629","630-759",">=760"))) 
  }
  
  
  aov1 <-  lm(Wri~gcat, data = init)
  
  y <- init %>% group_by(gcat, Classe, range) %>% summarise(n=n()) %>% droplevels()#nb de poissons dans ch classe de taille
  init <- merge(init, y, by="gcat") #merge le df avec le nb de poissons
  
  grps <-levels(y$gcat)
  nd <- data.frame(gcat=factor(grps, levels=grps)) 
  pred <-predict(aov1 , nd , interval="confidence") %>% as.data.frame()
  pred <- cbind(pred, nd )
  pred <- merge(pred,
                y, by="gcat")
  pred$fit <- round(pred$fit, digit=0)
  pred$lwr <- round(pred$lwr, digit=0)
  pred$upr <- round(pred$upr, digit=0)
  pred <- pred %>% mutate(IC95=paste0("[",lwr,"-",upr,"]"))
  pred <- pred %>% dplyr::select(Classe, fit, IC95, n)
  pred <- pred %>% rename(Groupe=Classe)
  pred
  
  #Ajouter une colonne pour Trophée, même si aucun spécimen (indique 0 plutôt que NA).
  vec <- c("Sous-stock","Stock","Qualité", "Préférée","Mémorable","Trophée") 
  
  pred <- pred %>%
   dplyr::add_row(Groupe = setdiff(vec, pred$Groupe)) %>%
    tidyr::complete(Groupe, fill = list(fit = 0, IC95 = "0",n = 0))
  
  

    aov2 <-  lm(Wri~sexe, data = init)
  y2 <- init %>% group_by(sexe) %>% summarise(n=n()) %>% droplevels()#nb de poissons dans ch classe de taille

  grps2 <-levels(y2$sexe)
  nd2 <- data.frame(sexe=factor(grps2, levels=grps2)) 
  pred2 <-predict(aov2 , nd2 , interval="confidence") %>% as.data.frame()
  pred2 <- cbind(pred2, nd2 )
  pred2 <- merge(pred2,
                y2, by="sexe")
  pred2$fit <- round(pred2$fit, digit=0)
  pred2$lwr <- round(pred2$lwr, digit=0)
  pred2$upr <- round(pred2$upr, digit=0)
  pred2 <- pred2 %>% mutate(IC95=paste0("[",lwr,"-",upr,"]"))
  pred2 <- pred2 %>% dplyr::select(sexe, fit, IC95, n)
  pred2 <- pred2 %>% rename(Groupe=sexe)
  
  pred2
  
  aov3 <-  lm(Wri~1, data = init)
  y3 <- init %>%  summarise(n=n()) %>% droplevels()#nb de poissons dans ch classe de taille

  nd3 <- data.frame(Groupe="Tous") 
  pred3 <-predict(aov3 , nd3 , interval="confidence") %>% as.data.frame()
  pred3 <- cbind(pred3, nd3 )
  pred3 <- merge(pred3,
                 y3)
  pred3$fit <- round(pred3$fit, digit=0)
  pred3$lwr <- round(pred3$lwr, digit=0)
  pred3$upr <- round(pred3$upr, digit=0)
  pred3 <- pred3 %>% mutate(IC95=paste0("[",lwr,"-",upr,"]"))
  pred3 <- pred3 %>% dplyr::select(Groupe, fit, IC95, n)
  
  
  pred3
  shapiro.test(residuals(aov3))
  summary(init$Wri)
  
  CLEAN <- rbind(pred3, pred2,pred)
  CLEAN <- CLEAN %>% filter(Groupe!="IND")
  CLEAN <- CLEAN %>% rename(Wr=fit,
                            "IC 95%" = IC95)
  CLEAN <- CLEAN %>% mutate(Groupe=plyr::mapvalues(Groupe, from=c("F","M"), to=c("Femelle","Mâle")))
  
  CLEAN2 <- t(CLEAN)  %>% as.data.frame()        
  colnames(CLEAN2) <- CLEAN2[1,]
  CLEAN2 <- CLEAN2[-1, ] 
  CLEAN2
  
  
  
  }

