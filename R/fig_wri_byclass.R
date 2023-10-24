fig_wri_byclass <- function(data, espece) {
  #aide reference Ogle IFAR p.113
  init <- data %>% filter(sp==espece)# selectionner slmt le data necessaire
  init <- subset(init ,!is.na(masse) & !is.na(ltm)) # this data frame needed to be “cleaned” by removing all records where mesures of either length or weight were missing
  init <- init  %>% dplyr::select(no_specimen,sp,ltm,masse,age,sexe,maturite) #selectionner slmt les col necessaires
  
  
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
  init <- mutate(init, Classe=plyr::mapvalues(gcat, from=breakClass,to=Classename)) #classe de taille format texte 
  
  #definir les gcat
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
  
  
 # ggWribyClass <- 
    pred %>% arrange(gcat) %>% ggplot(aes(x=Classe, y=fit)) +
    geom_point() +
    geom_point(data = init, aes(x=Classe.x,y=Wri), shape = 21, colour = "black", fill = "white", size = 1,alpha=0.5)+
    geom_errorbar(aes(ymin=lwr, ymax=upr), colour="black", width=.1) +
    xlab("Classe de taille") +
    ylab("Indice de condition (%)") +
    #ggtitle(data$ID)+ 
    theme(panel.background = element_rect(fill="white", 
                                          colour="white", 
                                          linewidth=0.5),
          panel.grid.minor.x=element_blank(),
          panel.grid.major.x=element_blank(),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.y=element_blank(),
          axis.text.y.left = element_text(color = "black" ), 
          axis.text.x = element_text(color = "black"),
          axis.title.y.left = element_text(color="black", 
                                           hjust = 0.5),
          axis.title.x = element_text(color="black",
                                      hjust = 0.5),
          plot.margin=unit(c(0.5,0.1,0.2,0.1), "cm"),
          axis.line = element_line(colour = "black")  ) +
    scale_y_continuous(#expand = c(0, 0),
      breaks= scales::breaks_extended(n=5,only.loose=TRUE)
    ) +  
    scale_x_discrete(#expand = c(0, 0),
      breaks= Classename,
      drop =FALSE,
      limits = c("Sous-stock","Stock","Qualité", "Préférée","Mémorable","Trophée") # Use limits to adjust in what order
    ) +
    annotate("segment", x= -Inf, xend = Inf, y= 100, yend = 100, linewidth=0.5, color="black", linetype=2)  
  
 # ggWribyClass
  
  
  
  }

