psd_byclass <- function(data, sp) {
    df  <- data %>% filter(sp==sp) %>% droplevels()# selectionner slmt le data necessaire
    Classename <- c("Sous-stock","Stock","Qualité", "Préférée","Mémorable","Trophée")
    
    
  if (sp =="SANA") {
    breakClass <- c(0,300,500,650,800,1000)
  } else if (sp =="SAFO"){
    breakClass <- c(0,150,250,325,400,500)
  } else if (sp =="SAVI"){
    breakClass <- c(0,250,380,510,630,760)
  }
  
  limInfStock <- breakClass [2] # limite inférieure de la classe Stock pour cette sp.
   
  bunch <-  df  %>% 
    filter(ltm>=limInfStock ) %>%
    mutate(gcat=FSA::lencat(ltm, breaks=breakClass,use.names=TRUE, droplevels=TRUE))
 
  bunch <- mutate(bunch, Classe=plyr::mapvalues(gcat, from=breakClass,to=Classename)) #classe de taille texte 
  
  
  if (sp =="SANA") {
    bunch  <- mutate(bunch , range=plyr::mapvalues(gcat, from=breakClass,to=c("<300","300-499","500-649","650-799","800-999",">=1000"))) 
  } else if (sp == "SAFO"){
    bunch  <- mutate(bunch , range=plyr::mapvalues(gcat, from=breakClass,to=c("<150","150-249","250-324","325-399","400-499",">=500"))) 
  } else if (sp == "SAVI"){
    bunch  <- mutate(bunch , range=plyr::mapvalues(gcat, from=nestlac$breakClass,to=c("<250","250-379","380-509","510-629","630-759",">=760"))) 
    } 
  
  
  y <- bunch %>% group_by(gcat, Classe, range) %>% summarise(n=n()) %>% droplevels()#nb de poissons dans ch classe de taille
  bunch <- merge(bunch, y, by=c("gcat", "Classe", "range")) #merge le df avec le nb de poissons
  
  
  #ID <- unique(bunch$ID)
  gfreq <-  xtabs(~gcat, data = bunch)
  
  psdtable <- prop.table(gfreq) * 100 
  psdtable <- psdtable %>% as.data.frame()
  
  bunch <- merge(bunch, psdtable, by="gcat") #merge le df avec le nb de poissons
  bunch  <- bunch %>% dplyr::select(c(Classe, range, n, Freq ))
  bunch <-  bunch %>% group_by(Classe, range, n, Freq ) %>% summarise()
  
   virgin <- tibble(Classe = Classename)
  
 if (sp =="SANA") {
                     virgin$range <- c("<300","300-499","500-649","650-799","800-999",">=1000") 
                   } else if (unique(bunch$sp %>% droplevels()) %>% as.character() =="SAFO"){
                     virgin$range <- c("<150","150-249","250-324","325-399","400-499",">=500")
                   } else if (unique(bunch$sp %>% droplevels()) %>% as.character() =="SAVI"){
                     virgin$range <- c("<250","250-379","380-509","510-629","630-759",">=760")
                   }
  
   complet <- merge(virgin, bunch, by=c("Classe", "range"),all.x=TRUE) #merge le df avec le nb de poissons
   
   complet$Classe <- as.factor(complet$Classe)
   complet$Classe <- factor(complet$Classe, levels=Classename)
   complet <- complet %>% arrange(Classe)
   complet$Freq <- as.numeric(complet$Freq) %>% round(digits = 0)
   
   complet[1,3] <- data %>% filter(sp==sp) %>%  filter(ltm < limInfStock ) %>% summarise(n())
 
  colnames(complet)[2] <- "Intervalle (mm)"
  
  complet <- complet %>% mutate(Freq = ifelse(is.na(Freq), "0", Freq))
  colnames(complet)[4] <- "%"

  complet$n<- complet$n %>% tidyr::replace_na(0)
  # complet$'%'<- complet$'%' %>% tidyr::replace_na(0)  
  # complet$'%'<- complet$'%' %>% as.numeric() %>% round(digits = 0)
complet
  }