abondance_table <- function(capture, specimen, espece) {

  
  datacapt <- capture %>%  dplyr::filter(sp==espece)  %>% droplevels() 
  datacapt <- datacapt %>% dplyr::select("no_station", "nb_capture",  "nb_pese"   )
  
  dataspec <- specimen %>%  dplyr::filter(sp==espece)  %>% droplevels() 
 alldata <- merge(dataspec, datacapt, all.x = TRUE) #ch ligne 1 specimen ou on a ajoute n total de capture et de poissons peses par station.On part de ca pour tous les autres filtres

#alldata <- alldata %>% dplyr::filter(TYPE_MAILL %in% c("G",NA))  %>% droplevels() #on veut juste les filets expérimentaux
  #UPDATE: LES TECHS SPECIFIENT QUEL COTE DU FILET EXP EST SUR LE BORD DE LA RIVE. DONC NON, PAS DE FILTRE POUR LE TYPE DENGIN
 
 alldata <- alldata %>% dplyr::filter(st_hasard=="O") #slmt les stations hasard  tirage aleatoire
 alldata <- alldata %>% dplyr::filter(st_valide %in% c("O",NA)) #slmt les stations valides. Filet entortillés par ex. 
 #Pourrait etre valide mais hasard non, et tout les combinaisons donc imp les 2 oui. 
 #Parfois NA partout donc on selectionne oui, sauf si explicitement N
 
 alldata <- alldata %>% dplyr::filter(no_station!="") #retirer station NA où oubli d'écrire la station
 alldata$sexe[is.na(alldata$sexe)] <- "IND" #quand sexe=NA, jai mis ind
 
 
 
 #TOUS
 temp <- alldata  %>% summarise(Groupe="Tous", 
                                Abondance =n(), 
                                Perc = NA, 
                                CPUE = NA, 
                                IC95 = NA,
                                ratioMF = NA)
 abondancefix <- temp$Abondance %>% as.numeric()
 
 TOUS <- temp #CLEAN


#MFIND
temp <- alldata %>%  dplyr::group_by(sexe)  %>% 
  summarise(Abondance =n(), 
            Perc = NA, 
            CPUE = NA, 
            IC95 = NA,
            ratioMF = NA)

temp <- temp %>% mutate(Perc = round(Abondance*100/abondancefix,digits =0))

temp <- temp %>% mutate(sexe=plyr::mapvalues(sexe, from=c("F","M", "IND"), to=c("Femelle","Mâle", "sexe inconnu")))
temp <- temp %>% rename(Groupe=sexe)


temp <- temp %>% dplyr::select(Groupe,Abondance,Perc,CPUE,IC95, ratioMF)


MFIND <- temp #CLEAN


#FMmature
temp <- alldata %>% filter(maturite=="O" & sexe %in% c("M","F")) %>%
  dplyr::group_by(sexe) %>%
  summarise(Abondance =n(), 
            Perc = NA, 
            CPUE = NA, 
            IC95 = NA,
            ratioMF = NA)

temp <- temp %>% mutate(Perc = round(Abondance*100/abondancefix,digits =0))
temp <- temp %>% mutate(sexe=plyr::mapvalues(sexe, from=c("F","M"), to=c("♀ mature","♂ mature")))
temp <- temp %>% rename(Groupe=sexe)
temp <- temp %>% dplyr::select(Groupe,Abondance,Perc,CPUE,IC95, ratioMF)

FMmature <- temp #CLEAN

#Immature

temp <- alldata %>% filter(maturite=="N") %>%
  dplyr::group_by(maturite) %>%
  summarise(Groupe="Immature", 
            Abondance =n(), 
            Perc = NA, 
            CPUE = NA, 
            IC95 = NA,
            ratioMF = NA)

temp <- temp %>% dplyr::mutate(Perc = round(Abondance*100/abondancefix,digits =0))
temp <- temp %>% dplyr::select(-maturite)
temp <- temp %>% dplyr::select(Groupe,Abondance,Perc,CPUE,IC95, ratioMF)

Immature <- temp #CLEAN


CLEAN <- rbind(TOUS, MFIND, FMmature, Immature)

#ratio MF
nbmale <- CLEAN$Abondance[CLEAN$Groupe=="Mâle"] %>% as.numeric()
nbfemelle <- CLEAN$Abondance[CLEAN$Groupe=="Femelle"] %>% as.numeric()

ratioMF <-  round(as.numeric(nbmale/nbfemelle), digits = 1) 
CLEAN$ratioMF[CLEAN$Groupe=="Tous"] <- ratioMF
CLEAN$Groupe <- factor(CLEAN$Groupe, levels=c("Tous", "Femelle", "Mâle", "♀ mature","♂ mature", "Immature", "sexe inconnu"))

#CLEAN <- CLEAN %>% mutate(ID= IDfix)
#IDfix <- unique(capture$ID) %>% as.character()
#CLEAN <- CLEAN %>% mutate(ID= paste(IDfix))
#CLEAN <- CLEAN %>%  dplyr::select(ID, everything()) #Moving the last column to the start


CLEAN
}
