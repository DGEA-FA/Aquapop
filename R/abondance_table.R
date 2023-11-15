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

# tous --------------------------------------------------------------------

 temp <- alldata  %>% summarise(Groupe="Tous", 
                                Abondance =n(), 
                                Perc = NA, 
                                CPUE = NA, 
                                IC95 = NA,
                                ratioMF = NA)
 abondancefix <- temp$Abondance %>% as.numeric()
 
 TOUS <- temp #CLEAN



# MFIND -------------------------------------------------------------------

levels(alldata$sexe) <- c("F","M", "IND")
temp <- alldata %>%  dplyr::group_by(sexe, .drop = FALSE)  %>% 
  summarise(Abondance =n(), 
            Perc = NA, 
            CPUE = NA, 
            IC95 = NA,
            ratioMF = NA)

temp <- temp %>% mutate(Perc = round(Abondance*100/abondancefix,digits =0))

temp <- temp %>% mutate(sexe=plyr::mapvalues(sexe, from=c("F","M", "IND"), to=c("Femelle","Mâle", "Sexe inconnu")))
temp <- temp %>% rename(Groupe=sexe)


temp <- temp %>% dplyr::select(Groupe,Abondance,Perc,CPUE,IC95, ratioMF)


MFIND <- temp #CLEAN



# FMmature ----------------------------------------------------------------

temp <- alldata %>% filter(maturite=="O" & sexe %in% c("M","F")) %>%
  dplyr::group_by(sexe) %>%
  summarise(Abondance =n(), 
            Perc = NA, 
            CPUE = NA, 
            IC95 = NA,
            ratioMF = NA)

temp <- temp %>% mutate(Perc = round(Abondance*100/abondancefix,digits =0))
temp <- temp %>% mutate(sexe=plyr::mapvalues(sexe, from=c("F","M"), to=c("Repro. actifs ♀","Repro. actifs ♂")))
temp <- temp %>% rename(Groupe=sexe)
temp <- temp %>% dplyr::select(Groupe,Abondance,Perc,CPUE,IC95, ratioMF)

FMmature <- temp #CLEAN


# Immature ----------------------------------------------------------------

temp <- alldata %>% filter(maturite=="N") %>%
  dplyr::group_by(maturite) %>%
  summarise(Groupe="Imm. ou reprod. inactifs", 
            Abondance =n(), 
            Perc = NA, 
            CPUE = NA, 
            IC95 = NA,
            ratioMF = NA)


nMaleimm <- alldata %>% filter(maturite=="N" & sexe == "M") 
nMaleimm <- length(unique(nMaleimm$no_specimen)) %>% as.numeric()

nFemaleimm <- alldata %>% filter(maturite=="N" & sexe == "F") 
nFemaleimm <- length(unique(nFemaleimm$no_specimen)) %>% as.numeric()

temp <- temp %>% dplyr::mutate(Perc = round(Abondance*100/abondancefix,digits =0))
temp <- temp %>% dplyr::select(-maturite)

temp <- temp %>% dplyr::mutate(ratioMF = paste0(nMaleimm,":",nFemaleimm))


temp <- temp %>% dplyr::select(Groupe,Abondance,Perc,CPUE,IC95, ratioMF)

Immature <- temp #CLEAN


# Statut reproducteur inconnu ----------------------------------------------------------------

temp <- alldata %>% filter(is.na(maturite)) %>%
  summarise(Groupe="Statut reprod. inconnu", 
            Abondance =n(), 
            Perc = NA, 
            CPUE = NA, 
            IC95 = NA,
            ratioMF = NA)


nMaleinconnu <- alldata %>% filter(is.na(maturite) & sexe == "M") 
nMaleinconnu <- length(unique(nMaleinconnu$no_specimen)) %>% as.numeric()

nFemaleinconnu <- alldata %>% filter(is.na(maturite) & sexe == "F") 
nFemaleinconnu <- length(unique(nFemaleinconnu$no_specimen)) %>% as.numeric()

temp <- temp %>% dplyr::mutate(Perc = round(Abondance*100/abondancefix,digits =0))
#temp <- temp %>% dplyr::select(-maturite)

temp <- temp %>% dplyr::mutate(ratioMF = paste0(nMaleinconnu,":",nFemaleinconnu))


temp <- temp %>% dplyr::select(Groupe,Abondance,Perc,CPUE,IC95, ratioMF)

inconnu <- temp #CLEAN



CLEAN <- rbind(TOUS, MFIND, FMmature, Immature,inconnu)

#ratio MF
nbmale <- CLEAN$Abondance[CLEAN$Groupe=="Mâle"] %>% as.numeric()
nbfemelle <- CLEAN$Abondance[CLEAN$Groupe=="Femelle"] %>% as.numeric()

#ratioMF <-  round(as.numeric(nbmale/nbfemelle), digits = 1) 
ratioMF <-  paste0(nbmale,":",nbfemelle)
CLEAN$ratioMF[CLEAN$Groupe=="Tous"] <- ratioMF
CLEAN$Perc[CLEAN$Groupe=="Tous"] <- 100
#CLEAN <- CLEAN %>% dplyr::mutate(Perc = round(as.numeric(Perc),digits =0))

CLEAN$Perc <- format(round(CLEAN$Perc, digits =0), nsmall = 0)


CLEAN$Groupe <- factor(CLEAN$Groupe, levels=c("Tous", "Femelle", "Mâle", "Sexe inconnu", "Repro. actifs ♀","Repro. actifs ♂", "Imm. ou reprod. inactifs", "Statut reprod. inconnu"))
CLEAN <- CLEAN %>% arrange(Groupe)
#CLEAN <- CLEAN %>% mutate(ID= IDfix)
#IDfix <- unique(capture$ID) %>% as.character()
#CLEAN <- CLEAN %>% mutate(ID= paste(IDfix))
#CLEAN <- CLEAN %>%  dplyr::select(ID, everything()) #Moving the last column to the start

CLEAN <- CLEAN %>% rename("Nombre" = Abondance,
                          "Prop. (%)" = Perc#,
                          #"Ratio M:F" = ratioMF
                          )
#colnames(CLEAN)[5] <- 'IC 95%' #renommer la colonne 
CLEAN
}
