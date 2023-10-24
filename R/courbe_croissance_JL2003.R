courbe_croissance_JL2003 <- function(data, espece) {
 init <-data %>% filter(sp==espece)# selectionner slmt le data necessaire
 init <- subset( init, !is.na(ltm))# this data frame needed to be “cleaned” by removing all records where mesures were missing
 init <- subset( init, !is.na(age))# this data frame needed to be “cleaned” by removing all records where mesures were missing
 
 #Comparaison des estimes de Linf
 #Janoscik et Lester (2003)
 #Etape 1: combien éliminer de specimen
 Nretrait <- round(dim(init)[1]*5/100,0)
 #Etape 2: moyenne des 5 plus grands individus après retrait + correction
 LT <- init$ltm[order(init$ltm,decreasing=TRUE)]
 LinfJL2003 <- mean(LT[c((Nretrait+1):( Nretrait+5))])/0.95
 #coef(fit_vB)[1] #von Bertalanffy
 #LinfJL2003 #Janoscik et Lester (2003)
 LinfJL2003 <- LinfJL2003 %>% round(digits=0)
 
 LinfJL2003
 }

