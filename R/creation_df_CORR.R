creation_df_CORR <- function(data, peakplus, agemax) {

FM <- fishmethods::agesurv(
  type = 1, #l’analyse porte sur des données individuelles
  age = data$age,
  full = peakplus, #valeur PeakPlus
  last = agemax, #l’âge maximal
  estimate = c("z"),
  method = c("cr","crcb","pois") ) 
#biaise par classe de taille vide donc on tweak plus  
    
dlimb <- FM$data #reprendre les données individuelles qui sont enregistré dans l’objet que l’on vient de créer

#corriger le jeu de données
prepare_data <- function(data) {
  full_range <- tibble(age = seq(min(data$age),
                                 max(data$age),
                                 1))
  new_dat <- full_join(data,full_range)
  new_dat$number[is.na(new_dat$number)] <- 0
  new_dat <- new_dat %>% arrange(age)
  return(new_dat)
}    

df_CORR <- prepare_data(dlimb) #La ou les classes d’âge manquantes ont été insérées dans la descending limb of the catch curve. Ce script vient de vous sauver passablement de temps de préparation de données
df_CORR
 
}

    

    

