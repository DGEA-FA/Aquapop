creation_df_EXT <- function(data, peakplus, agemax) {
  #Nous allons maintenant estimer Z à nouveau à partir des méthodes CR , CRCB et PM à partir de df_CORR
  #Remarquez qu’en utilisant les données de fréquences d’âge provenant de FM$data, on se trouve à avoir changé le nom des variables pour age et number, tout simplement parce que c’est de cette façon que
  #fishmethods les nomme. On doit donc utiliser ces nouveaux noms de variables dans notre script ayant recours à la fonction agesurv() et en sauvegardant les résultats dans FM_CORR
  FM_df_CORR <- fishmethods::agesurv(
    type = 2,
    #type =2, cest par frequence
    age = data$age,
    number = data$number,
    full = peakplus,
    last = agemax,
    estimate = c("z"),
    method = c("cr", "crcb", "pois")
  )
  
  #Si on agrandi le jeu de données avec des zéros (EXT = artificially EXTended with zero counts)
  df_EXT <- rbind(data,
                  cbind(
                    age = (agemax + 1):(3 * agemax),
                    number = rep(0, agemax)
                  ))
  
  df_EXT
  
}
