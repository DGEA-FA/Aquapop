agemax <- function(data) {
  age_max <-
    max(na.omit(data$age)) #Trouver le plus vieil âge et ignorer les NA de votre jeu de données s’il en contient (sinon = erreur)
  age_max
}