verifier_dataframes <- function(dataframe, nom_dataframe) {
  if (nrow(dataframe) == 0) {
    return(paste(nom_dataframe, "est vide."))
  }
  return(NULL)
}

verifier_doublons <- function(dataframe, nom_dataframe) {
  doublons <- dataframe[duplicated(dataframe), ]
  if (nrow(doublons) > 0) {
    return(paste("Doublons trouvés dans", nom_dataframe))
  }
  return(NULL)
}

# Définir les fonctions pour chaque modèle de croissance
vBert <- function(age, Linf, K, t0) { Linf * (1 - exp(-K * (age - t0)))}
Gompt <- function(age, Linf, K, t0) {Linf * exp(-exp(-K * (age - t0)))}
Logis <- function(age, Linf, K, t0) {Linf / (1 + exp(-K * (age - t0)))}
