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