# Fonction pour vérifier si un dataframe est vide
verifier_dataframe_vide <- function(dataframe, nom_data) {
  if (is.null(dataframe) || nrow(dataframe) == 0) {
    return(
      HTML(paste("<div style='font-weight: bold; color: #AD781C;'>Attention: la base de données brutes", 
                 nom_data, "est vide.</div>"))
    )
  } else {
    return(
      paste("La base de données brutes", nom_data, "a", nrow(dataframe), "lignes.")
    )
  }
}