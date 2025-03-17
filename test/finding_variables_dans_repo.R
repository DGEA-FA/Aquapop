# Charger les packages nécessaires
if (!requireNamespace("fs", quietly = TRUE)) install.packages("fs")
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")

library(fs)
library(stringr)

# Définir le terme à rechercher
terme_a_chercher <- "minitable_param_model_add_a50"

# Lister tous les fichiers R dans le projet (sauf ceux dans test/)
files <- dir_ls(path = ".", recurse = TRUE, glob = "*.R")

# Exclure les scripts du dossier test/ (y compris ce script lui-même)
files <- files[!str_detect(files, "^test/")]

# Fonction pour rechercher le terme dans chaque fichier
rechercher_terme <- function(fichier, terme) {
  lignes <- readLines(fichier, warn = FALSE)
  lignes_trouvees <- grep(terme, lignes, value = TRUE)
  
  if (length(lignes_trouvees) > 0) {
    return(data.frame(
      fichier = fichier,
      ligne = grep(terme, lignes),
      texte = lignes_trouvees,
      stringsAsFactors = FALSE
    ))
  }
  return(NULL)
}

# Appliquer la recherche à tous les fichiers
resultats <- do.call(rbind, lapply(files, rechercher_terme, terme = terme_a_chercher))

# Afficher les résultats
if (!is.null(resultats)) {
  print(resultats)
} else {
  cat("Aucune correspondance trouvée pour '", terme_a_chercher, "'.\n", sep = "")
}
