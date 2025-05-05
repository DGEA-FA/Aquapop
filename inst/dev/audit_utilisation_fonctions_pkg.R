library(dplyr)
library(purrr)
library(tibble)
library(stringr)

# Étape 1 : lister les fonctions du package
fonctions_nlstools <- getNamespaceExports("shinyBS")

# Étape 2 : définir les fichiers à analyser
chemin_projet <- "C:/Users/bruca03/aqua_local/aquapop"

fichiers <- list.files(
  path = chemin_projet,
  pattern = "\\.(R|Rmd|Rnw|qmd)$",
  recursive = TRUE,
  full.names = TRUE
)

# Étape 3 : pour chaque fichier, vérifier quelles fonctions nlstools y sont utilisées
resultats <- purrr::map_dfr(fichiers, function(f) {
  lignes <- tryCatch(readLines(f, warn = FALSE), error = function(e) return(character(0)))
  
  map_dfr(fonctions_nlstools, function(fn) {
    if (any(str_detect(lignes, paste0("\\b", fn, "\\s*\\(")))) {
      tibble(fichier = f, fonction = fn)
    } else {
      NULL
    }
  })
})
resultats
