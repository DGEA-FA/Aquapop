# Charger les packages nécessaires
if (!requireNamespace("fs", quietly = TRUE)) install.packages("fs")
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")

library(fs)
library(stringr)

# Définir le terme à rechercher
terme_a_chercher <- "afficher_table"

# Lister tous les fichiers R dans le projet (sauf ceux dans test/)
# files <- dir_ls(path = ".", recurse = TRUE, glob = "*.R")
files <- dir_ls(path = ".", recurse = TRUE, regexp = "\\.(R|Rmd|qmd)$")

# Exclure les scripts du dossier test/ (y compris ce script lui-même)
files <- files[!str_detect(files, "^test/|nom_de_ce_script.R$")]

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
resultats <- files |>
  lapply(rechercher_terme, terme = terme_a_chercher) |>
  purrr::compact() |>
  dplyr::bind_rows()

# Afficher les résultats
if (!is.null(resultats)) {
  print(resultats)
} else {
  cat("Aucune correspondance trouvée pour '", terme_a_chercher, "'.\n", sep = "")
}
# super_recherche <- function(terme, dossier = ".", extensions = c("R", "Rmd", "qmd"), exclure_dossiers = c("test", "renv")) {
#   # Charger les packages requis
#   if (!requireNamespace("fs", quietly = TRUE)) install.packages("fs")
#   if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
#   if (!requireNamespace("tibble", quietly = TRUE)) install.packages("tibble")
#   if (!requireNamespace("purrr", quietly = TRUE)) install.packages("purrr")
#   
#   library(fs)
#   library(stringr)
#   library(tibble)
#   library(purrr)
#   
#   # Créer un regex d’extensions
#   extension_regex <- paste0("\\.(", paste(extensions, collapse = "|"), ")$")
#   
#   # Lister les fichiers correspondants
#   fichiers <- dir_ls(path = dossier, recurse = TRUE, regexp = extension_regex)
#   
#   # Exclure les dossiers non désirés
#   if (length(exclure_dossiers) > 0) {
#     exclusion_regex <- paste0("^", exclure_dossiers, collapse = "|")
#     fichiers <- fichiers[!str_detect(fichiers, exclusion_regex)]
#   }
#   
#   # Fonction interne de recherche
#   rechercher_terme <- function(fichier, terme) {
#     lignes <- tryCatch(readLines(fichier, warn = FALSE), error = function(e) return(character(0)))
#     lignes_correspondantes <- stringr::str_which(lignes, fixed(terme))
#     
#     if (length(lignes_correspondantes) > 0) {
#       return(tibble(
#         fichier = fichier,
#         ligne = lignes_correspondantes,
#         texte = lignes[lignes_correspondantes]
#       ))
#     } else {
#       return(NULL)
#     }
#   }
#   
#   # Appliquer à tous les fichiers
#   resultats <- fichiers |>
#     map(rechercher_terme, terme = terme) |>
#     compact() |>
#     dplyr::bind_rows()
#   
#   if (nrow(resultats) == 0) {
#     message("✅ Aucune correspondance trouvée pour '", terme, "'.")
#   }
#   
#   return(resultats)
# }
# 
# # 
# # # Rechercher où ID est utilisé (préfixé ou non)
# # super_recherche("capture_verif")
# # 
# # # Rechercher une colonne spécifique
# # super_recherche("lac$ID")
# # 
# # # Rechercher un champ dans un module
# # super_recherche("input$nom_du_champ")
# # 
# # # Rechercher dans tout sauf 'test' et 'data'
# # super_recherche("sp_queentextelatin", exclure_dossiers = c("test", "data"))
