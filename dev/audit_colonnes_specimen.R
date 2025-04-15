# dev/audit_colonnes_specimen.R
# Analyse de l’utilisation des colonnes de load_specimen() dans le projet
# Exécuter ce script pour voir quelles colonnes sont réellement utilisées
# Date : 15 avril 2025

library(stringr)
library(dplyr)
library(tibble)
library(here)

message("[audit_colonnes_specimen] Début de l'analyse...")

# Liste complète des colonnes retournées par load_specimen()
colonnes_specimen <- c(
  "no_lac", "typ_pech", "no_station", "no_specimen", "sp",
  "ltm", "lf", "masse", "age",
  "sexe", "maturite", "marquage",
  "ind_insec", "ind_benth", "ind_planc", "ind_chyme", "ind_vide", "ind_poiss",
  "poiss1", "poiss2",
  "annee", "comments_specimen"
)

# Lecture de tout le code des scripts R
fichiers <- list.files(here::here("R"), pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
tout_le_code <- unlist(lapply(fichiers, readLines, warn = FALSE))

# Vérifie l'utilisation de chaque colonne dans le code source
colonnes_utilisees <- unique(unlist(lapply(colonnes_specimen, function(nom) {
  motifs <- c(
    paste0("specimen\\$", nom),
    paste0("specimen\\[\\[\\s*[\"']", nom, "[\"']\\s*\\]\\]"),
    paste0("filter\\s*\\(.*", nom),
    paste0("group_by\\s*\\(.*", nom),
    paste0("mutate\\s*\\(.*", nom)
  )
  lignes_trouvees <- tout_le_code[str_detect(tout_le_code, paste(motifs, collapse = "|"))]
  if (length(lignes_trouvees) > 0) return(nom) else return(NULL)
})))

# Tableau des résultats
res <- tibble(
  colonne = colonnes_specimen,
  utilisee = colonne %in% colonnes_utilisees
)

print(res)

# Optionnel : export CSV
# write.csv(res, here::here("dev", "analyse_colonnes_utilisees_specimen.csv"), row.names = FALSE)

message("[audit_colonnes_specimen] Analyse terminée.")
