# Analyse de l’utilisation des colonnes de load_recolte() dans le projet
# Exécuter ce script pour voir quelles colonnes sont réellement utilisées
# Date : 15 avril 2025

library(stringr)
library(dplyr)
library(tibble)
library(here)

message("[audit_colonnes_recolte] Début de l'analyse...")

colonnes_recolte <- c(
  "no_lac", "typ_pech", "no_station", "sp", "annee",
  "nb_capture", "nb_pese", "comments_recolte"
)

# Robuste : cherche tous les scripts R dans le dossier R/
fichiers <- list.files(here::here("R"), pattern = "\\.R$", full.names = TRUE, recursive = TRUE)

tout_le_code <- unlist(lapply(fichiers, readLines, warn = FALSE))

colonnes_utilisees <- unique(unlist(lapply(colonnes_recolte, function(nom) {
  motifs <- c(
    paste0("recolte\\$", nom),
    paste0("recolte\\[\\[\\s*[\"']", nom, "[\"']\\s*\\]\\]"),
    paste0("filter\\s*\\(.*", nom),
    paste0("group_by\\s*\\(.*", nom),
    paste0("mutate\\s*\\(.*", nom)
  )
  lignes_trouvees <- tout_le_code[str_detect(tout_le_code, paste(motifs, collapse = "|"))]
  if (length(lignes_trouvees) > 0) return(nom) else return(NULL)
})))

res <- tibble(
  colonne = colonnes_recolte,
  utilisee = colonne %in% colonnes_utilisees
)

print(res)

# write.csv(res, here::here("dev", "analyse_colonnes_utilisees_recolte.csv"), row.names = FALSE)

message("[audit_colonnes_recolte] Analyse terminée.")
