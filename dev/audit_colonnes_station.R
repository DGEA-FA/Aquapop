# audit_colonnes_station.R
# Analyse de l’utilisation des colonnes de load_station() dans le projet
# Date : 14 avril 2025

library(stringr)
library(dplyr)
library(tibble)
library(here)

message("[audit_colonnes_station] Début de l'analyse...")

# Colonnes potentielles dans load_station()
colonnes_station <- c(
  "no_lac", "typ_pech", "annee", "no_station",
  "lat_dd.dec", "long_dd.dec",
  "prof_deb", "prof_fin",
  "st_valide", "st_hasard",
  "date_pose", "date_leve",
  "heure_pose", "min_pose",
  "heure_leve", "min_leve",
  "h_pose", "h_leve",
  "pose", "leve",
  "duree",
  "type_maill", "comments"
)

# Fichiers R à analyser (depuis le dossier R/)
fichiers <- list.files(here::here("R"), pattern = "\\.R$", full.names = TRUE, recursive = TRUE)

tout_le_code <- unlist(lapply(fichiers, readLines, warn = FALSE))

colonnes_utilisees <- unique(unlist(lapply(colonnes_station, function(nom) {
  motifs <- c(
    paste0("station\\$", nom),
    paste0("station\\[\\[\\s*[\"']", nom, "[\"']\\s*\\]\\]"),
    paste0("filter\\s*\\(.*", nom),
    paste0("group_by\\s*\\(.*", nom),
    paste0("mutate\\s*\\(.*", nom)
  )
  lignes_trouvees <- tout_le_code[str_detect(tout_le_code, paste(motifs, collapse = "|"))]
  if (length(lignes_trouvees) > 0) return(nom) else return(NULL)
})))

res <- tibble(
  colonne = colonnes_station,
  utilisee = colonne %in% colonnes_utilisees
)

print(res)

# write.csv(res, here::here("dev", "analyse_colonnes_utilisees_station.csv"), row.names = FALSE)

message("[audit_colonnes_station] Analyse terminée.")
