# Analyse de l’utilisation des colonnes de load_lac() dans le projet
# Exécuter ce script pour voir quelles colonnes sont réellement utilisées
# Date : 14 avril 2025

library(stringr)
library(dplyr)
library(tibble)
library(here)

message("[audit_colonnes_lac] Début de l'analyse...")

colonnes_lac <- c(
  "region_admin", "no_lac", "nom_lac", "typ_pech", "annee", "sp_pen",
  "long_dd.dec", "lat_dd.dec", "terr_faun", "zon_pech",
  "superficie_ha", "perimetre_km", "prof_max_m", "prof_moy_m", "comments", "ID"
)

# Robuste : cherche les fichiers depuis la racine du projet
fichiers <- list.files(here::here("R"), pattern = "\\.R$", full.names = TRUE, recursive = TRUE)

tout_le_code <- unlist(lapply(fichiers, readLines, warn = FALSE))

colonnes_utilisees <- unique(unlist(lapply(colonnes_lac, function(nom) {
  motifs <- c(
    paste0("lac\\$", nom),
    paste0("lac\\[\\[\\s*[\"']", nom, "[\"']\\s*\\]\\]"),
    paste0("filter\\s*\\(.*", nom),
    paste0("group_by\\s*\\(.*", nom),
    paste0("mutate\\s*\\(.*", nom)
  )
  lignes_trouvees <- tout_le_code[str_detect(tout_le_code, paste(motifs, collapse = "|"))]
  if (length(lignes_trouvees) > 0) return(nom) else return(NULL)
})))

res <- tibble(
  colonne = colonnes_lac,
  utilisee = colonne %in% colonnes_utilisees
)

print(res)

# write.csv(res, here::here("dev", "analyse_colonnes_utilisees_lac.csv"), row.names = FALSE)

message("[audit_colonnes_lac] Analyse terminée.")
