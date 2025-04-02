# ============================================================================
# Script : exemple_utilisation.R
# Rôle   : Démonstration simple de l’utilisation des fonctions métier AquaPop
#          hors de l'application Shiny
# ============================================================================

# CHARGER LES DÉPENDANCES ET LES FONCTIONS MÉTIER -------------------------

# Charger les packages
source("R/load_packages.R")

# Charger toutes les fonctions définies 
script_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)

invisible(lapply(script_files, function(f) {
  tryCatch(
    source(f, local = FALSE),
    error = function(e) message("Erreur dans ", f, ": ", e$message)
  )
}))

# DÉFINIR LES PARAMÈTRES D’EXTRACTION -------------------------------------

# Chemin vers le fichier Excel d’extraction
path <- "data/Extract IFA_R04_AquaPop.xlsx"

# Paramètres d’identification du sous-ensemble de données à analyser
typ_pech <- "PENT"
no_lac   <- "01565"
annee    <- 2008



# EXTRAIRE LES DONNÉES POUR ANALYSE ---------------------------------------

# Cette fonction regroupe la logique d’import et de filtrage par typ_pech / lac / année
df <- get_analysis_data(
  path     = path,
  typ_pech = typ_pech,
  no_lac   = no_lac,
  annee    = annee
)

# Les objets retournés sont organisés en liste
data_station     <- df$data_station
specimen         <- df$specimen
specimen_valid   <- df$specimen_valid
capture          <- df$capture

# Extraire et filtrer la feuille "Lac"
data_lac <- load_lac(path, namesheet = "Lac") |>
  # appliquer les filtres 
  filtrer_par_pen_lac_annee(typ_pech = typ_pech,
                            no_lac = no_lac,
                            annee = annee)

# Générer un tableau synthèse (optionnel)
table_recap(data_lac = data_lac, data_station = data_station)

# EXTRAIRE LES MÉTADONNÉES LIÉES AU TYPE DE PÊCHE -------------------------

info_pen <- get_info_pen(typ_pech)
info_pen

# TAILLE MASSE ÂGE  -------------------------------------------------------

# Pour obtenir le tableau de données
df_taillemasseage <- taille_masse_age(data = specimen_valid, format = "data.frame")

# Pour afficher le flextable
taille_masse_age(data = specimen_valid, format = "flextable")

# Exemple : Exporter manuellement un tableau
# download_data(df_taillemasseage, path = "df_taillemasseage.xlsx")



# Croissance --------------------------------------------------------------
# 2. Ajuster les modèles et créer la table de comparaison
table_modele <- courbe_croissance_comparaison(
  data = specimen,
  format = "data.frame"
)

# Afficher la table dans la console
print(table_modele)


# 3. Sélectionner le meilleur modèle automatiquement


modele_best <- select_best_croissance_model(table_modele)
cat("Meilleur modèle sélectionné :", modele_best, "\n")

# 4. Générer le graphique du modèle choisi
p <- courbe_croissance_plot(
  dfspecimen = specimen,
  tablemodele = table_modele,
  modele = modele_best
)

# Afficher le graphique
print(p)



# PSD ---------------------------------------------------------------------

psd_indice(data = specimen_valid, format = "flextable")
psd_indice(data = specimen_valid, format = "data.frame")
psd_byclass(data = specimen_valid, format = "data.frame")
psd_byclass(data = specimen_valid, format = "flextable")
psd_byclass(data = specimen_valid, format = "plot")

# Relation masse-longueur ---------------------------------------------------------------------

relation_masse_longueur(data = specimen, format = "data.frame")
relation_masse_longueur(data = specimen, format = "flextable")
relation_masse_longueur(data = specimen, format = "plot")

# Structure de taille ---------------------------------------------------------------------
# ----- 1. Graphique de base (aucun groupement) -----
structure_taille(data = specimen_valid, format = "plot")

# ----- 2. Graphique groupé par sexe -----
structure_taille(data = specimen_valid, groupement = "sexe", format = "plot")

# ----- 3. Graphique groupé par maturité -----
structure_taille(data = specimen_valid, groupement = "maturite", format = "plot")

# ----- 4. Export des données pour tableau -----
df_taille_plot <- structure_taille(data = specimen_valid, groupement = "sexe", format = "data.frame")
print(df_taille_plot)

# ----- 5. Affichage en flextable -----
structure_taille(data = specimen_valid, groupement = "marquage", format = "flextable")

# Structure d'âge ---------------------------------------------------------------------

# ----- 1. Graphique de base (aucun groupement) -----
structure_age(data = specimen_valid, format = "plot")

# ----- 2. Graphique groupé par sexe -----
structure_age(data = specimen_valid, groupement = "sexe", format = "plot")

# ----- 3. Graphique groupé par maturité -----
structure_age(data = specimen_valid, groupement = "maturite", format = "plot")

# ----- 4. Export des données pour tableau -----
df_age_plot <- structure_age(data = specimen_valid, groupement = "sexe", format = "data.frame")
print(df_age_plot)

# ----- 5. Affichage en flextable -----
structure_age(data = specimen_valid, groupement = "sexe", format = "flextable")

# Indice de condition -----------------------------------------------------

# Exemple : Indice de condition (Wr)

# Format tableau (data.frame ou flextable)
indice_condition(data = specimen, format = "data.frame")
indice_condition(data = specimen, format = "flextable")

# Format graphique (Wr par sexe ou par classe de taille)
indice_condition(data = specimen, format = "plot_tous")
indice_condition(data = specimen, format = "plot_byclass")

