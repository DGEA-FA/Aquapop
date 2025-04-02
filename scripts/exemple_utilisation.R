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
no_lac   <- "00024"
annee    <- 2015



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
# Exemple d'utilisation de la fonction structure_taille()

# --- Variante 1 : Graphique global (tous les poissons)
structure_taille(
  data = specimen_valid,
  groupement = "tous",
  format = "plot"
)

# --- Variante 2 : Graphique par sexe
structure_taille(
  data = specimen_valid,
  groupement = "sexe",
  format = "plot"
)

# --- Variante 3 : Graphique par statut reproducteur
structure_taille(
  data = specimen_valid,
  groupement = "maturite",
  format = "plot"
)

# --- Variante 4 : Graphique par marquage
structure_taille(
  data = specimen_valid,
  groupement = "marquage",
  format = "plot"
)

# --- Variante 5 : Données brutes du graphique (data.frame) - groupement "sexe"
df_sexe <- structure_taille(
  data = specimen_valid,
  groupement = "sexe",
  format = "data.frame"
)
print(df_sexe)

# --- Variante 6 : Tableau formaté (flextable) - groupement "maturite"
ft_maturite <- structure_taille(
  data = specimen_valid,
  groupement = "maturite",
  format = "flextable"
)
ft_maturite


purrr::walk(
  c("tous", "sexe", "maturite", "marquage"),
  ~ print(structure_taille(data = specimen_valid, groupement = .x, format = "plot"))
)
