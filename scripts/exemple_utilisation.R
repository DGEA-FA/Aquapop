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
station_valides     <- df$station_valides
station_hasard_valide     <- df$station_hasard_valide
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

# Maturité sexuelle -------------------------------------------------------

# Longueur à maturité -------------------------------------------------------
# Obtenir les résultats
res <- table_maturite_modeles(specimen, variable = "ltm")
specimen_data = specimen
# Tableau principal recommandé
res$table$df
res$table$flextable
res$message
# # Tous les modèles séparés
# res$table_sep$df
# res$table_sep$flextable
# 
# # Tous les modèles combinés
# res$table_comb$df
# res$table_comb$flextable

best <- res$best_model

# Si c’est un modèle combiné
x <- fit_maturite(
  data = specimen,
  variable = best$variable,
  modele = best$modele,
  lien = best$lien
)
x$table_resultats
x$table_resultats_flextable
x$commentaire
x$graphique

# TLO - probit - ltm
res_tlo_probit_ltm <- fit_maturite(data = specimen, variable = "ltm", modele = "TLO", lien = "probit")
res_tlo_probit_ltm$table_resultats
res_tlo_probit_ltm$graphique
res_tlo_probit_ltm$table_resultats_flextable

# TLO - probit - age
res_tlo_probit_age <- fit_maturite(data = specimen, variable = "age", modele = "TLO", lien = "probit")
res_tlo_probit_age$table_resultats
res_tlo_probit_age$graphique
res_tlo_probit_age$table_resultats_flextable

# TLO - logit - ltm
res_tlo_logit_ltm <- fit_maturite(data = specimen, variable = "ltm", modele = "TLO", lien = "logit")
res_tlo_logit_ltm$table_resultats
res_tlo_logit_ltm$graphique
res_tlo_logit_ltm$table_resultats_flextable

# TLO - logit - age
res_tlo_logit_age <- fit_maturite(data = specimen, variable = "age", modele = "TLO", lien = "logit")
res_tlo_logit_age$table_resultats
res_tlo_logit_age$graphique
res_tlo_logit_age$table_resultats_flextable

# TLO - cloglog - ltm
res_tlo_cloglog_ltm <- fit_maturite(data = specimen, variable = "ltm", modele = "TLO", lien = "cloglog")
res_tlo_cloglog_ltm$table_resultats
res_tlo_cloglog_ltm$graphique
res_tlo_cloglog_ltm$table_resultats_flextable

# TLO - cloglog - age
res_tlo_cloglog_age <- fit_maturite(data = specimen, variable = "age", modele = "TLO", lien = "cloglog")
res_tlo_cloglog_age$table_resultats
res_tlo_cloglog_age$graphique
res_tlo_cloglog_age$table_resultats_flextable


# ADD - logit - ltm
res_add_logit_ltm <- fit_maturite(data = specimen, variable = "ltm", modele = "ADD", lien = "logit")
res_add_logit_ltm$table_resultats
res_add_logit_ltm$graphique
res_add_logit_ltm$table_resultats_flextable

# ADD - logit - age
res_add_logit_age <- fit_maturite(data = specimen, variable = "age", modele = "ADD", lien = "logit")
res_add_logit_age$table_resultats
res_add_logit_age$graphique
res_add_logit_age$table_resultats_flextable

# ADD - cloglog - ltm
res_add_cloglog_ltm <- fit_maturite(data = specimen, variable = "ltm", modele = "ADD", lien = "cloglog")
res_add_cloglog_ltm$table_resultats
res_add_cloglog_ltm$graphique
res_add_cloglog_ltm$table_resultats_flextable

# ADD - cloglog - age
res_add_cloglog_age <- fit_maturite(data = specimen, variable = "age", modele = "ADD", lien = "cloglog")
res_add_cloglog_age$table_resultats
res_add_cloglog_age$graphique
res_add_cloglog_age$table_resultats_flextable

# COM - logit - ltm
res_com_logit_ltm <- fit_maturite(data = specimen, variable = "ltm", modele = "COM", lien = "logit")
res_com_logit_ltm$table_resultats
res_com_logit_ltm$graphique
res_com_logit_ltm$table_resultats_flextable



# COM - logit - age
res_com_logit_age <- fit_maturite(data = specimen, variable = "age", modele = "COM", lien = "logit")
res_com_logit_age$table_resultats
res_com_logit_age$graphique
res_com_logit_age$table_resultats_flextable

# COM - cloglog - ltm
res_com_cloglog_ltm <- fit_maturite(data = specimen, variable = "ltm", modele = "COM", lien = "cloglog")
res_com_cloglog_ltm$table_resultats
res_com_cloglog_ltm$graphique
res_com_cloglog_ltm$table_resultats_flextable

# COM - cloglog - age
res_com_cloglog_age <- fit_maturite(data = specimen, variable = "age", modele = "COM", lien = "cloglog")
res_com_cloglog_age$table_resultats
res_com_cloglog_age$graphique
res_com_cloglog_age$table_resultats_flextable

# INT - logit - ltm
res_int_logit_ltm <- fit_maturite(data = specimen, variable = "ltm", modele = "INT", lien = "logit")
res_int_logit_ltm$table_resultats
res_int_logit_ltm$graphique
res_int_logit_ltm$table_resultats_flextable

# INT - logit - age
res_int_logit_age <- fit_maturite(data = specimen, variable = "age", modele = "INT", lien = "logit")
res_int_logit_age$table_resultats
res_int_logit_age$graphique
res_int_logit_age$table_resultats_flextable

# INT - cloglog - ltm
res_int_cloglog_ltm <- fit_maturite(data = specimen, variable = "ltm", modele = "INT", lien = "cloglog")
res_int_cloglog_ltm$table_resultats
res_int_cloglog_ltm$graphique
res_int_cloglog_ltm$table_resultats_flextable

# INT - cloglog - age
res_int_cloglog_age <- fit_maturite(data = specimen, variable = "age", modele = "INT", lien = "cloglog")
res_int_cloglog_age$table_resultats
res_int_cloglog_age$graphique
res_int_cloglog_age$table_resultats_flextable

# ADD - probit - ltm
res_add_probit_ltm <- fit_maturite(data = specimen, variable = "ltm", modele = "ADD", lien = "probit")
res_add_probit_ltm$table_resultats
res_add_probit_ltm$graphique
res_add_probit_ltm$table_resultats_flextable

# ADD - probit - age
res_add_probit_age <- fit_maturite(data = specimen, variable = "age", modele = "ADD", lien = "probit")
res_add_probit_age$table_resultats
res_add_probit_age$graphique
res_add_probit_age$table_resultats_flextable

# COM - probit - ltm
res_com_probit_ltm <- fit_maturite(data = specimen, variable = "ltm", modele = "COM", lien = "probit")
res_com_probit_ltm$table_resultats
res_com_probit_ltm$graphique
res_com_probit_ltm$table_resultats_flextable

# COM - probit - age
res_com_probit_age <- fit_maturite(data = specimen, variable = "age", modele = "COM", lien = "probit")
res_com_probit_age$table_resultats
res_com_probit_age$graphique
res_com_probit_age$table_resultats_flextable

# INT - probit - ltm
res_int_probit_ltm <- fit_maturite(data = specimen, variable = "ltm", modele = "INT", lien = "probit")
res_int_probit_ltm$table_resultats
res_int_probit_ltm$graphique
res_int_probit_ltm$table_resultats_flextable

# INT - probit - age
res_int_probit_age <- fit_maturite(data = specimen, variable = "age", modele = "INT", lien = "probit")
res_int_probit_age$table_resultats
res_int_probit_age$graphique
res_int_probit_age$table_resultats_flextable

# Mortalite --------------------------------------------------------------

pp <- get_peak_plus(specimen)
age_max <- get_age_max(specimen)
df_age_corrigee <- prepare_age_data_corrigee(specimen,pp,age_max)
df_age_etendue <- prepare_age_data_etendue(df_corrigee = df_age_corrigee, age_max = age_max)

# result_poisson <- ajuster_modele_mortalite_poisson(df_age_etendue)
# # print(result_poisson)
# # 
# result_nb1 <- ajuster_modele_mortalite_nb1(df_age_etendue)
# # print(result_nb1)
# # 
# result_nb2 <- ajuster_modele_mortalite_nb2(df_age_etendue)
# # print(result_nb2)
# # 
# result_cmp <- ajuster_modele_mortalite_cmp(df_age_etendue)
# # print(result_cmp)
# # 
# result_gp <- ajuster_modele_mortalite_gp(df_age_etendue)
# # print(result_gp)

# 1. Exécuter la fonction
res_disp <- test_surdispersion_poisson(df_age_corrigee)

# 2. Afficher le message
cat(res_disp$message)

# 3. Visualiser le graphique (dans RStudio)
print(res_disp$plot)

# 4. Valeur de dispersion brute si besoin
res_disp$dispersion



# 3. Comparer les modèles de mortalité
mortalite_compare_modele_res <- mortalite_compare_modele(data = df_age_etendue)
print(mortalite_compare_modele_res$data)
print(mortalite_compare_modele_res$flextable)

meilleur_modele <- select_best_mortalite_model(mortalite_compare_modele_res$data)
modele <- get_best_mortalite_model(df_age_etendue, methode = meilleur_modele)
plot_mortalite_modele(specimen, modele, mortalite_compare_modele_res$data)

# Format data.frame
res_chaprob_df <- mortalite_chaprob(specimen = specimen, pp = pp, age_max = age_max, format = "data.frame")
print(res_chaprob_df)

# Format flextable (à afficher dans un R Markdown ou RStudio Viewer)
res_chaprob_ft <- mortalite_chaprob(specimen = specimen, pp = pp, age_max = age_max, format = "flextable")
res_chaprob_ft  # s'affiche bien dans un environnement interactif


# 5. Estimer la mortalité selon Chapman-Robson
df_chaprob <- mortalite_chaprob(df_corr, pp = pp, age_max = age_max)
print(df_chaprob)

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


# BPUE --------------------------------------------------------------------
# Créer le tableau résumé de biomasse et BPUE par groupe biologique
table_biomasse <- biomasse_table(
  data_specimen     = specimen,
  data_station = station_hasard_valide,
  format       = "data.frame" # ou "data.frame"
)
table_biomasse <- biomasse_table(
  data_specimen     = specimen,
  data_station = station_hasard_valide,
  format       = "flextable" # ou "data.frame"
)

# Afficher le résultat
table_biomasse

# CPUE --------------------------------------------------------------------

# Calcul des CPUE par station
df_cpue_tous <- prepare_cpue_data(capture = capture, specimen = specimen, group = "tous")
df_cpue_femelles <- prepare_cpue_data(capture = capture, specimen = specimen, group = "femelles")

# Comparaison des modèles CPUE
cpue_table_modele_tous <- cpue_modele_comparaison(df_cpue_tous, format = "data.frame")
cpue_table_modele_femelles <- cpue_modele_comparaison(df_cpue_femelles, format = "data.frame")

# Meilleur modèle
meilleur_modele_cpue_tous <- select_best_cpue_model(cpue_table_modele_tous)
meilleur_modele_cpue_femelles <- select_best_cpue_model(cpue_table_modele_femelles)

# Génération de la table d’abondance (avec CPUE intégrées)
abondance <- abondance_table(
  data = specimen,
  cpue_table_tous = cpue_table_modele_tous,
  cpue_table_femelles = cpue_table_modele_femelles,
  best_model_tous = meilleur_modele_cpue_tous,
  best_model_femelles = meilleur_modele_cpue_femelles,
  format = "flextable"
)
abondance  # affiche le tableau flextable

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

