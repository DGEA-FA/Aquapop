# Script : exemple_utilisation.R
# Rôle   : Démonstration simple de l’utilisation des fonctions métier AquaPop
#          hors de l'application Shiny


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
  filter_by_pen_lac_annee(typ_pech = typ_pech,
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
res <- maturite_compare_modele(specimen, variable = "ltm")
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
x <- maturite_generate_modele(
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
res_tlo_probit_ltm <- maturite_generate_modele(data = specimen, variable = "ltm", modele = "TLO", lien = "probit")
res_tlo_probit_ltm$table_resultats
res_tlo_probit_ltm$graphique
res_tlo_probit_ltm$table_resultats_flextable

# TLO - probit - age
res_tlo_probit_age <- maturite_generate_modele(data = specimen, variable = "age", modele = "TLO", lien = "probit")
res_tlo_probit_age$table_resultats
res_tlo_probit_age$graphique
res_tlo_probit_age$table_resultats_flextable

# TLO - logit - ltm
res_tlo_logit_ltm <- maturite_generate_modele(data = specimen, variable = "ltm", modele = "TLO", lien = "logit")
res_tlo_logit_ltm$table_resultats
res_tlo_logit_ltm$graphique
res_tlo_logit_ltm$table_resultats_flextable

# TLO - logit - age
res_tlo_logit_age <- maturite_generate_modele(data = specimen, variable = "age", modele = "TLO", lien = "logit")
res_tlo_logit_age$table_resultats
res_tlo_logit_age$graphique
res_tlo_logit_age$table_resultats_flextable

# TLO - cloglog - ltm
res_tlo_cloglog_ltm <- maturite_generate_modele(data = specimen, variable = "ltm", modele = "TLO", lien = "cloglog")
res_tlo_cloglog_ltm$table_resultats
res_tlo_cloglog_ltm$graphique
res_tlo_cloglog_ltm$table_resultats_flextable

# TLO - cloglog - age
res_tlo_cloglog_age <- maturite_generate_modele(data = specimen, variable = "age", modele = "TLO", lien = "cloglog")
res_tlo_cloglog_age$table_resultats
res_tlo_cloglog_age$graphique
res_tlo_cloglog_age$table_resultats_flextable


# ADD - logit - ltm
res_add_logit_ltm <- maturite_generate_modele(data = specimen, variable = "ltm", modele = "ADD", lien = "logit")
res_add_logit_ltm$table_resultats
res_add_logit_ltm$graphique
res_add_logit_ltm$table_resultats_flextable

# ADD - logit - age
res_add_logit_age <- maturite_generate_modele(data = specimen, variable = "age", modele = "ADD", lien = "logit")
res_add_logit_age$table_resultats
res_add_logit_age$graphique
res_add_logit_age$table_resultats_flextable

# ADD - cloglog - ltm
res_add_cloglog_ltm <- maturite_generate_modele(data = specimen, variable = "ltm", modele = "ADD", lien = "cloglog")
res_add_cloglog_ltm$table_resultats
res_add_cloglog_ltm$graphique
res_add_cloglog_ltm$table_resultats_flextable

# ADD - cloglog - age
res_add_cloglog_age <- maturite_generate_modele(data = specimen, variable = "age", modele = "ADD", lien = "cloglog")
res_add_cloglog_age$table_resultats
res_add_cloglog_age$graphique
res_add_cloglog_age$table_resultats_flextable

# COM - logit - ltm
res_com_logit_ltm <- maturite_generate_modele(data = specimen, variable = "ltm", modele = "COM", lien = "logit")
res_com_logit_ltm$table_resultats
res_com_logit_ltm$graphique
res_com_logit_ltm$table_resultats_flextable



# COM - logit - age
res_com_logit_age <- maturite_generate_modele(data = specimen, variable = "age", modele = "COM", lien = "logit")
res_com_logit_age$table_resultats
res_com_logit_age$graphique
res_com_logit_age$table_resultats_flextable

# COM - cloglog - ltm
res_com_cloglog_ltm <- maturite_generate_modele(data = specimen, variable = "ltm", modele = "COM", lien = "cloglog")
res_com_cloglog_ltm$table_resultats
res_com_cloglog_ltm$graphique
res_com_cloglog_ltm$table_resultats_flextable

# COM - cloglog - age
res_com_cloglog_age <- maturite_generate_modele(data = specimen, variable = "age", modele = "COM", lien = "cloglog")
res_com_cloglog_age$table_resultats
res_com_cloglog_age$graphique
res_com_cloglog_age$table_resultats_flextable

# INT - logit - ltm
res_int_logit_ltm <- maturite_generate_modele(data = specimen, variable = "ltm", modele = "INT", lien = "logit")
res_int_logit_ltm$table_resultats
res_int_logit_ltm$graphique
res_int_logit_ltm$table_resultats_flextable

# INT - logit - age
res_int_logit_age <- maturite_generate_modele(data = specimen, variable = "age", modele = "INT", lien = "logit")
res_int_logit_age$table_resultats
res_int_logit_age$graphique
res_int_logit_age$table_resultats_flextable

# INT - cloglog - ltm
res_int_cloglog_ltm <- maturite_generate_modele(data = specimen, variable = "ltm", modele = "INT", lien = "cloglog")
res_int_cloglog_ltm$table_resultats
res_int_cloglog_ltm$graphique
res_int_cloglog_ltm$table_resultats_flextable

# INT - cloglog - age
res_int_cloglog_age <- maturite_generate_modele(data = specimen, variable = "age", modele = "INT", lien = "cloglog")
res_int_cloglog_age$table_resultats
res_int_cloglog_age$graphique
res_int_cloglog_age$table_resultats_flextable

# ADD - probit - ltm
res_add_probit_ltm <- maturite_generate_modele(data = specimen, variable = "ltm", modele = "ADD", lien = "probit")
res_add_probit_ltm$table_resultats
res_add_probit_ltm$graphique
res_add_probit_ltm$table_resultats_flextable

# ADD - probit - age
res_add_probit_age <- maturite_generate_modele(data = specimen, variable = "age", modele = "ADD", lien = "probit")
res_add_probit_age$table_resultats
res_add_probit_age$graphique
res_add_probit_age$table_resultats_flextable

# COM - probit - ltm
res_com_probit_ltm <- maturite_generate_modele(data = specimen, variable = "ltm", modele = "COM", lien = "probit")
res_com_probit_ltm$table_resultats
res_com_probit_ltm$graphique
res_com_probit_ltm$table_resultats_flextable

# COM - probit - age
res_com_probit_age <- maturite_generate_modele(data = specimen, variable = "age", modele = "COM", lien = "probit")
res_com_probit_age$table_resultats
res_com_probit_age$graphique
res_com_probit_age$table_resultats_flextable

# INT - probit - ltm
res_int_probit_ltm <- maturite_generate_modele(data = specimen, variable = "ltm", modele = "INT", lien = "probit")
res_int_probit_ltm$table_resultats
res_int_probit_ltm$graphique
res_int_probit_ltm$table_resultats_flextable

# INT - probit - age
res_int_probit_age <- maturite_generate_modele(data = specimen, variable = "age", modele = "INT", lien = "probit")
res_int_probit_age$table_resultats
res_int_probit_age$graphique
res_int_probit_age$table_resultats_flextable

# Mortalite --------------------------------------------------------------

pp <- mortalite_get_peak_plus(data = specimen)
mortalite_get_age_max_res <- mortalite_get_age_max(data = specimen)
df_age_corrigee <- mortalite_prepare_corr(specimen,pp,mortalite_get_age_max_res)
df_age_etendue <- mortalite_prepare_extended(df_corrigee = df_age_corrigee, age_max = mortalite_get_age_max_res)

res_disp <- mortalite_test_surdispersion_poisson(df_age_corrigee)

cat(res_disp$message)

print(res_disp$plot)

res_disp$dispersion



# Comparer les modèles de mortalité
mortalite_compare_modele_res <- mortalite_compare_modele(data = df_age_etendue)
print(mortalite_compare_modele_res$data)
print(mortalite_compare_modele_res$flextable)

meilleur_modele_nom <- mortalite_select_best_modele(mortalite_compare_modele_res$data)
meilleur_modele_nom
modele <- mortalite_fit_best_modele(df_age_etendue, methode = meilleur_modele_nom)
mortalite_plot_modele(specimen, modele, mortalite_compare_modele_res$data)

# Estimer la mortalité selon Chapman-Robson
mortalite_chaprob_res <- mortalite_chaprob(specimen = specimen, pp = pp, age_max = mortalite_get_age_max_res)
mortalite_chaprob_res$data        # Résultat brut (data.frame)
mortalite_chaprob_res$flextable   # Tableau formaté (flextable)


# Croissance --------------------------------------------------------------
# 2. Ajuster les modèles et créer la table de comparaison
table_modele <- croissance_compare_modele(
  data = specimen,
  format = "data.frame"
)


# Afficher la table dans la console
print(table_modele)


# 3. Sélectionner le meilleur modèle automatiquement

modele_best <- croissance_select_best_modele(table_modele)
cat("Meilleur modèle sélectionné :", modele_best, "\n")

# 4. Générer le graphique du modèle choisi
p <- croissance_plot(
  dfspecimen = specimen,
  tablemodele = table_modele,
  modele = modele_best
)

# Afficher le graphique
print(p)


# BPUE --------------------------------------------------------------------
# Créer le tableau résumé de biomasse et BPUE par groupe biologique
table_biomasse <- bpue_generate_biomasse(
  data_specimen     = specimen,
  data_station = station_hasard_valide)
# Afficher le résultat
table_biomasse$data
table_biomasse$flextable

# CPUE --------------------------------------------------------------------

# Calcul des CPUE par station
df_cpue_tous <- cpue_prepare(capture = capture, specimen = specimen, group = "tous")
df_cpue_femelles <- cpue_prepare(capture = capture, specimen = specimen, group = "femelles")

# Comparaison des modèles CPUE
cpue_compare_modele_tous_res <- cpue_compare_modele(df_cpue_tous)
cpue_compare_modele_fem_res <- cpue_compare_modele(df_cpue_femelles)

# Meilleur modèle
meilleur_modele_cpue_tous <- cpue_select_best_modele(cpue_compare_modele_tous_res$data)
meilleur_modele_cpue_femelles <- cpue_select_best_modele(cpue_compare_modele_fem_res$data)

# Génération de la table d’abondance (avec CPUE intégrées)
abondance <- cpue_abondance_table(
  data = specimen,
  cpue_table_tous = cpue_compare_modele_tous_res$data,
  cpue_table_femelles = cpue_compare_modele_fem_res$data,
  best_model_tous = meilleur_modele_cpue_tous,
  best_model_femelles = meilleur_modele_cpue_femelles
)
abondance$flextable  # affiche le tableau flextable

# TAILLE MASSE ÂGE  -------------------------------------------------------

# Pour obtenir le tableau de données
taille_masse_age_res <- taille_masse_age(data = specimen_valid)
taille_masse_age_res$data
taille_masse_age_res$flextable

# PSD ---------------------------------------------------------------------

psd_q_res <- psd_q(data = specimen_valid)
psd_q_res$data
psd_q_res$flextable


psd_byclass_res <- psd_byclass(data = specimen_valid)
psd_byclass_res$data
psd_byclass_res$flextable
psd_byclass_res$plot

# Relation masse-longueur ---------------------------------------------------------------------
masse_longueur_fit_res <- masse_longueur_fit(data = specimen)
masse_longueur_fit_res$data
masse_longueur_fit_res$plot
masse_longueur_fit_res$flextable
# Structure de taille ---------------------------------------------------------------------
structure_taille_res <- structure_taille(data = specimen, groupement = "maturite")

structure_taille_res$plot       # <- ggplot
structure_taille_res$data       # <- data.frame
structure_taille_res$flextable  # <- flextable


# Structure d'âge ---------------------------------------------------------------------
structure_age_res <- structure_age(data = specimen, groupement = "maturite")

structure_age_res$plot       # <- ggplot
structure_age_res$data       # <- data.frame
structure_age_res$flextable  # <- flextable

# Indice de condition -----------------------------------------------------

# Exemple : Indice de condition (Wr)

# Format tableau (data.frame ou flextable)
indice_condition(data = specimen, format = "data.frame")
indice_condition(data = specimen, format = "flextable")

# Format graphique (Wr par sexe ou par classe de taille)
indice_condition(data = specimen, format = "plot_tous")
indice_condition(data = specimen, format = "plot_byclass")

