# exemple_utilisation.R
# Rôle : Démonstration simple de l'utilisation des fonctions métier AquaPop

# Charger toutes les fonctions du package en développement
devtools::load_all()

# Téléchargement des données ----

path     <- "inst/extdata/Extract_IFA_AquaPop_2026-02-27.xlsx"
typ_pech <- "PENDJ"
no_lac   <- "98430"
annee    <- 2017

# path     <- get0("path",     ifnotfound = "inst/extdata/Extract_IFA_AquaPop_2026-02-27.xlsx")
# typ_pech <- get0("typ_pech", ifnotfound = "PENOF")
# no_lac   <- get0("no_lac",   ifnotfound = "39404")
# annee    <- get0("annee",    ifnotfound = 1998)
 # 01589, PENT 2012
# 39834, PENOF 2020

df <- get_analysis_data(path, typ_pech, no_lac, annee)
data_station         <- df$data_station
station_valide      <- df$station_valide
station_hasard_valide <- df$station_hasard_valide
specimen_tous             <- df$specimen_tous
specimen_hasard_valide       <- df$specimen_hasard_valide
specimen_valide       <- df$specimen_valide
capture              <- df$capture

data_lac <- load_lac(path, namesheet = "Lac", verbose = TRUE) |>
  filter_by_pen_lac_annee(typ_pech, no_lac, annee)

generate_recapitulatif_inventaire(data_lac, data_station)

info_pen <- get_info_pen(typ_pech)

# Mortalité ----
## Tableau de sélection de modèles ----
pp_res <- mortalite_get_peak_plus(specimen_valide)
pp_res$message
pp <- pp_res$value

age_max_res <- mortalite_get_age_max(specimen_valide)
age_max_res$message
age_max <- age_max_res$value

df_age_corrigee_res <- mortalite_prepare_corr(specimen_valide, pp, age_max)
df_age_corrigee_res$message
df_age_corrigee <- df_age_corrigee_res$data

df_age_etendue_res <- mortalite_prepare_extended(df_age_corrigee, age_max)
df_age_etendue_res$message
df_age_etendue <- df_age_etendue_res$data

res_disp <- mortalite_test_surdispersion_poisson(df_age_corrigee)
res_disp$message
res_disp$plot
res_disp$dispersion

mortalite_compare_modele_res <- mortalite_compare_modele(df_age_etendue)
mortalite_compare_modele_res$success
mortalite_compare_modele_res$message
mortalite_compare_modele_res$data
mortalite_compare_modele_res$flextable


meilleur_modele_nom <- mortalite_select_best_modele(mortalite_compare_modele_res$data)
meilleur_modele_nom

modele <- mortalite_fit_best_modele(df_age_etendue, methode = meilleur_modele_nom)
modele
## Graphique du modèle choisi ----
mortalite_plot_modele(specimen_valide, modele, mortalite_compare_modele_res$data)

## Chapman-Robson ----
mortalite_chaprob_res <- mortalite_chaprob(specimen_valide, pp, age_max)
mortalite_chaprob_res$message
mortalite_chaprob_res$data
mortalite_chaprob_res$flextable

mortalite_phrase_resume(mortalite_compare_modele_res$data, meilleur_modele_nom)





# CPUE - Abondance ----
## Tableau CPUE - Tous ----

# CPUE - Abondance ----

## Validation Récolte vs Spécimens ----
validation_capture_specimen_res <- cpue_compare_capture_specimen(capture,specimen_hasard_valide)

validation_capture_specimen_res$message
validation_capture_specimen_res$data
validation_capture_specimen_res$flextable

df_cpue_tous <- cpue_prepare(capture, specimen_hasard_valide, group = "tous")
cpue_compare_modele_tous_res <- cpue_compare_modele(df_cpue_tous)
cpue_compare_modele_tous_res$data
cpue_compare_modele_tous_res$flextable
meilleur_modele_cpue_tous <- cpue_select_best_modele(cpue_compare_modele_tous_res$data)
meilleur_modele_cpue_tous
## Tableau CPUE - Femelles matures ----
df_cpue_femelles <- cpue_prepare(capture, specimen_hasard_valide, group = "femelles")
cpue_compare_modele_fem_res <- cpue_compare_modele(df_cpue_femelles)
cpue_compare_modele_fem_res$data
cpue_compare_modele_fem_res$flextable
meilleur_modele_cpue_femelles <- cpue_select_best_modele(cpue_compare_modele_fem_res$data)
meilleur_modele_cpue_femelles
## Tableau d'abondance ----
abondance <- cpue_abondance_table(
  specimen_hasard_valide,
  cpue_compare_modele_tous_res$data,
  cpue_compare_modele_fem_res$data,
  meilleur_modele_cpue_tous,
  meilleur_modele_cpue_femelles
)
abondance$flextable
abondance$data

# BPUE - Biomasse ----

table_biomasse <- bpue_generate_biomasse(specimen_hasard_valide, station_hasard_valide)
table_biomasse$data
table_biomasse$flextable



# Croissance ----

## Tableau de sélection de modèles ----

table_modele_res <- croissance_compare_modele(specimen_tous) # Résultat brut (data.frame)
table_modele_res$data
table_modele_res$flextable
table_modele_res$success
table_modele_res$message

## Graphique du modèle choisi ----

modele_best <- croissance_select_best_modele(table_modele_res$data)
modele_best
croissance_plot(specimen_tous, table_modele_res$data, modele_best)
croissance_plot(specimen_tous, table_modele_res$data, modele = "Von Bertalanffy")
croissance_plot(specimen_tous, table_modele_res$data, modele = "Gompertz")
croissance_plot(specimen_tous, table_modele_res$data, modele = "Logistique")



# Taille, masse, âge ----

taille_masse_age_res <- taille_masse_age(data = specimen_valide)
taille_masse_age_res$data
taille_masse_age_res$flextable
taille_masse_age_res$message

# Structure de taille ----
structure_taille_res <- structure_taille(specimen_valide, groupement = "tous")
structure_taille_res$data
structure_taille_res$plot
structure_taille_res$success
structure_taille_res$message
structure_taille_res$flextable
structure_taille_res <- structure_taille(specimen_valide, groupement = "marquage")
structure_taille_res$data
structure_taille_res$plot
structure_taille_res$success
structure_taille_res$message
structure_taille_res$flextable
structure_taille_res <- structure_taille(specimen_valide, groupement = "sexe")
structure_taille_res$data
structure_taille_res$plot
structure_taille_res$success
structure_taille_res$message
structure_taille_res$flextable
structure_taille_res <- structure_taille(specimen_valide, groupement = "maturite")
structure_taille_res$data
structure_taille_res$plot
structure_taille_res$success
structure_taille_res$message
structure_taille_res$flextable



# Structure d'âge ----

structure_age_res <- structure_age(specimen_valide, groupement = "tous")
structure_age_res$data
structure_age_res$plot
structure_age_res$success
structure_age_res$message
structure_age_res$flextable
structure_age_res <- structure_age(specimen_valide, groupement = "marquage")
structure_age_res$data
structure_age_res$plot
structure_age_res$success
structure_age_res$message
structure_age_res$flextable
structure_age_res <- structure_age(specimen_valide, groupement = "sexe")
structure_age_res$data
structure_age_res$plot
structure_age_res$success
structure_age_res$message
structure_age_res$flextable
structure_age_res <- structure_age(specimen_valide, groupement = "maturite")
structure_age_res$data
structure_age_res$plot
structure_age_res$success
structure_age_res$message
structure_age_res$flextable


# PSD ----
## Indice Q ----
psd_q_res <- psd_q(specimen_valide)
psd_q_res$data
psd_q_res$flextable
psd_q_res$success
psd_q_res$message
## Répartition par classe de taille – Tableau ----
psd_byclass_res <- psd_byclass(specimen_valide)
psd_byclass_res$data
psd_byclass_res$flextable
psd_byclass_res$success
psd_byclass_res$message
## Répartition par classe de taille – Graphique ----
psd_byclass_res$plot

# Relation masse-longueur ----

masse_longueur_fit_res <- masse_longueur_fit(data = specimen_tous)
masse_longueur_fit_res$plot
masse_longueur_fit_res$success
masse_longueur_fit_res$data
masse_longueur_fit_res$flextable
masse_longueur_fit_res$message

# Indice de condition ----

wri_res <- wri(data = specimen_tous)
wri_res$data # Résultat brut (data.frame)
wri_res$flextable # Tableau formaté (flextable)
wri_res$success
wri_res$message
wri_res$plot_tous #Graphique Wr par sexe
wri_res$plot_byclass #Graphique Wr par classe de taille


# Maturité sexuelle ----
## Longueur à maturité ----

# Tableau de sélection de modèles ----
res_l50 <- maturite_compare_modele(specimen_tous, variable = "ltm")

# Vérification globale de la sortie
res_l50$success
res_l50$message
str(res_l50$best_model)

# Tableaux de comparaison
res_l50$table$df
str(res_l50$table$df)
res_l50$table_sep$df
res_l50$table_comb$df

# Versions flextable
res_l50$table$flextable
res_l50$table_sep$flextable
res_l50$table_comb$flextable

# Détail des meilleurs modèles retenus
res_l50$best_model$best_model_F
res_l50$best_model$best_model_M
res_l50$best_model$best_model_combined

# Modèle femelles ----
l50_modele_f <- maturite_generate_modele(
  data = specimen_tous,
  variable = res_l50$best_model$best_model_F$variable,
  modele = res_l50$best_model$best_model_F$modele,
  lien = res_l50$best_model$best_model_F$lien
)

l50_modele_f$success
l50_modele_f$message
l50_modele_f$commentaire
l50_modele_f$table_resultats
l50_modele_f$table_resultats_flextable
l50_modele_f$graphique
l50_modele_f$donnees_ogive

# Modèle mâles ----
l50_modele_m <- maturite_generate_modele(
  data = specimen_tous,
  variable = res_l50$best_model$best_model_M$variable,
  modele = res_l50$best_model$best_model_M$modele,
  lien = res_l50$best_model$best_model_M$lien
)

l50_modele_m$success
l50_modele_m$message
l50_modele_m$commentaire
l50_modele_m$table_resultats
l50_modele_m$table_resultats_flextable
l50_modele_m$graphique
l50_modele_m$donnees_ogive

# Modèle combiné ----
l50_modele_comb <- maturite_generate_modele(
  data = specimen_tous,
  variable = res_l50$best_model$best_model_combined$variable,
  modele = res_l50$best_model$best_model_combined$modele,
  lien = res_l50$best_model$best_model_combined$lien
)

l50_modele_comb$success
l50_modele_comb$message
l50_modele_comb$commentaire
l50_modele_comb$table_resultats
l50_modele_comb$table_resultats_flextable
l50_modele_comb$graphique
l50_modele_comb$donnees_ogive

## Âge à maturité ----

# Tableau de sélection de modèles ----
res_a50 <- maturite_compare_modele(specimen_tous, variable = "age")

# Vérification globale de la sortie
res_a50$success
res_a50$message
str(res_a50$best_model)

# Tableaux de comparaison
res_a50$table$df
res_a50$table_sep$df
res_a50$table_comb$df

# Versions flextable
res_a50$table$flextable
res_a50$table_sep$flextable
res_a50$table_comb$flextable

# Détail des meilleurs modèles retenus
res_a50$best_model$best_model_F
res_a50$best_model$best_model_M
res_a50$best_model$best_model_combined

# Modèle femelles ----
a50_modele_f <- maturite_generate_modele(
  data = specimen_tous,
  variable = res_a50$best_model$best_model_F$variable,
  modele = res_a50$best_model$best_model_F$modele,
  lien = res_a50$best_model$best_model_F$lien
)

a50_modele_f$success
a50_modele_f$message
a50_modele_f$commentaire
a50_modele_f$table_resultats
a50_modele_f$table_resultats_flextable
a50_modele_f$graphique
a50_modele_f$donnees_ogive

# Modèle mâles ----
a50_modele_m <- maturite_generate_modele(
  data = specimen_tous,
  variable = res_a50$best_model$best_model_M$variable,
  modele = res_a50$best_model$best_model_M$modele,
  lien = res_a50$best_model$best_model_M$lien
)

a50_modele_m$success
a50_modele_m$message
a50_modele_m$commentaire
a50_modele_m$table_resultats
a50_modele_m$table_resultats_flextable
a50_modele_m$graphique
a50_modele_m$donnees_ogive

# Modèle combiné ----
a50_modele_comb <- maturite_generate_modele(
  data = specimen_tous,
  variable = res_a50$best_model$best_model_combined$variable,
  modele = res_a50$best_model$best_model_combined$modele,
  lien = res_a50$best_model$best_model_combined$lien
)

a50_modele_comb$success
a50_modele_comb$message
a50_modele_comb$commentaire
a50_modele_comb$table_resultats
a50_modele_comb$table_resultats_flextable
a50_modele_comb$graphique
a50_modele_comb$donnees_ogive





