# exemple_utilisation.R
# Rôle : Démonstration simple de l’utilisation des fonctions métier AquaPop

# Charger toutes les fonctions du package en développement
devtools::load_all()

# Téléchargement des données ----

path     <- "inst/extdata/Extract_IFA_AquaPop_2026-02-27.xlsx"
typ_pech <- "PENT"
no_lac   <- "01589"
annee    <- 2012

 # 01589, PENT 2012

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



# Croissance ----

## Tableau de sélection de modèles ----

table_modele_res <- croissance_compare_modele(specimen_tous) # Résultat brut (data.frame)
table_modele_res$data
table_modele_res$flextable

## Graphique du modèle choisi ----

modele_best <- croissance_select_best_modele(table_modele_res$data)
croissance_plot(specimen_tous, table_modele_res$data, modele_best)
croissance_plot(specimen_tous, table_modele_res$data, modele = "Von Bertalanffy")
croissance_plot(specimen_tous, table_modele_res$data, modele = "Gompertz")
croissance_plot(specimen_tous, table_modele_res$data, modele = "Logistique")


# CPUE - Abondance ----
## Tableau CPUE - Tous ----
df_cpue_tous <- cpue_prepare(capture, specimen_hasard_valide, group = "tous")
cpue_compare_modele_tous_res <- cpue_compare_modele(df_cpue_tous)
cpue_compare_modele_tous_res$data
cpue_compare_modele_tous_res$flextable
meilleur_modele_cpue_tous <- cpue_select_best_modele(cpue_compare_modele_tous_res$data)
meilleur_modele_cpue_tous
## Tableau CPUE - Femelles matures ----
df_cpue_femelles <- cpue_prepare(capture, specimen_hasard_valide, group = "femelles")
cpue_compare_modele_fem_res <- cpue_compare_modele(df_cpue_femelles)
meilleur_modele_cpue_femelles <- cpue_select_best_modele(cpue_compare_modele_fem_res$data)
meilleur_modele_cpue_femelles
## Tableau d’abondance ----
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

# Taille, masse, âge ----

taille_masse_age_res <- taille_masse_age(data = specimen_valide)
taille_masse_age_res$data
taille_masse_age_res$flextable

# Structure de taille ----
structure_taille_res <- structure_taille(specimen_valide, groupement = "tous")
structure_taille_res$data
structure_taille_res$plot
structure_taille_res$flextable
structure_taille_res <- structure_taille(specimen_valide, groupement = "marquage")
structure_taille_res$data
structure_taille_res$plot
structure_taille_res$flextable
structure_taille_res <- structure_taille(specimen_valide, groupement = "sexe")
structure_taille_res$data
structure_taille_res$plot
structure_taille_res$flextable
structure_taille_res <- structure_taille(specimen_valide, groupement = "maturite")
structure_taille_res$data
structure_taille_res$plot
structure_taille_res$flextable



# Structure d'âge ----

structure_age_res <- structure_age(specimen_valide, groupement = "tous")
structure_age_res$data
structure_age_res$plot
structure_age_res$flextable
structure_age_res <- structure_age(specimen_valide, groupement = "marquage")
structure_age_res$data
structure_age_res$plot
structure_age_res$flextable
structure_age_res <- structure_age(specimen_valide, groupement = "sexe")
structure_age_res$data
structure_age_res$plot
structure_age_res$flextable
structure_age_res <- structure_age(specimen_valide, groupement = "maturite")
structure_age_res$data
structure_age_res$plot
structure_age_res$flextable


# PSD ----
## Indice Q ----
psd_q_res <- psd_q(specimen_valide)
psd_q_res$data
psd_q_res$flextable
## Répartition par classe de taille – Tableau ----
psd_byclass_res <- psd_byclass(specimen_valide)
psd_byclass_res$data
psd_byclass_res$flextable
## Répartition par classe de taille – Graphique ----
psd_byclass_res$plot

# Relation masse-longueur ----

masse_longueur_fit_res <- masse_longueur_fit(data = specimen_tous)

print(masse_longueur_fit_res$plot)

print(masse_longueur_fit_res$data)
masse_longueur_fit_res$flextable
## Graphique ----

masse_longueur_fit_res$plot

## Tableau des coefficients ----

masse_longueur_fit_res$data # Résultat brut (data.frame)
masse_longueur_fit_res$flextable # Tableau formaté (flextable)

# Indice de condition ----

wri_res <- wri(data = specimen_tous)


## Tableau Wr ----

wri_res$data # Résultat brut (data.frame)
wri_res$flextable # Tableau formaté (flextable)

## Graphique Wr par sexe ----

wri_res$plot_tous

## Graphique Wr par classe de taille ----

wri_res$plot_byclass


# Mortalité ----
## Tableau de sélection de modèles ----
pp <- mortalite_get_peak_plus(specimen_valide)
age_max <- mortalite_get_age_max(specimen_valide)
df_age_corrigee <- mortalite_prepare_corr(specimen_valide, pp, age_max)
df_age_etendue <- mortalite_prepare_extended(df_age_corrigee, age_max)

res_disp <- mortalite_test_surdispersion_poisson(df_age_corrigee)
res_disp$message
res_disp$plot
res_disp$dispersion

mortalite_compare_modele_res <- mortalite_compare_modele(df_age_etendue)
print(mortalite_compare_modele_res$data)
print(mortalite_compare_modele_res$flextable)

meilleur_modele_nom <- mortalite_select_best_modele(mortalite_compare_modele_res$data)
meilleur_modele_nom

modele <- mortalite_fit_best_modele(df_age_etendue, methode = meilleur_modele_nom)

## Graphique du modèle choisi ----
mortalite_plot_modele(specimen_valide, modele, mortalite_compare_modele_res$data)

## Chapman-Robson ----
mortalite_chaprob_res <- mortalite_chaprob(specimen_valide, pp, age_max)
mortalite_chaprob_res$data        # Résultat brut (data.frame)
mortalite_chaprob_res$flextable   # Tableau formaté (flextable)


mortalite_phrase_resume(mortalite_compare_modele_res$data, meilleur_modele_nom)


# Maturité sexuelle ----
## Longueur à maturité ----

### Tableau de sélection de modèles ----

res <- maturite_compare_modele(specimen_tous, variable = "ltm")
res$table$df
res$table$flextable

res$best_model
res$message
res$table_sep$flextable
res$table_comb$flextable

### Tableau du modèle choisi ----

l50_modele <- maturite_generate_modele(
  data = specimen_tous,
  variable = "ltm",
  modele = res$best_model$modele,
  lien = res$best_model$lien
)

# 5. Afficher les résultats
print(l50_modele$table_resultats)
print(l50_modele$graphique)
print(l50_modele$table_resultats_flextable)


## Âge à maturité ----


res <- maturite_compare_modele(specimen_tous, variable = "age")
res$table$df
res$table$flextable

res$best_model
res$message
res$table_sep$flextable
res$table_comb$flextable

### Tableau du modèle choisi ----

a50_modele <- maturite_generate_modele(
  data = specimen_tous,
  variable = "age",
  modele = res$best_model$modele,
  lien = res$best_model$lien
)

# 5. Afficher les résultats
print(a50_modele$table_resultats)
print(a50_modele$graphique)
print(a50_modele$table_resultats_flextable)


