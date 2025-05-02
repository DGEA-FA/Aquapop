# exemple_utilisation.R
# Rôle : Démonstration simple de l’utilisation des fonctions métier AquaPop

# Charger les dépendances et les fonctions du package aquapop ----

# Charger les packages déclarés 
# source("R/load_packages.R")

# Charger toutes les fonctions du package en développement
devtools::load_all()

# Téléchargement des données ----

path     <- "data/Extract IFA_R04_AquaPop.xlsx"
typ_pech <- "PENT"
no_lac   <- "01565"
annee    <- 2008

df <- get_analysis_data(path, typ_pech, no_lac, annee)
data_station         <- df$data_station
station_valides      <- df$station_valides
station_hasard_valide <- df$station_hasard_valide
specimen             <- df$specimen
specimen_valid       <- df$specimen_valid
capture              <- df$capture

data_lac <- load_lac(path, namesheet = "Lac", verbose = TRUE) |>
  filter_by_pen_lac_annee(typ_pech, no_lac, annee)

generate_recapitulatif_inventaire(data_lac, data_station)

info_pen <- get_info_pen(typ_pech)


# CPUE - Abondance ----
## Tableau CPUE - Tous ----
df_cpue_tous <- cpue_prepare(capture, specimen, group = "tous")
cpue_compare_modele_tous_res <- cpue_compare_modele(df_cpue_tous)
meilleur_modele_cpue_tous <- cpue_select_best_modele(cpue_compare_modele_tous_res$data)

## Tableau CPUE - Femelles matures ----
df_cpue_femelles <- cpue_prepare(capture, specimen, group = "femelles")
cpue_compare_modele_fem_res <- cpue_compare_modele(df_cpue_femelles)
meilleur_modele_cpue_femelles <- cpue_select_best_modele(cpue_compare_modele_fem_res$data)

## Tableau d’abondance ----
abondance <- cpue_abondance_table(
  data = specimen,
  cpue_table_tous = cpue_compare_modele_tous_res$data,
  cpue_table_femelles = cpue_compare_modele_fem_res$data,
  best_model_tous = meilleur_modele_cpue_tous,
  best_model_femelles = meilleur_modele_cpue_femelles
)
abondance$flextable

# BPUE - Biomasse ----

table_biomasse <- bpue_generate_biomasse(specimen, station_hasard_valide)
table_biomasse$data
table_biomasse$flextable

# Taille, masse, âge ----

taille_masse_age_res <- taille_masse_age(data = specimen_valid)
taille_masse_age_res$data
taille_masse_age_res$flextable

## Structure de taille ----
## Structure d'âge ----
## PSD ----
### Indice Q ----
psd_q_res <- psd_q(specimen_valid)
psd_q_res$data
psd_q_res$flextable
### Répartition par classe de taille – Tableau ----
psd_byclass_res <- psd_byclass(specimen_valid)
psd_byclass_res$data
psd_byclass_res$flextable
### Répartition par classe de taille – Graphique ----
psd_byclass_res$plot

# Relation masse-longueur ----

masse_longueur_fit_res <- masse_longueur_fit(data = specimen)

## Graphique ----

masse_longueur_fit_res$plot

## Tableau des coefficients ----

masse_longueur_fit_res$data # Résultat brut (data.frame)
masse_longueur_fit_res$flextable # Tableau formaté (flextable)

# Indice de condition ----

wri_res <- wri(data = specimen_valid)


## Tableau Wr ----

wri_res$data # Résultat brut (data.frame)
wri_res$flextable # Tableau formaté (flextable)

## Graphique Wr par sexe ----

wri_res$plot_tous

## Graphique Wr par classe de taille ----

wri_res$plot_byclass

# Croissance ----

## Tableau de sélection de modèles ----

table_modele <- croissance_compare_modele(specimen) # Résultat brut (data.frame)
table_modele$data
table_modele$flextable

## Graphique du modèle choisi ----

modele_best <- croissance_select_best_modele(table_modele$data)
croissance_plot(specimen, table_modele$data, modele_best)
croissance_plot(specimen, table_modele$data, modele = "Von Bertalanffy")
croissance_plot(specimen, table_modele$data, modele = "Gompertz")
croissance_plot(specimen, table_modele$data, modele = "Logistique")

# Mortalité ----
## Tableau de sélection de modèles ----
pp <- mortalite_get_peak_plus(specimen)
age_max <- mortalite_get_age_max(specimen)
df_age_corrigee <- mortalite_prepare_corr(specimen, pp, age_max)
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
mortalite_plot_modele(specimen, modele, mortalite_compare_modele_res$data)

## Chapman-Robson ----
mortalite_chaprob_res <- mortalite_chaprob(specimen, pp, age_max)
mortalite_chaprob_res$data        # Résultat brut (data.frame)
mortalite_chaprob_res$flextable   # Tableau formaté (flextable)


mortalite_phrase_resume(mortalite_compare_modele_res$data, meilleur_modele_nom)


# Maturité sexuelle ----
## Longueur à maturité ----

### Tableau de sélection de modèles ----

res <- maturite_compare_modele(specimen, variable = "ltm")
res$table$df
res$table$flextable

res$best_model
res$message
res$table_sep$flextable
res$table_comb$flextable

### Tableau du modèle choisi ----
### Graphique du modèle choisi ----




## Âge à maturité ----
### Tableau de sélection de modèles ----
### Tableau du modèle choisi ----
### Graphique du modèle choisi ----
