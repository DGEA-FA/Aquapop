# Lister tous les fichiers R dans le dossier R/
script_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)

# Charger chaque script dans l'environnement global
invisible(lapply(script_files, function(f) {
  tryCatch(
    source(f, local = FALSE),
    error = function(e) message("Erreur dans ", f, ": ", e$message)
  )
}))

# Tous les scripts sont maintenant chargés et prêts à être utilisés

path <- "data/Extract IFA_R04_AquaPop.xlsx"
typ_pech <- "PENT"
no_lac <- "00024" 
annee <- 2015


df <- get_analysis_data(
  path     = path,
  typ_pech = typ_pechvar,
  no_lac   = no_lac_var,
  annee    = annee_var
)

resultats$capture
resultats$specimen_valid

data_lac <- load_lac(path, namesheet= "Lac")

# Si besoin de reload une fonction que tu viens de modifier
# source("R/load_station.R")

# Load datasets et Filter data based on specified variables
data_station <-  load_station(path, namesheet= "Stations") %>% filter(typ_pech == typ_pechvar & no_lac == no_lac_var & annee == annee_var)
data_recolte <- load_recolte(path, namesheet= "Recolte") %>% filter(typ_pech == typ_pechvar & no_lac == no_lac_var & annee == annee_var)
data_specimen <- load_specimen(path, namesheet= "Specimens") %>% filter(typ_pech == typ_pechvar & no_lac == no_lac_var & annee == annee_var)

# Create derived variables
sp_pen <- create_sp_pen(input_typ_pech = typ_pechvar)
specimen <- create_specimen(data_specimen, data_station)
specimen_valid <- create_specimen_valid(data_specimen, data_station)

capture <- create_capture(data_station, data_recolte)

df_maturiteltm <- create_df_maturiteltm(specimen,sp_pen )









# Test abundance and biomass tables
abondance_table <- abondance_table(specimen, sp_pen) %>% as.data.frame()

biomasse_table <- biomasse_table(specimen= specimen, sp_pen, data_station) %>% as.data.frame()
biomasse_table







# Étape 1 : Ajuster les modèles
L50_models <- fit_L50_models(df_maturiteltm)

# Étape 2 : Évaluer les modèles
L50_evaluation <- evaluate_L50_models(L50_models)
# Afficher les résultats
L50_evaluation %>% afficher_avec_labels()

# Étape 3 : Sélectionner les meilleurs modèles
best_L50 <- select_best_L50_models(L50_evaluation)
print(best_L50)

# Étape 4 : Ajuster les modèles combinés (sexes ensemble)
L50_combined_models <- fit_L50_combined_models(df_maturiteltm)

# Étape 5 : Évaluer les modèles combinés
L50_combined_evaluation <- evaluate_L50_models(L50_combined_models)
# Afficher les résultats
L50_combined_evaluation %>% afficher_avec_labels()

best_combined_L50 <- select_best_L50_combined_model(L50_combined_evaluation)
print(best_combined_L50)

modele_male <- get_best_L50_model(best_L50, sexe = "M")
modele_femelle <- get_best_L50_model(best_L50, sexe = "F")

# Exemple d'utilisation :
resultat_M <- process_L50_model(modele_id = "M_cloglog", data = df_maturiteltm, liste_modeles= L50_models)
resultat_F <- process_L50_model("F_probit", data= df_maturiteltm, liste_modeles = L50_models)

# Afficher les résultats
print(resultat_M$minitable)
print(resultat_F$minitable)
print(resultat_M$plot)
str(resultat_M$DATAogive)
















# Use the functions to define binwidth and nomsp
binwidth <- get_binwidth(sp_pen)
nomsp <- get_nomsp(sp_pen)

deathdf <- death(data = specimen, espece = sp_pen)

pp <- peakplus(data = deathdf)
newPP <- pp #on le change ici si on veut autre chose

agemax_val <- agemax(data = deathdf)

df_corr <- creation_df_CORR(data = deathdf,
                            peakplus = newPP,
                            agemax = agemax_val) %>% as.data.frame()

df_ext <- creation_df_EXT(data = df_corr,
                          peakplus = newPP,
                          agemax = agemax_val) %>% as.data.frame()



df_EXT = df_ext
# mortalite1 <- mortalite_selection_modeles(df_EXT = df_ext) #ca fini pu


mortalite2 <- mortalite_chaprob(data = deathdf,
                                pp = newPP,
                                agemax_val = agemax_val) %>% as.data.frame() %>% gt_mortalite2()



















croissance1 <- courbe_croissance_comparaison(dfspecimen= data_specimen, sp_pen)



selectedmodelcroissanceplot_vb <- courbe_croissance_plot(dfspecimen = data_specimen, 
                                                      sp_pen,
                                                      tablemodele = croissance1,
                                                      modele = "Von Bertalanffy")
selectedmodelcroissanceplot_vb

selectedmodelcroissanceplot_g <- courbe_croissance_plot(dfspecimen = data_specimen, 
                                                         sp_pen,
                                                         tablemodele = croissance1,
                                                         modele = "Gompertz")
selectedmodelcroissanceplot_g


selectedmodelcroissanceplot_l <- courbe_croissance_plot(dfspecimen = data_specimen, 
                                                        sp_pen,
                                                        tablemodele = croissance1,
                                                        modele = "Logistique")
selectedmodelcroissanceplot_l





selection_modele_CPUE_tous_data <- selection_modele_CPUE(capture, specimen, espece = sp_pen, station = data_station)
selection_modele_CPUE_tous_data




# Test with 'tous' groupement
plot_tous <- structure_taille(dfspecimen = specimen, espece = sp_pen, binwidth = binwidth, nomsp = nomsp, groupement = "tous")
print(plot_tous)

# Test with 'sexe' groupement
plot_sexe <- structure_taille(dfspecimen = specimen, espece = sp_pen, binwidth = binwidth, nomsp = nomsp, groupement = "sexe")
print(plot_sexe)

# Test with 'maturite' groupement
plot_maturite <- structure_taille(dfspecimen = specimen, espece = sp_pen, binwidth = binwidth, nomsp = nomsp, groupement = "maturite")
print(plot_maturite)

# Test with 'marquage' groupement
plot_marquage <- structure_taille(dfspecimen = specimen, espece = sp_pen, binwidth = binwidth, nomsp = nomsp, groupement = "marquage")
print(plot_marquage)


# Test avec 'tous' (aucun groupement)
plot_age_tous <- structure_age(dfspecimen = specimen, espece = sp_pen, nomsp = nomsp, groupement = "tous")
print(plot_age_tous)

# Test avec 'sexe' comme groupement
plot_age_sexe <- structure_age(dfspecimen = specimen, espece = sp_pen, nomsp = nomsp, groupement = "sexe")
print(plot_age_sexe)

# Test avec 'maturite' comme groupement
plot_age_maturite <- structure_age(dfspecimen = specimen, espece = sp_pen, nomsp = nomsp, groupement = "maturite")
print(plot_age_maturite)

# Test avec 'marquage' comme groupement
plot_age_marquage <- structure_age(dfspecimen = specimen, espece = sp_pen, nomsp = nomsp, groupement = "marquage")
print(plot_age_marquage)





taille_masse_agedf <- taille_masse_age(dataspecimen = specimen, espece = sp_pen)


relation_masse_longueur(data = data_specimen, espece = sp_pen)$graph %>%
  plotly::ggplotly(tooltip = "text")



psd_indice(data = specimen_valid, sp = sp_pen) %>% as.data.frame()


psd_byclass(data = specimen_valid, espece = sp_pen) %>% as.data.frame()

table_wri(data = data_specimen, espece = sp_pen)

fig_wri_tous(data = data_specimen, espece = sp_pen)

fig_wri_byclass(data = data_specimen, espece = sp_pen)


psd1 <- psd_indice(data = specimen, sp = sp_pen)
