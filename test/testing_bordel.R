library(shiny)
library(car)
library(DT)
library(kableExtra)
library(reactable)
library(FSA)
library(nlstools)
library(shinyBS)
library(gghighlight)
library(htmltools)
library(markdown)
library(readxl)
library(ggplot2)
library(scales)
library(dplyr)
library(patchwork)
library(reactlog)
library(stringr)
library(chron)
library(purrr)
library(writexl)
library(shinycssloaders)
library(glue)
library(fishmethods)
library(hnp)
library(MASS)
library(glmmTMB)
library(MuMIn)
library(plotly)
library(gapminder)
library(AER)
library(pROC)
library(DescTools)
library(emdbook)
library(AICcmodavg)
library(investr)

library(gt)
library(officer)
library(flextable)
library(forcats)
library(labelled)
# # Set path to your data
# path <- "data/exempledata.xlsx"
# # Set the variables for filtering
# typ_pechvar <- "PENT"
# no_lac_var <- "01480"
# annee_var <- 2020

path <- "data/Extract IFA_R04_AquaPop.xlsx"
typ_pechvar <- "PENDJ"
no_lac_var <- "00005"
annee_var <- 2005


source("R/load_lac.R")
data_lac <- load_lac(path, namesheet= "Lac")

# Load your custom functions
source("R/load_station.R")
source("R/load_recolte.R")
source("R/load_specimen.R")
source("R/utils.R")
source("R/create_sp_pen.R")
source("R/create_specimen.R")
source("R/create_capture.R")
source("R/create_specimen_valid.R")

source("R/abondance_table.R")
source("R/biomasse_table.R")

# Load datasets
data_station <- load_station(path, namesheet= "Stations")
data_recolte <- load_recolte(path, namesheet= "Recolte")
data_specimen <- load_specimen(path, namesheet= "Specimens")

# Filter data based on specified variables
data_station <- data_station %>% filter(typ_pech == typ_pechvar & no_lac == no_lac_var & annee == annee_var)
data_recolte <- data_recolte %>% filter(typ_pech == typ_pechvar & no_lac == no_lac_var & annee == annee_var)
data_specimen <- data_specimen %>% filter(typ_pech == typ_pechvar & no_lac == no_lac_var & annee == annee_var)

# Create derived variables
sp_pen <- create_sp_pen(input_typ_pech = typ_pechvar)
specimen <- create_specimen(data_specimen, data_station)
specimen_valid <- create_specimen_valid(data_specimen, data_station)

capture <- create_capture(data_station, data_recolte)

# Test abundance and biomass tables
abondance_table <- abondance_table(specimen, sp_pen) %>% as.data.frame()
biomasse_table <- biomasse_table(specimen, sp_pen, data_station) %>% as.data.frame()

# Use the functions to define binwidth and nomsp
binwidth <- get_binwidth(sp_pen)
nomsp <- get_nomsp(sp_pen)

source("R/structure_taille.R")  # Assuming you refactor all structure_taille_* functions into structure_taille

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

source("R/structure_age.R")  # Charger la fonction depuis le script

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


source("R/taille_masse_age.R")
taille_masse_agedf <- taille_masse_age(dataspecimen = specimen, espece = sp_pen)

source("R/relation_masse_longueur.R")

relation_masse_longueur(data = data_specimen, espece = sp_pen) %>%
  plotly::ggplotly(tooltip = "text")



source("R/psd_indice.R")
psd_indice(data = specimen_valid, sp = sp_pen) %>% as.data.frame()

source("R/psd_byclass.R")

psd_byclass(data = specimen_valid, espece = sp_pen) %>% as.data.frame()

source("R/table_wri.R")
table_wri(data = data_specimen, espece = sp_pen)

source("R/fig_wri_tous.R")
fig_wri_tous(data = data_specimen, espece = sp_pen)

source("R/fig_wri_byclass.R")
fig_wri_byclass(data = data_specimen, espece = sp_pen)

# source("R/utils.R")
source("R/death.R")
deathdf <- death(data = specimen, espece = sp_pen)

source("R/peakplus.R")
pp <- peakplus(data = deathdf)
newPP <- pp #on le change ici si on veut autre chose

source("R/agemax.R")
agemax_val <- agemax(data = deathdf)

source("R/creation_df_CORR.R")
df_corr <- creation_df_CORR(data = deathdf,
                            peakplus = newPP,
                            agemax = agemax_val) %>% as.data.frame()

source("R/creation_df_EXT.R")
df_ext <- creation_df_EXT(data = df_corr,
                            peakplus = newPP,
                            agemax = agemax_val) %>% as.data.frame()



source("R/mortalite_selection_modeles.R")
df_EXT = df_ext
# mortalite1 <- mortalite_selection_modeles(df_EXT = df_ext) #ca fini pu

source("R/mortalite_chaprob.R")

mortalite2 <- mortalite_chaprob(data = deathdf,
                          pp = newPP,
                          agemax_val = agemax_val) %>% as.data.frame() %>% gt_mortalite2()

source("R/psd_indice.R")
psd1 <- psd_indice(data = specimen, sp = sp_pen)

source("R/psd_byclass.R")

# psd2 <- psd_byclass(data = specimen, sp = sp_pen) %>% as.data.frame()
data = specimen
sp = sp_pen

  #selectionner slmt le data necessaire

source("R/create_initcroissance.R")
initcroissance <- create_initcroissance(specimen, sp_pen)

source("R/courbe_croissance_comparaison.R")
croissance1 <- courbe_croissance_comparaison(data = initcroissance)

source("R/courbe_croissance_ggVONBERT.R")
courbe_croissance_ggVONBERT(initcroissance = initcroissance, tablemodele = croissance1)

source("R/courbe_croissance_ggGOMP.R")
courbe_croissance_ggGOMP(initcroissance = initcroissance, tablemodele = croissance1)

source("R/courbe_croissance_ggLOGIST.R")
courbe_croissance_ggLOGIST(initcroissance = initcroissance, tablemodele = croissance1)



