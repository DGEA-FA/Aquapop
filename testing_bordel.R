library(dplyr)
library(ggplot2)
path <- "C:/Users/carol/OneDrive/EmploiMFFP/Hiver2022-2023/Jeu de données.xlsx"

typ_pechvar <- "PENT" 
no_lac_var <- "01480" 
annee_var <- 1994   


source("R/load_station.R")
data_station <- load_station(path, namesheet= "Stations")
source("R/load_recolte.R")
data_recolte <- load_recolte(path, namesheet= "Recolte")
source("R/load_specimen.R")
data_specimen <- load_specimen(path, namesheet= "Specimens")


data_station <- data_station %>% filter(typ_pech==typ_pechvar & no_lac==no_lac_var & annee==annee_var)
data_recolte <- data_recolte %>% filter(typ_pech==typ_pechvar & no_lac==no_lac_var & annee==annee_var)
data_specimen <- data_specimen %>% filter(typ_pech==typ_pechvar & no_lac==no_lac_var & annee==annee_var)

source("R/verifier_doublons_data_recolte.R")
verifier_doublons_data_recolte(data_recolte)

source("R/verifier_doublons_data_station.R")
verifier_doublons_data_station(data_station)

source("R/create_sp_pen.R")
sp_pen <- create_sp_pen(input_typ_pech = typ_pechvar)

source("R/create_specimen.R")
specimen <- create_specimen(data_specimen,data_station )
source("R/create_capture.R")
capture <- create_capture(data_station,data_recolte )


source("R/structure_taille_tous.R")
structure_taille_tous(dfspecimen = specimen, espece = sp_pen)
source("R/structure_taille_sexe.R")
structure_taille_sexe(dfspecimen = specimen, espece = sp_pen)
source("R/structure_taille_maturite.R")
structure_taille_maturite(dfspecimen = specimen, espece = sp_pen)
source("R/structure_taille_marquage.R")
structure_taille_marquage(dfspecimen = specimen, espece = sp_pen)

source("R/structure_age_tous.R")
structure_age_tous(dfspecimen = specimen, espece = sp_pen)

source("R/structure_age_maturite.R")
structure_age_maturite(dfspecimen = specimen, espece = sp_pen)

source("R/structure_age_sexe.R")
structure_age_sexe(dfspecimen = specimen, espece = sp_pen)

source("R/structure_age_marquage.R")
structure_age_marquage(dfspecimen = specimen, espece = sp_pen)

source("R/taille_masse_age.R")
taille_masse_age(dataspecimen = specimen, espece = sp_pen)

source("R/psd_indice.R")
psd_indice(data = specimen, sp = sp_pen)

source("R/table_wri.R")
table_wri(data = specimen, espece = sp_pen)
data = specimen
espece = sp_pen

source("R/fig_wri_tous.R")
fig_wri_tous(data = specimen, espece = sp_pen)

source("R/fig_wri_byclass.R")
# fig_wri_byclass(data = specimen, espece = sp_pen)

source("R/utils.R")
deathdf <- death(data = specimen, espece = sp_pen)
pp <- peakplus(data = deathdf)
newPP <- pp #on le change ici si on veut autre chose
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

source("R/mortalite_ChapmanRobson.R")

mortalite2 <- mortalite_ChapmanRobson(data = deathdf,
                          pp = newPP,
                          agemax_val = agemax_val) %>% as.data.frame()

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
# courbe_croissance_ggVONBERT(initcroissance = initcroissance, tablemodele = croissance1)

source("R/courbe_croissance_ggGOMP.R")
# courbe_croissance_ggGOMP(initcroissance = initcroissance, tablemodele = croissance1)

source("R/courbe_croissance_ggLOGIST.R")
# courbe_croissance_ggLOGIST(initcroissance = initcroissance, tablemodele = croissance1)