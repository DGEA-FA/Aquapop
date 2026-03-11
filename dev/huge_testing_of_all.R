# # Charger toutes les fonctions du package en développement
devtools::load_all()
# 
# # Téléchargement des données ----
# 
path     <- "inst/extdata/Extract_IFA_AquaPop_2026-02-27.xlsx"
data_lac <- load_lac(path)
# 
# 
# combinaison_lac <- data_lac |>
#   dplyr::distinct(no_lac, typ_pech, annee) |>
#   tibble::as_tibble()|> filter(typ_pech!="PENDJ")


# writexl::write_xlsx(combinaison_lac, "combinaison_lac.xlsx")

############
