load_recolte <- function(path, namesheet) {
  recolte <- readxl::read_excel(
    path$datapath,
    col_names = TRUE,
    sheet = namesheet,
    na = c("", "NULL", "NA", " "),
    col_types = "text"
  ) %>%
    as.data.frame()
  colnames(recolte)[1] <-
    'no_lac' #renommer la 1 colonne "No plan d'eau"
  colnames(recolte)[2] <-
    'nom_lac' #renommer la 2 colonne "Nom plan d'eau"
  colnames(recolte)[3] <-
    'typ_pech' #renommer la 3 colonne "Type de pêche"
  colnames(recolte)[4] <- 'annee' #renommer la 4 colonne "Année"
  colnames(recolte)[5] <-
    'no_station' #renommer la 5 colonne "No station"
  colnames(recolte)[6] <- 'sp' #renommer la 6 colonne  "Espèce"
  colnames(recolte)[7] <-
    'nb_capture' #renommer la 7 colonne "Nbre capturé"
  colnames(recolte)[8] <-
    'nb_pese' #renommer la 8 colonne "Nbre pesé"
  colnames(recolte)[9] <-
    'comments' #renommer la 9 colonne "Commentaires"
  recolte <- mutate_at(recolte,
                       vars(no_lac,
                            nom_lac,
                            typ_pech,
                            annee,
                            no_station,
                            sp),
                       factor) #transformer en factor
  recolte$nb_capture <-
    as.numeric(recolte$nb_capture)#transformer en numeric
  recolte$nb_pese <-
    as.numeric(recolte$nb_pese)#transformer en numeric
  recolte$comments <-
    as.character(recolte$comments)#transformer en character
  recolte %>% dplyr::distinct()
}