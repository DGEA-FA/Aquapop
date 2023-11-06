load_specimen <- function(path, namesheet) {
  specimen <- readxl::read_excel(
    path$datapath,
    col_names = TRUE,
    sheet = namesheet,
    na = c("", "NULL", "NA", " "),
    col_types = "text"
  ) %>%
    as.data.frame()
  colnames(specimen)[1] <-
    'no_lac' #renommer la 1 colonne "No plan d'eau fusionné"
  colnames(specimen)[2] <-
    'nom_lac' #renommer la 2 colonne "Nom plan d'eau fusionné"
  colnames(specimen)[3] <-
    'typ_pech' #renommer la 3 colonne "Type de pêche"
  colnames(specimen)[4] <-
    'annee' #renommer la 4 colonne "Anné début inventaire"
  colnames(specimen)[5] <-
    'no_station' #renommer la 5 colonne "No station"
  colnames(specimen)[6] <-
    'no_specimen' #renommer la 6 colonne "No spécimen"
  colnames(specimen)[7] <- 'sp' #renommer la 7 colonne "Espèce code"
  colnames(specimen)[8] <-
    'ltm' #renommer la 8 colonne  "Long. totale max (mm)"
  colnames(specimen)[9] <-
    'lf' #renommer la 9 colonne  "Longueur à la fourche (mm)"
  colnames(specimen)[10] <-
    'masse' #renommer la 10 colonne  "masse (g)"
  colnames(specimen)[11] <-
    'sexe' #renommer la 11 colonne  "sexe"
  colnames(specimen)[12] <-
    'maturite' #renommer la 12 colonne  "Maturité sexuelle"
  colnames(specimen)[13] <- 'age' #renommer la 13 colonne  "Âge 1"
  colnames(specimen)[14] <-
    'ind_insec' #renommer la 14 colonne  "Ind. insecte"
  colnames(specimen)[15] <-
    'ind_benth' #renommer la 15 colonne  "Ind. benthos"
  colnames(specimen)[16] <-
    'ind_planc' #renommer la 16 colonne  "Ind. plancton"
  colnames(specimen)[17] <-
    'ind_chyme' #renommer la 17 colonne  "Ind. chyme"
  colnames(specimen)[18] <-
    'ind_vide' #renommer la 18 colonne "Ind. vide"
  colnames(specimen)[19] <-
    'ind_poiss' #renommer la 19 colonne  "Ind. poisson"
  colnames(specimen)[20] <-
    'poiss1' #renommer la 20 colonne  "Contenu - Poisson 1"
  colnames(specimen)[21] <-
    'poiss2' #renommer la 21 colonne  "Contenu - Poisson 2"
  colnames(specimen)[22] <-
    'marquage' #renommer la 22 colonne  "Statut marquage"
  colnames(specimen)[23] <-
    'comments' #renommer la 23 colonne "Commentaires"
  specimen <- mutate_at(
    specimen,
    vars(
      no_lac,
      nom_lac,
      typ_pech,
      annee,
      no_station,
      no_specimen,
      sp,
      sexe,
      maturite,
      ind_insec,
      ind_benth,
      ind_planc,
      ind_chyme,
      ind_vide,
      ind_poiss,
      poiss1,
      poiss2,
      marquage
    ),
    factor
  ) #transformer en factor
  specimen$ltm <- as.numeric(specimen$ltm)#transformer en numeric
  specimen$lf <- as.numeric(specimen$lf)#transformer en numeric
  specimen$masse <-
    as.numeric(specimen$masse)#transformer en numeric
  specimen$age <- as.numeric(specimen$age)#transformer en numeric
  specimen$comments <-
    as.character(specimen$comments)#transformer en character
  specimen %>% dplyr::distinct()
}