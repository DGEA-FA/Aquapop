load_parametres <- function(path, namesheet) {
  parametres <- readxl::read_excel(
    path$datapath,
    col_names = TRUE,
    sheet = namesheet,
    na = c("", "NULL", "NA", " "),
    col_types = c("text",
                  "text", "date", "text", "text", "text",
                  "text", "text")
  ) %>%
    as.data.frame()
  colnames(parametres)[1] <-
    'no_lac' #renommer la 1 colonne "No plan d'eau fusionné"
  colnames(parametres)[2] <-
    'nom_lac' #renommer la 2 colonne "Nom plan d'eau fusionné"
  colnames(parametres)[3] <-
    'date' #renommer la 3 colonne "date"
  colnames(parametres)[4] <-
    'typ_pech' #renommer la 4 colonne "Type de pêche"
  colnames(parametres)[5] <-
    'no_station' #renommer la 5 colonne "No station"
  colnames(parametres)[6] <-
    'nom_param' #renommer la 6 colonne "Nom paramètre"
  colnames(parametres)[7] <-
    'results' #renommer la 7 volonne "Résultat"
  colnames(parametres)[8] <-
    'comments' #renommer la 8 colonne  "Commentaires"
  parametres <- mutate_at(parametres,
                          vars(no_lac,
                               nom_lac,
                               typ_pech,
                               no_station,
                               nom_param),
                          factor) #transformer en factor
  parametres$results <-
    as.numeric(parametres$results)#transformer en numeric
  parametres <-
    parametres %>% mutate(annee = format(date, format = "%Y"))# isoler annee
  parametres$comments <-
    as.character(parametres$comments)#transformer en character
  parametres %>% dplyr::distinct()
}