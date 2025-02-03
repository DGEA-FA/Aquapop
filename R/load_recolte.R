load_recolte <- function(path, namesheet) {
  # Charger les données à partir du fichier Excel
  recolte <- readxl::read_excel(
    path,
    col_names = TRUE,
    sheet = namesheet,
    na = c("", "NULL", "NA", " ", "-"), # Considérer ces valeurs comme NA
    col_types = "text" # Toutes les colonnes en tant que texte
  ) %>%
    as.data.frame() # Convertir en data.frame pour une manipulation plus facile
  
  # Renommer les colonnes
  colnames(recolte) <- c(
    'no_lac',        # 1ère colonne : No plan d'eau
    'nom_lac',       # 2ème colonne : Nom plan d'eau
    'typ_pech',      # 3ème colonne : Type de pêche
    'annee',         # 4ème colonne : Année
    'no_station',    # 5ème colonne : No station
    'sp',            # 6ème colonne : Espèce
    'nb_capture',    # 7ème colonne : Nbre capturé
    'nb_pese',       # 8ème colonne : Nbre pesé
    'comments'       # 9ème colonne : Commentaires
  )
  
  
  recolte$annee <- recolte$annee %>% as.integer()
  recolte$annee <- ifelse(nchar(recolte$annee) == 5, 
                      as.integer(lubridate::year(as.Date(recolte$annee, origin = "1899-12-30"))), 
                      recolte$annee)  
  
  
  # Convertir les colonnes appropriées en facteurs, numériques ou caractères
  recolte <- recolte %>%
    mutate(
      no_lac = as.factor(no_lac),
      nom_lac = as.factor(nom_lac),
      typ_pech = as.factor(typ_pech),
      no_station = as.factor(no_station),
      sp = as.factor(sp),
      nb_capture = as.numeric(nb_capture),
      nb_pese = as.numeric(nb_pese),
      comments = as.character(comments)
    )
  
  # Supprimer les doublons
  recolte <- recolte %>% dplyr::distinct()
  
  return(recolte)
}