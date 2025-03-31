#' Charger et structurer les données du feuillet "Spécimens"
#'
#' Cette fonction importe les données brutes du feuillet "Specimens" d’un fichier Excel,
#' applique les transformations nécessaires pour nettoyer, convertir et uniformiser les types de colonnes,
#' puis retourne un tableau exploitable pour l’analyse biologique.
#'
#' @param path Chemin complet vers le fichier Excel (.xlsx) à importer.
#' @param namesheet Nom du feuillet contenant les données de spécimens (par défaut `"Specimens"`).
#'
#' @return Un `data.frame` contenant :
#' \describe{
#'   \item{no_lac, nom_lac, typ_pech, no_station, no_specimen, sp}{Facteurs}
#'   \item{ltm, lf, masse, age}{Numériques}
#'   \item{sexe, maturite, marquage}{Facteurs avec niveaux ordonnés ("F", "M", "IND"), ("O", "N", "IND"), ("MA", "NMA")}
#'   \item{ind_insec, ind_benth, ind_planc, ind_chyme, ind_vide, ind_poiss, poiss1, poiss2}{Facteurs}
#'   \item{annee}{Année numérique, convertie si format Excel}
#'   \item{comments}{Chaîne de caractères}
#' }
#'
#' @details
#' Étapes effectuées :
#' \enumerate{
#'   \item Lecture du fichier avec toutes les colonnes en texte
#'   \item Renommage des colonnes
#'   \item Remplacement des `NA` dans `sexe`, `maturite`, et `marquage`
#'   \item Conversion des colonnes en facteurs ou numériques selon leur nature
#'   \item Définition de niveaux explicites pour `sexe`, `maturite` et `marquage`
#'   \item Tri par numéro de spécimen croissant
#'   \item Suppression des doublons
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom lubridate year
#' @importFrom dplyr mutate across distinct arrange select
#' @importFrom tidyr replace_na
#' @export
load_specimen <- function(path, namesheet) {

  # 1. Lecture du fichier
  specimen <- readxl::read_excel(
    path,
    col_names = TRUE,
    sheet = namesheet,
    na = c("", "NULL", "NA", " ", "-"),
    col_types = "text"
  ) %>%
    as.data.frame()
  
  # 2. Renommage des colonnes
  colnames(specimen) <- c(
    'no_lac',        # 1ère colonne : No plan d'eau fusionné
    'nom_lac',       # 2ème colonne : Nom plan d'eau fusionné
    'typ_pech',      # 3ème colonne : Type de pêche
    'annee',         # 4ème colonne : Année début inventaire
    'no_station',    # 5ème colonne : No station
    'no_specimen',   # 6ème colonne : No spécimen
    'sp',            # 7ème colonne : Espèce code
    'ltm',           # 8ème colonne : Long. totale max (mm)
    'lf',            # 9ème colonne : Longueur à la fourche (mm)
    'masse',         # 10ème colonne : Masse (g)
    'sexe',          # 11ème colonne : Sexe
    'maturite',      # 12ème colonne : Maturité sexuelle
    'age',           # 13ème colonne : Âge 1
    'ind_insec',     # 14ème colonne : Ind. insecte
    'ind_benth',     # 15ème colonne : Ind. benthos
    'ind_planc',     # 16ème colonne : Ind. plancton
    'ind_chyme',     # 17ème colonne : Ind. chyme
    'ind_vide',      # 18ème colonne : Ind. vide
    'ind_poiss',     # 19ème colonne : Ind. poisson
    'poiss1',        # 20ème colonne : Contenu - Poisson 1
    'poiss2',        # 21ème colonne : Contenu - Poisson 2
    'marquage',      # 22ème colonne : Statut marquage
    'comments'       # 23ème colonne : Commentaires
  )
  
  # 3. Remplacement des valeurs manquantes dans certaines colonnes clés
  specimen <- specimen %>%
    dplyr::mutate(
      maturite = tidyr::replace_na(maturite, "IND"),
      sexe     = tidyr::replace_na(sexe, "IND"),
      marquage = tidyr::replace_na(marquage, "NMA")
    )
  
  # 4. Conversion des types
  specimen <- specimen %>%
    dplyr::mutate(
      across(
        c(no_lac, nom_lac, typ_pech, no_station, no_specimen, sp,
          ind_insec, ind_benth, ind_planc, ind_chyme, ind_vide, ind_poiss,
          poiss1, poiss2, sexe, maturite, marquage),
        as.factor
      ),
      sexe     = factor(sexe, levels = c("F", "M", "IND")),
      maturite = factor(maturite, levels = c("O", "N", "IND")),
      marquage = factor(marquage, levels = c("MA", "NMA"))
    )
  
  # 5. Conversion de l’année (Excel → entier si nécessaire)
  specimen$annee <- as.integer(specimen$annee)
  specimen$annee <- ifelse(
    nchar(specimen$annee) == 5,
    as.integer(lubridate::year(as.Date(specimen$annee, origin = "1899-12-30"))),
    specimen$annee
  )
  
  # 6. Conversion des autres colonnes
  specimen <- specimen %>%
    dplyr::mutate(
      ltm      = as.numeric(ltm),
      lf       = as.numeric(lf),
      masse    = as.numeric(masse),
      age      = as.numeric(age),
      comments = as.character(comments)
    )
  
  # 7. Tri croissant par no_specimen
  specimen <- specimen %>%
    dplyr::mutate(no_specimen_numeric = as.numeric(as.character(no_specimen))) %>%
    dplyr::arrange(no_specimen_numeric) %>%
    dplyr::select(-no_specimen_numeric)
  
  # 8. Suppression des doublons
  specimen <- specimen %>% dplyr::distinct()
  
  return(specimen)
}