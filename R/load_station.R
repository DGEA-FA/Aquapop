#' Charger et structurer les données du feuillet "Stations"
#'
#' Cette fonction importe les données brutes du feuillet "Stations" d’un fichier Excel,
#' applique des transformations pour nettoyer et standardiser les colonnes,
#' et calcule des variables supplémentaires utiles pour l’analyse (ex. date/heure de pose/levée, durée).
#'
#' @param path Chemin complet vers le fichier Excel (.xlsx) à importer.
#' @param namesheet Nom du feuillet contenant les données de stations (par défaut `"Stations"`).
#'
#' @return Un `data.frame` contenant :
#' \describe{
#'   \item{no_lac, nom_lac, typ_pech, no_station, st_valide, st_hasard, type_maill}{Facteurs}
#'   \item{annee}{Année numérique, convertie à partir du format Excel si nécessaire}
#'   \item{lat_dd.dec, long_dd.dec, prof_deb, prof_fin}{Numériques}
#'   \item{date_pose, date_leve}{Dates calculées (pose = veille de la levée)}
#'   \item{heure_pose, min_pose, heure_leve, min_leve}{Chaînes formatées à deux chiffres}
#'   \item{h_pose, h_leve}{Heures complètes au format texte ("HH:MM:SS")}
#'   \item{pose, leve}{Date/heure combinées (`POSIXct`)}
#'   \item{duree}{Durée entre pose et levée (`difftime`)}
#'   \item{comments}{Chaîne de caractères}
#' }
#'
#' @details
#' La fonction effectue les étapes suivantes :
#' \enumerate{
#'   \item Lecture du fichier Excel avec toutes les colonnes en texte.
#'   \item Renommage des colonnes selon le format attendu.
#'   \item Nettoyage des colonnes `st_valide` et `st_hasard`, remplacement des valeurs manquantes.
#'   \item Conversion des types (facteurs, numériques, dates).
#'   \item Construction des variables de temps `pose`, `leve`, et `duree`.
#'   \item Suppression des doublons.
#' }
#'
#' Cette fonction peut être utilisée dans une application interactive ou dans un script classique.
#'
#' @importFrom readxl read_excel
#' @importFrom lubridate year
#' @importFrom dplyr mutate across case_when distinct
#' @importFrom stringr str_pad
#' @export

load_station <- function(path, namesheet) {
 
  # 1. Lecture du fichier Excel
  station <- readxl::read_excel(
    path,
    col_names = TRUE,
    sheet = namesheet,
    na = c("", "NULL", "NA", " ", "-"),
    col_types = "text"
  ) %>% 
    as.data.frame()
  
  # 2. Renommage des colonnes
  colnames(station) <- c(
    'no_lac',          # 1ère colonne : No plan d'eau
    'nom_lac',         # 2ème colonne : Nom plan d'eau
    'typ_pech',        # 3ème colonne : Type de pêche
    'annee',           # 4ème colonne : Année
    'no_station',      # 5ème colonne : No station
    'lat_dd.dec',      # 6ème colonne : Latitude (degré, décimales)
    'long_dd.dec',     # 7ème colonne : Longitude (degré, décimales)
    'heure_pose',      # 8ème colonne : Heure de pose du filet
    'min_pose',        # 9ème colonne : Minute - Pose de filet
    'date_leve',       # 10ème colonne : Date de levée du filet
    'heure_leve',      # 11ème colonne : Heure de levée du filet
    'min_leve',        # 12ème colonne : Minute - Levée de filet
    'st_hasard',       # 13ème colonne : Hasard
    'st_valide',       # 14ème colonne : Station valide
    'prof_deb',        # 15ème colonne : Profondeur début (m)
    'prof_fin',        # 16ème colonne : Profondeur fin (m)
    'type_maill',      # 17ème colonne : Type mailles en rive
    'comments'         # 18ème colonne : Commentaires
  )
  
  # 3. Nettoyage des statuts : NA, "IND" ou "-" deviennent "O" dans les colonnes "st_valide" et "st_hasard"
  station <- station %>%
    dplyr::mutate(
      st_valide = dplyr::case_when(
        is.na(st_valide) | st_valide %in% c("IND", "-") ~ "O",
        TRUE ~ st_valide
      ),
      st_hasard = dplyr::case_when(
        is.na(st_hasard) | st_hasard %in% c("IND", "-") ~ "O",
        TRUE ~ st_hasard
      )
    )

  # 4. Conversion des types de base
  station <- station %>%
    dplyr::mutate(
      across(c(no_lac, nom_lac, typ_pech, st_hasard, st_valide, type_maill, no_station), as.factor),
      annee = dplyr::case_when(
        nchar(annee) == 5 ~ as.integer(lubridate::year(as.Date(as.numeric(annee), origin = "1899-12-30"))),
        TRUE              ~ suppressWarnings(as.integer(annee))
      ),
      lat_dd.dec  = as.numeric(lat_dd.dec),
      long_dd.dec = as.numeric(long_dd.dec),
      prof_deb    = as.numeric(prof_deb),
      prof_fin    = as.numeric(prof_fin),
      comments    = as.character(comments)
    )
  
  # 6. Conversion de la date de levée et calcul de la date de pose
  station$date_leve <- as.Date(as.numeric(station$date_leve), origin = "1899-12-30")
  station$date_pose <- station$date_leve - lubridate::days(1) # veille de la levée
  
  # 7. Normalisation des heures et minutes (ex : "7" → "07")
  station <- station %>%
    dplyr::mutate(
      min_pose   = stringr::str_pad(min_pose,   2, pad = "0"),
      heure_pose = stringr::str_pad(heure_pose, 2, pad = "0"),
      min_leve   = stringr::str_pad(min_leve,   2, pad = "0"),
      heure_leve = stringr::str_pad(heure_leve, 2, pad = "0")
    )
  
  # 8. Construction des champs horaires en ajoutant les secondes
  station <- station %>%
    dplyr::mutate(
      h_pose = ifelse(!is.na(heure_pose) & !is.na(min_pose), paste0(heure_pose, ":", min_pose, ":00"), NA),
      h_leve = ifelse(!is.na(heure_leve) & !is.na(min_leve), paste0(heure_leve, ":", min_leve, ":00"), NA)
    )
  
  # 9. Fusion date + heure → POSIXct
  station <- station %>%
    dplyr::mutate(
      pose = as.POSIXct(paste(date_pose, h_pose), format = "%Y-%m-%d %H:%M:%S"),
      leve = as.POSIXct(paste(date_leve, h_leve), format = "%Y-%m-%d %H:%M:%S"),
      duree = difftime(leve, pose, units = "auto")
    )
  
  # 10. Tri et suppression des doublons
  station <- station[order(station$no_station), ]
  station <- station %>% dplyr::distinct()
  
  return(station)
}