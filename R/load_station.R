#' Charger et structurer les données du feuillet "Stations" (version robuste)
#'
#' Cette fonction importe les données brutes du feuillet "Stations" d’un fichier Excel,
#' applique des transformations pour nettoyer et standardiser les colonnes essentielles,
#' et calcule des variables supplémentaires utiles pour l'analyse (ex. date/heure de pose/levée, durée).
#'
#' @param path Chemin complet vers le fichier Excel (.xlsx) à importer.
#' @param namesheet Nom du feuillet contenant les données de stations (par défaut `"Stations"`).
#' @param verbose Afficher les messages de diagnostic ? (défaut `TRUE`)
#'
#' @return Un `data.frame` structuré avec au minimum :
#' \describe{
#'   \item{no_lac, typ_pech, no_station, st_valide, st_hasard, type_maill}{Facteurs}
#'   \item{annee}{Année (entier)}
#'   \item{lat_dd.dec, long_dd.dec, prof_deb, prof_fin}{Numériques}
#'   \item{date_pose, date_leve, h_pose, h_leve, pose, leve, duree}{Variables temporelles}
#'   \item{comments_station}{Chaîne de caractères}
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom janitor make_clean_names
#' @importFrom lubridate year days
#' @importFrom dplyr mutate across distinct case_when select
#' @importFrom checkmate assert_file_exists assert_character assert_flag
#' @importFrom stringr str_pad
#' @export
load_station <- function(path,
                         namesheet = "Stations",
                         verbose = TRUE) {
  # --- Validation des entrées ---
  assert_file_exists(path, extension = "xlsx")
  assert_character(namesheet, len = 1)
  assert_flag(verbose)
  
  # --- Colonnes attendues ---
  colonnes_obligatoires <- c("no_lac", "typ_pech", "annee", "no_station", "st_valide", "st_hasard")
  colonnes_optionnelles <- c(
    "lat_dd.dec", "long_dd.dec", "prof_deb", "prof_fin",
    "date_pose", "date_leve", "heure_pose", "min_pose",
    "heure_leve", "min_leve", "h_pose", "h_leve",
    "pose", "leve", "duree", "type_maill", "comments"
  )
  
  # --- Synonymes pour mappage des colonnes ---
  synonymes <- list(
    no_lac      = c("no_lac", "numero_lce", "no_plan_deau"),
    typ_pech    = c("typ_pech", "type_peche", "type_de_peche"),
    annee       = c("annee", "année"),
    no_station  = c("no_station", "station_id", "no_stn"),
    st_valide   = c("st_valide", "station_valide", "valide"),
    st_hasard   = c("st_hasard", "hasard", "tirage_aleatoire"),
    leve        = c("leve"),
    comments    = c("comments", "commentaires", "commentaires_generaux"),
    lat_dd.dec  = c("lat_dd.dec", "latitude_degre_decimales"),
    long_dd.dec = c("long_dd.dec", "longitude_degre_decimales"),
    heure_pose  = c("heure_pose", "heure_de_pose_du_filet"),
    min_pose    = c("min_pose", "minute_pose_de_filet"),
    heure_leve  = c("heure_leve", "heure_de_levee_du_filet"),
    min_leve    = c("min_leve", "minute_levee_de_filet_mm"),
    date_leve   = c("date_leve", "date_de_levee_du_filet"),
    type_maill  = c("type_maill", "type_mailles_en_rive")
  )
  
  # --- Lecture brute ---
  station_raw <- read_excel(
    path,
    sheet = namesheet,
    col_names = TRUE,
    col_types = "text",
    na = c("", "NULL", "NA", " ", "-")
  ) |> as.data.frame()
  
  noms_originaux <- names(station_raw)
  noms_clean <- make_clean_names(noms_originaux)
  
  # --- Renommage intelligent ---
  mapping <- sapply(names(synonymes), function(canonique) {
    idx <- match(synonymes[[canonique]], noms_clean)
    idx <- idx[!is.na(idx)]
    if (length(idx) > 0) noms_originaux[idx[1]] else NA
  }, USE.NAMES = TRUE)
  
  # --- Validation des colonnes obligatoires ---
  manquantes <- names(mapping[colonnes_obligatoires])[is.na(mapping[colonnes_obligatoires])]
  if (length(manquantes) > 0) {
    stop("Colonnes obligatoires manquantes : ", paste(manquantes, collapse = ", "))
  }
  
  # --- Construction du tableau structuré ---
  n <- nrow(station_raw)
  station <- as.data.frame(matrix(NA_character_, nrow = n, ncol = 0))
  for (col in names(mapping)) {
    source_col <- mapping[[col]]
    if (!is.na(source_col)) {
      station[[col]] <- station_raw[[source_col]]
      if (verbose) message("[load_station] Colonne ‘", source_col, "' reconnue comme ‘", col, "'.")
    } else {
      station[[col]] <- rep(NA_character_, n)
      if (verbose) message("[load_station] Colonne ‘", col, "' absente, ajoutée comme NA.")
    }
  }
  
  # --- Colonnes optionnelles absentes : ajout en NA ---
  colonnes_manquantes <- setdiff(colonnes_optionnelles, names(station))
  for (col in colonnes_manquantes) {
    station[[col]] <- NA
    if (verbose) message("[load_station] Colonne ‘", col, "' absente, ajoutée comme NA.")
  }
  
  # --- Nettoyage des statuts ---
  station <- station |>
    mutate(
      st_valide = case_when(
        is.na(st_valide) | st_valide %in% c("IND", "-", "VALIDE") ~ "O",
        TRUE ~ st_valide
      ),
      st_hasard = case_when(
        st_hasard %in% c("REJETEE") ~ "N",
        is.na(st_hasard) | st_hasard %in% c("IND", "-") ~ "O",
        TRUE ~ st_hasard
      )
    )
  
  # --- Conversion types de base ---
  station <- station |>
    mutate(
      annee = case_when(
        nchar(annee) == 5 ~ as.integer(year(as.Date(as.numeric(annee), origin = "1899-12-30"))),
        TRUE              ~ suppressWarnings(as.integer(annee))
      ),
      across(
        intersect(c("no_lac", "typ_pech", "no_station", "type_maill", "st_hasard", "st_valide"), names(station)),
        as.factor
      ),
      lat_dd.dec  = suppressWarnings(as.numeric(lat_dd.dec)),
      long_dd.dec = suppressWarnings(as.numeric(long_dd.dec)),
      prof_deb    = suppressWarnings(as.numeric(prof_deb)),
      prof_fin    = suppressWarnings(as.numeric(prof_fin)),
      comments_station = as.character(comments)
    ) |>
    select(-comments)
  
  # --- Dates et heures combinées ---
  station$date_leve <- suppressWarnings(as.Date(as.numeric(station$date_leve), origin = "1899-12-30"))
  station$date_pose <- if (!all(is.na(station$date_leve))) station$date_leve - days(1) else NA
  
  station <- station |>
    mutate(
      min_pose   = str_pad(min_pose,   2, pad = "0"),
      heure_pose = str_pad(heure_pose, 2, pad = "0"),
      min_leve   = str_pad(min_leve,   2, pad = "0"),
      heure_leve = str_pad(heure_leve, 2, pad = "0"),
      h_pose     = ifelse(!is.na(heure_pose) & !is.na(min_pose), paste0(heure_pose, ":", min_pose, ":00"), NA),
      h_leve     = ifelse(!is.na(heure_leve) & !is.na(min_leve), paste0(heure_leve, ":", min_leve, ":00"), NA),
      pose       = suppressWarnings(as.POSIXct(paste(date_pose, h_pose), format = "%Y-%m-%d %H:%M:%S")),
      leve       = suppressWarnings(as.POSIXct(paste(date_leve, h_leve), format = "%Y-%m-%d %H:%M:%S")),
      duree      = difftime(leve, pose, units = "hours")
    )
  
  # --- Suppression des doublons ---
  station <- distinct(station)
  
  return(station)
}
