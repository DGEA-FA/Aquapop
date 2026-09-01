#' Préparer les jeux de données pour l'analyse
#'
#' Cette fonction importe, nettoie, filtre et fusionne les données de stations, de spécimens
#' et de récoltes à partir d'un fichier Excel structuré selon les standards AquaPop. 
#' Elle retourne tous les objets nécessaires aux analyses biologiques, incluant des variantes 
#' prêtes à l'emploi pour les stations valides et/ou au hasard.
#'
#' @param path Chemin complet vers le fichier Excel (.xlsx) à importer
#' @param typ_pech Type de pêche sélectionné (ex: `"PENDJ"`)
#' @param no_lac Numéro du lac sélectionné (ex: `"00045"`)
#' @param annee Année sélectionnée (ex: `2022`)
#' @param sheet_station Nom du feuillet des stations (défaut = `"Stations"`)
#' @param sheet_specimen Nom du feuillet des spécimens (défaut = `"Specimens"`)
#' @param sheet_recolte Nom du feuillet des récoltes (défaut = `"Recolte"`)
#' @param verbose Afficher les messages de progression des fonctions `load_*` (défaut = `TRUE`)
#'
#' @return Une liste nommée contenant :
#' \describe{
#'   \item{data_station}{Toutes les stations du lac sélectionné (filtrées par année et type de pêche)}
#'   \item{station_valide}{Sous-ensemble des stations valides (`st_valide == "O"`)}
#'   \item{station_hasard_valide}{Sous-ensemble des stations valides et au hasard (`st_valide == "O" & st_hasard == "O"`)}
#'   \item{specimen_tous}{Tous les spécimens de l'espèce cible associés}
#'   \item{specimen_valide}{Spécimens de l'espèce cible associés à toutes les stations valides (hasard + dirigées)}
#'   \item{specimen_hasard_valide}{Spécimens de l'espèce cible associés à toutes les stations valides et au hasard}
#'   \item{capture}{Table des captures par station (inclut toutes les stations, même celles sans capture)}
#' }
#'
#' @importFrom tidyr replace_na
#' @importFrom dplyr full_join mutate distinct left_join filter inner_join if_else
#' @export
get_analysis_data <- function(path, typ_pech, no_lac, annee,
                              sheet_station = "Stations",
                              sheet_specimen = "Specimens",
                              sheet_recolte = "Recolte",
                              verbose = TRUE) {
  
  # Identification ----
  ## Espèce cible à partir du type de pêche ----
  info_pen <- get_info_pen(typ_pech)
  if (is.null(info_pen)) stop("Type de pêche inconnu : aucun code espèce disponible.")
  code_sp <- info_pen$code_sp
  
  # Chargement des données ----
  
  ## Feuille des stations ----
  data_station <- load_station(path, sheet_station, verbose = verbose) |>
    filter_by_pen_lac_annee(typ_pech = typ_pech, no_lac = no_lac, annee = annee)
  
  # --- Réorganisation pour visualisation ---
    data_station <- data_station |>
      select(no_lac,
           annee,
           typ_pech,
           no_station,
           st_hasard,
           st_valide,
           lat_dd.dec,
           long_dd.dec,
           prof_deb,
           prof_fin,
           date_pose,
           h_pose,
           date_leve,
           h_leve,
           comments_station
           )
    
    data_station <- data_station |>
      arrange(no_station)
  
    # Préparation des stations ----
    station_valide <- filter(data_station, .data$st_valide == "O")
    station_hasard_valide <- filter(
      data_station, 
      .data$st_valide == "O", 
      .data$st_hasard == "O")
    
    
  ## Feuille des spécimens ----
  data_specimen <- load_specimen(path, sheet_specimen, verbose = verbose) |>
    filter_by_pen_lac_annee(typ_pech = typ_pech, no_lac = no_lac, annee = annee) |>
    filter(.data$sp == code_sp)

  # --- Réorganisation pour visualisation ---
    data_specimen <- data_specimen |>
      select(no_lac,
           annee,
           typ_pech,
           no_station,
           st_valide,
           st_hasard,
           no_specimen,
           sp,
           ltm,
           lf,
           masse,
           sexe,
           maturite,
           age,
           marquage,
           comments_specimen
           )
  
    data_specimen <- data_specimen |>
      arrange(no_specimen)  
  
    # Préparation des spécimens ----
    specimen_tous <- data_specimen # Spécimens associés à toutes les stations ----
    
    specimen_valide <- filter(data_specimen, .data$st_valide == "O") # Spécimens associés à toutes les stations valides ----
    
    specimen_hasard_valide <- filter(
      data_specimen,
      .data$st_valide == "O", 
      .data$st_hasard == "O")
    
    
  ## Feuille de la récolte ----
  # Somme des captures par stations (si panneau ou commentaire)
  data_recolte <- load_recolte(path, sheet_recolte, verbose = verbose) |>
    filter_by_pen_lac_annee(typ_pech = typ_pech, no_lac = no_lac, annee = annee) |>
    filter(.data$sp == code_sp) |>
    group_by(no_station, st_hasard, st_valide) |>
    summarise(
      no_lac = first(no_lac),
      annee = first(annee),
      typ_pech = first(typ_pech),
      sp = first(sp),
      nb_capture = sum(nb_capture, na.rm = TRUE),
      nb_pese = sum(nb_pese, na.rm = TRUE),
      .groups = "drop"
      )
  
  # Ajout des stations sans captures (toutes les stations) ----
    data_recolte <- left_join(
    data_station, 
    data_recolte,
    by = c("no_station", "annee", "no_lac", "typ_pech", "st_valide", "st_hasard"),
    relationship = "one-to-one" #possible puisqu'on a déjà fait filter(sp == code_sp) plus tôt
  ) |>
    mutate(
      sp = if_else(is.na(.data$sp), code_sp, .data$sp),
      nb_capture = replace_na(.data$nb_capture, 0),
      nb_pese    = replace_na(.data$nb_pese, 0)
    ) |>
    distinct() |>
    droplevels()
  
    # --- Réorganisation dans l'ordre des variables (visualisation) ---
    
    data_recolte <-  data_recolte |>
      select(no_lac,
             annee,
             typ_pech,
             sp,
             no_station,
             st_hasard,
             st_valide,
             nb_capture,
             nb_pese
      ) 
    
    data_recolte <-  data_recolte |>
      arrange(no_station)
  
  # Préparation des captures (stations valides et hasard seulement) ----
  capture <- filter(
    data_recolte, 
    .data$st_valide == "O", 
    .data$st_hasard == "O")
  
#    capture <- full_join(
#    station_hasard_valide, 
#    data_recolte,
#    by = c("no_station", "annee", "no_lac", "typ_pech", "st_valide", "st_hasard"),
#    relationship = "one-to-one" #possible puisqu'on a déjà fait filter(sp == code_sp) plus tôt
#  ) |>
#    mutate(
#      sp = if_else(is.na(.data$sp), code_sp, .data$sp),
#      nb_capture = replace_na(.data$nb_capture, 0),
#      nb_pese    = replace_na(.data$nb_pese, 0)
#    ) |>
#    distinct() |>
#    droplevels()
  
  
  # Retour ----
  return(list(
    data_station = data_station,
    station_valide = station_valide,
    station_hasard_valide = station_hasard_valide,
    specimen_tous = specimen_tous,
    specimen_valide = specimen_valide,    
    specimen_hasard_valide = specimen_hasard_valide,
    data_recolte = data_recolte,
    capture = capture
  ))
}
