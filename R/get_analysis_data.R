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
#'
#' @return Une liste nommée contenant :
#' \describe{
#'   \item{data_station}{Toutes les stations du lac sélectionné (filtrées par année et type de pêche)}
#'   \item{station_valides}{Sous-ensemble des stations valides (`st_valide == "O"`)}
#'   \item{station_hasard_valide}{Sous-ensemble des stations valides et au hasard (`st_valide == "O" & st_hasard == "O"`)}
#'   \item{specimen}{Spécimens de l’espèce cible associés aux stations valides et au hasard}
#'   \item{specimen_valid}{Spécimens de l’espèce cible associés à toutes les stations valides (hasard + dirigées)}
#'   \item{capture}{Table des captures par station (inclut les stations valides et au hasard, même sans capture)}
#' }
#'
#' @export
get_analysis_data <- function(path, typ_pech, no_lac, annee,
                              sheet_station = "Stations",
                              sheet_specimen = "Specimens",
                              sheet_recolte = "Recolte") {
  
  # Obtenir le code de l'espèce ciblée
  info_pen <- get_info_pen(typ_pech)
  if (is.null(info_pen)) stop("Type de pêche inconnu : aucun code espèce disponible.")
  code_sp <- info_pen$code_sp
  
  # Charger et filtrer les stations par lac, type de pêche et année
  data_station <- load_station(path, sheet_station) |>
    filtrer_par_pen_lac_annee(
      typ_pech = typ_pech,
      no_lac   = no_lac,
      annee    = annee
    )
  
  # Définir les sous-ensembles utiles de stations
  station_valides <- dplyr::filter(data_station, st_valide == "O")
  station_hasard_valide <- dplyr::filter(data_station, st_valide == "O", st_hasard == "O")
  
  # Charger les spécimens filtrés par espèce, lac, type de pêche et année
  data_specimen <- load_specimen(path, sheet_specimen) |>
    filtrer_par_pen_lac_annee(
      typ_pech = typ_pech,
      no_lac   = no_lac,
      annee    = annee
    ) |>
    dplyr::filter(sp == code_sp)
  
  # Charger les récoltes filtrées
  data_recolte <- load_recolte(path, sheet_recolte) |>
    filtrer_par_pen_lac_annee(
      typ_pech = typ_pech,
      no_lac   = no_lac,
      annee    = annee
    ) |>
    dplyr::filter(sp == code_sp)
  
  # Spécimens joints uniquement aux stations valides et au hasard
  specimen <- dplyr::left_join(
    data_specimen,
    station_hasard_valide,
    by = c("no_station", "annee", "no_lac", "typ_pech"),
    relationship = "many-to-one"
  ) |>
    dplyr::distinct() |>
    base::droplevels()
  
  # Spécimens valides (stations valides, incluant les dirigées)
  specimen_valid <- dplyr::left_join(
    data_specimen,
    station_valides,
    by = c("no_station", "annee", "no_lac", "typ_pech"),
    relationship = "many-to-one"
  ) |>
    dplyr::distinct() |>
    base::droplevels()
  
  # Captures (stations valides et au hasard, même sans capture)
  capture <- dplyr::full_join(
    station_hasard_valide,
    data_recolte,
    by = c("no_station", "annee", "no_lac", "typ_pech"),
    relationship = "one-to-many"
  ) |>
    dplyr::mutate(
      nb_capture = tidyr::replace_na(nb_capture, 0),
      nb_pese    = tidyr::replace_na(nb_pese, 0)
    ) |>
    dplyr::distinct() |>
    base::droplevels()
  
  # Retourner tous les objets nécessaires à l’analyse
  return(list(
    data_station         = data_station,
    station_valides      = station_valides,
    station_hasard_valide = station_hasard_valide,
    specimen             = specimen,
    specimen_valid       = specimen_valid,
    capture              = capture
  ))
}
