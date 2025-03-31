#' Préparer les jeux de données pour l'analyse
#'
#' Cette fonction importe, nettoie, filtre et fusionne les données de stations,
#' de spécimens et de récoltes à partir d'un fichier Excel structuré. Elle retourne
#' les objets requis pour les analyses biologiques de l'application AquaPop.
#'
#' @param path Chemin complet vers le fichier Excel (.xlsx) à importer
#' @param typ_pech Type de pêche sélectionné (ex: "PENDJ")
#' @param no_lac Numéro du lac sélectionné (ex: "00045")
#' @param annee Année sélectionnée (ex: 2022)
#' @param sheet_station Nom du feuillet des stations (défaut = `"Stations"`)
#' @param sheet_specimen Nom du feuillet des spécimens (défaut = `"Specimens"`)
#' @param sheet_recolte Nom du feuillet des récoltes (défaut = `"Recolte"`)
#'
#' @return Une liste nommée contenant :
#' \describe{
#'   \item{data_station}{Stations filtrées pour le lac, type de pêche et année}
#'   \item{specimen}{Spécimens de l’espèce ciblée associés aux stations valides et au hasard}
#'   \item{specimen_valid}{Spécimens de l’espèce ciblée associés à toutes les stations valides (hasard + dirigées)}
#'   \item{capture}{Table des captures de l’espèce ciblée par station, incluant les stations sans capture (0)}
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
  
  # Charger et filtrer les stations
  data_station <- load_station(path, sheet_station) |>
    filtrer_par_pen_lac_annee(
      typ_pech = typ_pech,
      no_lac   = no_lac,
      annee    = annee
    )
  
  # Charger les spécimens et récoltes filtrés par espèce
  data_specimen <- load_specimen(path, sheet_specimen) |>
    filtrer_par_pen_lac_annee(
      typ_pech = typ_pech,
      no_lac   = no_lac,
      annee    = annee
    ) |>
    dplyr::filter(sp == code_sp)
  
  data_recolte <- load_recolte(path, sheet_recolte) |>
    filtrer_par_pen_lac_annee(
      typ_pech = typ_pech,
      no_lac   = no_lac,
      annee    = annee
    )|>
    dplyr::filter(sp == code_sp)
  
  # Spécimens : joindre aux stations valides et au hasard
  specimen <- dplyr::left_join(
    data_specimen,
    data_station |> dplyr::filter(st_valide == "O", st_hasard == "O"),
    by = c("no_station", "annee", "no_lac", "typ_pech"),
    relationship = "many-to-one"
  ) |>
    dplyr::distinct() |>
    base::droplevels()
  
  # Spécimens valides : joindre aux stations valides (hasard + dirigées)
  specimen_valid <- dplyr::left_join(
    data_specimen,
    data_station |> dplyr::filter(st_valide == "O"),
    by = c("no_station", "annee", "no_lac", "typ_pech"),
    relationship = "many-to-one"
  ) |>
    dplyr::distinct() |>
    base::droplevels()
  
  # Captures : full_join avec les stations valides et au hasard
  capture <- dplyr::full_join(
    data_station |> dplyr::filter(st_valide == "O", st_hasard == "O"),
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
  
  # Retourner les objets
  return(list(
    data_station   = data_station,
    specimen       = specimen,
    specimen_valid = specimen_valid,
    capture        = capture
  ))
}
