#' Charger et structurer les données du feuillet "Récolte" (version robuste)
#'
#' Cette fonction importe les données brutes du feuillet "Récolte" d'un fichier Excel
#' formaté selon les standards AquaPop. Elle applique des transformations pour nettoyer
#' les colonnes, corriger les années (Excel ou texte), et produire un tableau structuré.
#'
#' @importFrom dplyr distinct
#' @importFrom dplyr across
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom checkmate assert_names
#' @importFrom checkmate assert_flag
#' @importFrom checkmate assert_character
#' @param path Chemin complet vers le fichier Excel (.xlsx) à importer.
#' @param namesheet Nom du feuillet contenant les données de récolte (défaut : "Recolte").
#' @param verbose Afficher les messages de transformation ? (défaut : TRUE)
#'
#' @return Un `data.frame` structuré contenant :
#' \describe{
#'   \item{no_lac, typ_pech, no_station, sp}{Facteurs}
#'   \item{annee}{Numérique (entier), année de récolte}
#'   \item{nb_capture, nb_pese}{Numériques}
#'   \item{comments_recolte}{Texte, si présente}
#' }
#'
#' @details
#' Colonnes obligatoires attendues : `no_lac`, `typ_pech`, `no_station`, `sp`,
#' `annee`, `nb_capture`, `nb_pese`.  
#' Colonnes optionnelles : `comments`, qui sera renommée `comments_recolte` si présente.
#'
#' Les colonnes peuvent apparaître dans n'importe quel ordre. La colonne `nom_lac`, si présente, est supprimée.
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr mutate select distinct across any_of
#' @importFrom lubridate year
#' @importFrom checkmate assert_file_exists assert_character assert_flag assert_names
#' @importFrom janitor make_clean_names
#' @export
load_recolte <- function(path,
                         namesheet = "Recolte",
                         verbose = TRUE) {
  
  # --- Validation des arguments ---
  assert_file_exists(path, extension = "xlsx")
  assert_character(namesheet, len = 1)
  assert_flag(verbose)
  
  # --- Lecture brute du fichier Excel ---
  recolte_raw <- read_excel(
    path,
    sheet = namesheet,
    col_names = TRUE,
    col_types = "text",
    na = c("", "NULL", "NA", " ", "-")
  ) |> as.data.frame()
  
  # --- Table de synonymes avec noms déjà clean_names() ---
  synonymes_clean <- list(
    no_lac     = c("no_lac", "numero_lce", "id_lac", "no_plan_deau" ),
    nom_lac    = c("nom_lac", "nom_du_lac", "nom_plan_d_eau",  "nom_plan_deau"),
    typ_pech   = c("typ_pech", "type_peche", "type_de_peche" ),
    annee      = c("annee", "year"),
    no_station = c("no_station", "station", "no_stn" ),
    sp         = c("sp", "espece"),
    nb_capture = c("nb_capture", "n_captur", "nbre_capture", "nbre_capturee"),
    nb_pese    = c("nb_pese", "n_pese", "nbre_pese", "nbre_pesee", "nbre_pesé"),
    comments   = c("comments", "commentaires", "remarques", "notes"),
    st_valide  = c("lengin_na_pas_peche_normal", "valide"),     
    st_hasard  = c("station_choisie_au_hasard", "hasard")
  )
  
  # --- Nettoyage des noms des colonnes du fichier ---
  noms_originaux <- names(recolte_raw)
  noms_clean <- make_clean_names(noms_originaux)
  
  # --- Création du mapping canonique → nom original ---
  mapping <- sapply(names(synonymes_clean), function(canonique) {
    idx <- match(synonymes_clean[[canonique]], noms_clean)
    idx <- idx[!is.na(idx)]
    if (length(idx) > 0) {
      return(noms_originaux[idx[1]])
    } else {
      return(NA)
    }
  }, USE.NAMES = TRUE)
  
  # --- Construction du tableau avec noms canoniques ---
  n <- nrow(recolte_raw)
  recolte <- as.data.frame(matrix(NA_character_, nrow = n, ncol = 0))
  for (col in names(mapping)) {
    col_source <- mapping[[col]]
    if (!is.na(col_source)) {
      recolte[[col]] <- recolte_raw[[col_source]]
      if (verbose) message("[load_recolte] Colonne ‘", col_source, "' reconnue comme ‘", col, "'.")
    }
  }
  
  # --- Validation des colonnes essentielles ---
  colonnes_obligatoires <- c("no_lac", "typ_pech", "annee", "no_station", "sp", "nb_capture", "nb_pese", "st_valide", "st_hasard")
  assert_names(names(recolte), must.include = colonnes_obligatoires)
  
  # --- Suppression optionnelle de nom_lac ---
  if ("nom_lac" %in% names(recolte)) {
    recolte <- select(recolte, -"nom_lac")
    if (verbose) message("[load_recolte] Colonne ‘nom_lac' supprimée.")
  }
  
  # --- Conversion de l'année Excel ou texte ---
  recolte$annee <- case_when(
    nchar(recolte$annee) == 5 ~ as.integer(year(as.Date(as.numeric(recolte$annee), origin = "1899-12-30"))),
    TRUE                      ~ suppressWarnings(as.integer(recolte$annee))
  )
  
  # --- Nettoyage des statuts ---
  recolte <- recolte |>
    mutate(
      st_valide = case_when(is.na(.data$st_valide) | .data$st_valide %in% c("IND", "-", "VALIDE") ~ "O", TRUE ~ .data$st_valide),
      st_hasard = case_when(is.na(.data$st_hasard) | .data$st_hasard %in% c("IND", "-") ~ "O", TRUE ~ .data$st_hasard)
    )
  
  # --- Conversion des types ---
  recolte <- recolte |>
    mutate(
      across(c("no_lac", "typ_pech", "no_station", "sp", "st_valide", "st_hasard"), as.factor),
      across(c("nb_capture", "nb_pese"), as.numeric),
      comments_recolte = if ("comments" %in% names(recolte)) as.character(recolte$comments) else NA_character_
    ) |>
    select(-any_of("comments"))
  

  return(recolte)
}
