#' Charger les données du feuillet "Lac" d'un fichier Excel (version robuste)
#'
#' Cette fonction importe, nettoie et structure les données associées aux lacs contenues
#' dans un fichier Excel, avec validations robustes et souples. Les colonnes
#' essentielles sont obligatoires, les autres sont facultatives et ajoutées si présentes.
#'
#' @param path Chemin vers le fichier `.xlsx` à lire.
#' @param namesheet Nom du feuillet contenant les données des lacs (défaut `"Lac"`).
#' @param verbose Afficher les messages de diagnostic ? (défaut `TRUE`)
#' @param col_rename Appliquer le renommage des colonnes selon la correspondance interne ? (défaut `TRUE`)
#'
#' @return Un `data.frame` structuré avec identifiant unique (`ID`) et colonnes normalisées.
#'
#' @details
#' Colonnes **obligatoires** : `no_lac`, `nom_lac`, `typ_pech`, `annee`
#'
#' Colonnes **optionnelles** (ajoutées comme `NA` si absentes) :
#' `region_admin`, `sp_pen`, `long_dd.dec`, `lat_dd.dec`, `terr_faun`, `zon_pech`,
#' `superficie_ha`, `perimetre_km`, `prof_max_m`, `prof_moy_m`, `comments`
#'
#' La fonction reconnaît également des **variantes courantes** de ces noms grâce à une table
#' de synonymes nettoyés via `make_clean_names()`.
#'
#' @importFrom readxl read_excel
#' @importFrom janitor make_clean_names
#' @importFrom lubridate year
#' @importFrom dplyr mutate across distinct case_when
#' @importFrom checkmate assert_file_exists assert_character assert_flag assert_subset
#' @examples
#' # Exemple minimal avec un fichier Excel temporaire
#' df <- data.frame(
#'   "Nom du lac" = "Lac Test",
#'   "Numéro LCE" = "00123",
#'   "Type pêche" = "PE",
#'   "Année" = "2022"
#' )
#' path <- tempfile(fileext = ".xlsx")
#' write_xlsx(list("Lac" = df), path)
#' load_lac(path)
#'
#' @export
load_lac <- function(path,
                     namesheet = "Lac",
                     verbose = TRUE,
                     col_rename = TRUE) {
  
  # Validation des arguments ----
  assert_file_exists(path, extension = "xlsx")
  assert_character(namesheet, len = 1)
  assert_flag(verbose)
  assert_flag(col_rename)
  
  # Définition des colonnes attendues ----
  colonnes_obligatoires <- c("no_lac", "nom_lac", "typ_pech", "annee")
  colonnes_optionnelles <- c(
    "region_admin", "sp_pen", "long_dd.dec", "lat_dd.dec", "terr_faun", "zon_pech",
    "superficie_ha", "perimetre_km", "prof_max_m", "prof_moy_m", "comments"
  )
  toutes_colonnes <- c(colonnes_obligatoires, colonnes_optionnelles)
  
  # Table de correspondance des noms (synonymes) ----
  synonymes_clean <- list(
    no_lac        = c("no_lac", "id_lac", "lac_id", "no_lce", "numero_lce", "no_plan_d_eau", "no_plan_deau"),
    nom_lac       = c("nom_lac", "nom_du_lac", "lac", "lake_name", "nom_lce", "nom_plan_d_eau", "nom_plan_deau"),
    typ_pech      = c("typ_pech", "type_peche", "peche_type", "type", "type_peche", "type_de_peche"),
    annee         = c("annee", "annee_peche", "year"),
    region_admin  = c("region_admin", "region", "reg_admin", "code_region", "region_administrative"),
    sp_pen        = c("sp_pen", "code_peche", "code_methode", "sp_methode", "espece_visee_code"),
    long_dd.dec   = c("long_dd_dec", "longitude", "lon", "x", "long", "long_dec", "longitude_dd_dec_", "longitude_dd_dec"),
    lat_dd.dec    = c("lat_dd_dec", "latitude", "lat", "y", "lat_dec", "latitude_dd_dec_", "latitude_dd_dec"),
    terr_faun     = c("terr_faun", "territoire", "territoire_faunique", "code_terr_faun"),
    zon_pech      = c("zon_pech", "zone_peche", "zone", "zone_peche", "code_zone", "zone_de_peche"),
    superficie_ha = c("superficie_ha", "area_ha", "sup_ha", "superficie", "surface_ha", "surface"),
    perimetre_km  = c("perimetre_km", "perim_km", "perimetre"),
    prof_max_m    = c("prof_max_m", "profondeur_max", "zmax", "prof_maximale", "profond_max"),
    prof_moy_m    = c("prof_moy_m", "profondeur_moy", "zmean", "prof_moyenne", "profond_moy"),
    comments      = c("comments", "commentaires", "remarques", "notes", "observations", "commentaires_generaux")
  )
  
  # Lecture brute du fichier Excel ----
  lac_raw <- read_excel(
    path,
    sheet = namesheet,
    col_names = TRUE,
    col_types = "text",
    na = c("", "NULL", "NA", " ", "-")
  ) |> as.data.frame()
  
  noms_originaux <- names(lac_raw)
  noms_clean <- make_clean_names(noms_originaux)
  
  # Renommage des colonnes via la table de synonymes ----
  if (col_rename) {
    if (verbose) message("[load_lac] Table de synonymes utilisée pour le renommage.")
    
    # Création du mapping canonique → colonne source
    mapping <- sapply(names(synonymes_clean), function(canonique) {
      idx <- match(synonymes_clean[[canonique]], noms_clean)
      idx <- idx[!is.na(idx)]
      if (length(idx) > 0) {
        col <- noms_originaux[idx[1]]
        if (verbose) message("[load_lac] Colonne ‘", col, "’ reconnue comme ‘", canonique, "’.")
        return(col)
      } else {
        return(NA)
      }
    }, USE.NAMES = TRUE)
    
    # Validation des colonnes obligatoires ----
    col_mapping_valid <- !is.na(mapping[colonnes_obligatoires])
    if (!all(col_mapping_valid)) {
      col_absentes <- names(mapping[colonnes_obligatoires])[!col_mapping_valid]
      stop("Colonnes obligatoires manquantes : ", paste(col_absentes, collapse = ", "))
    }
    
    # Construction du tableau nettoyé ----
    n <- nrow(lac_raw)
    lac <- as.data.frame(matrix(NA_character_, nrow = n, ncol = 0))
    for (col in names(mapping)) {
      col_source <- mapping[[col]]
      if (!is.na(col_source)) {
        lac[[col]] <- lac_raw[[col_source]]
      } else {
        lac[[col]] <- rep(NA_character_, n)
        if (verbose) message("[load_lac] Colonne ‘", col, "’ absente, ajoutée comme NA.")
      }
    }
    
  } else {
    # Vérification sans renommage ----
    noms_clean_direct <- make_clean_names(names(lac_raw))
    canoniques_clean <- sapply(synonymes_clean[colonnes_obligatoires], `[[`, 1)
    assert_subset(canoniques_clean, noms_clean_direct,
                             .var.name = "Colonnes obligatoires manquantes")
    
    lac <- lac_raw
  }
  
  # Conversion des types ----
  lac <- lac |>
    mutate(
      annee = case_when(
        nchar(annee) == 5 ~ as.integer(year(as.Date(as.numeric(annee), origin = "1899-12-30"))),
        TRUE              ~ suppressWarnings(as.integer(annee))
      ),
      across(
        intersect(c("region_admin", "no_lac", "nom_lac", "typ_pech", "sp_pen", "terr_faun", "zon_pech"), names(lac)),
        as.factor
      ),
      across(
        intersect(c("long_dd.dec", "lat_dd.dec", "superficie_ha", "perimetre_km", "prof_max_m", "prof_moy_m"), names(lac)),
        as.numeric
      ),
      comments = if ("comments" %in% names(lac)) as.character(comments) else NA_character_
    )
  
  # Création de l'identifiant unique + dédoublonnage ----
  lac <- lac |>
    mutate(ID = paste0(nom_lac, " - ", annee, " - ", typ_pech)) |>
    distinct()
  
  return(lac)
}
