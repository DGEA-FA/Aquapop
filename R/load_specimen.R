#' Charger les données du feuillet "Specimens" d'un fichier Excel (version robuste)
#'
#' Cette fonction importe, nettoie et structure les données de spécimens de poissons
#' contenues dans un fichier Excel, avec validations robustes. Les colonnes essentielles
#' sont obligatoires, les autres sont ajoutées si présentes, sinon remplacées par `NA`.
#'
#' @importFrom dplyr distinct
#' @importFrom dplyr arrange
#' @importFrom dplyr across case_when mutate
#' @importFrom lubridate year
#' @importFrom tidyr replace_na
#' @importFrom checkmate assert_subset assert_character assert_flag assert_file_exists
#' @importFrom janitor make_clean_names
#' @importFrom readxl read_excel
#' @param path Chemin vers le fichier `.xlsx` à lire. Doit contenir un feuillet nommé `"Specimens"` formaté selon les conventions AquaPop.
#' @param namesheet Nom du feuillet contenant les données des spécimens (défaut `"Specimens"`).
#' @param verbose Afficher les messages de diagnostic ? (défaut `TRUE`)
#' @param col_rename Appliquer le renommage des colonnes selon la correspondance interne ? (défaut `TRUE`)
#'
#' @return Un `data.frame` structuré avec les colonnes normalisées suivantes :
#' - **Obligatoires** : `no_lac`, `typ_pech`, `no_station`, `no_specimen`, `sp`, `ltm`, `masse`, `age`, `sexe`, `maturite`, `annee`
#' - **Optionnelles** (ajoutées comme NA si absentes) : `lf`, `marquage`, `ind_insec`, `ind_benth`, `ind_planc`, `ind_chyme`, `ind_vide`, `ind_poiss`, `poiss1`, `poiss2`, `comments_specimen`
#'
#' @details
#' - Les valeurs manquantes sont remplacées par : `"IND"` (`sexe`, `maturite`), `"NMA"` (`marquage`).
#' - Les années codées au format Excel (5 chiffres) sont converties en année réelle.
#' - Les colonnes inutiles sont ignorées.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   no_lac      = "001", typ_pech = "PE", no_station = "ST01", no_specimen = "0001",
#'   sp = "SANA", ltm = "110", masse = "15.3", age = "2", sexe = "F",
#'   maturite = "O", annee = "2022"
#' )
#' path <- tempfile(fileext = ".xlsx")
#' write_xlsx(list("Specimens" = df), path)
#' load_specimen(path)
#' }
#'
#' @export
load_specimen <- function(path,
                          namesheet = "Specimens",
                          verbose = TRUE,
                          col_rename = TRUE) {
  
  # --- Dépendances ---
  requireNamespace("readxl")
  requireNamespace("janitor")
  requireNamespace("lubridate")
  requireNamespace("dplyr")
  requireNamespace("checkmate")
  
  # --- Validation des arguments ---
  assert_file_exists(path, extension = "xlsx")
  assert_character(namesheet, len = 1)
  assert_flag(verbose)
  assert_flag(col_rename)
  
  # --- Colonnes attendues ---
  colonnes_obligatoires <- c("no_lac", "typ_pech", "no_station", "no_specimen", "sp", "ltm", "masse", "age", "sexe", "maturite", "annee", "st_valide", "st_hasard")
  colonnes_optionnelles <- c(
    "lf", "marquage", "ind_insec", "ind_benth", "ind_planc", "ind_chyme", "ind_vide", "ind_poiss",
    "poiss1", "poiss2", "comments_specimen"
  )
  toutes_colonnes <- c(colonnes_obligatoires, colonnes_optionnelles)
  colonnes_num <- c("ltm", "lf", "masse", "age", "ind_insec", "ind_benth", "ind_planc", "ind_chyme", "ind_vide", "ind_poiss")
  
  # --- Table de correspondance des noms ---
  synonymes_clean <- list(
    no_lac            = c("no_lac", "no_plan_deau", "id_lac", "numero_lce","no_plan_deau_fusionne" ),
    typ_pech          = c("typ_pech", "type_peche", "peche_type", "type", "type_de_peche"),
    no_station        = c("no_station", "station", "no_stn"),
    no_specimen       = c("no_specimen", "specimen", "id_specimen"),
    sp                = c("sp", "espece", "species", "code_espece", "espece_code" ),
    ltm               = c("ltm", "long_totale", "longueur_totale", "lt", "long_tot", "long_totale_max_mm"),
    masse             = c("masse", "poids", "poids_g", "weight", "masse_g" ),
    age               = c("age", "âge", "age1", "age_1"),
    sexe              = c("sexe", "sex"),
    maturite          = c("maturite", "maturite_sexuelle"),
    annee             = c("annee", "année", "year", "anne_debut_inventaire"),
    lf                = c("lf", "long_fourche", "longueur_fourche", "longueur_a_la_fourche_mm"),
    marquage          = c("marquage", "statut_marquage"),
    ind_insec         = c("ind_insec", "indice_insecte", "ind_insecte"),
    ind_benth         = c("ind_benth", "indice_benthos", "ind_benthos"),
    ind_planc         = c("ind_planc", "indice_plancton", "ind_plancton"),
    ind_chyme         = c("ind_chyme", "indice_chyme"),
    ind_vide          = c("ind_vide", "indice_vide"),
    ind_poiss         = c("ind_poiss", "indice_poisson", "ind_poisson"),
    poiss1            = c("poiss1", "poisson_1", "contenu_poisson_1"),
    poiss2            = c("poiss2", "poisson_2", "contenu_poisson_2"),
    comments_specimen = c("comments_specimen", "commentaires", "remarques", "notes"),
    st_valide  = c("lengin_na_pas_peche_normal", "valide"),     
    st_hasard  = c("station_choisie_au_hasard", "hasard")
  )
  
  # --- Lecture brute du fichier Excel ---
  specimen_raw <- read_excel(
    path,
    sheet = namesheet,
    col_names = TRUE,
    col_types = "text",
    na = c("", "NULL", "NA", " ", "-")
  ) |> as.data.frame()
  
  noms_originaux <- names(specimen_raw)
  noms_clean <- make_clean_names(noms_originaux)
  
  # --- Renommage via table de correspondance ---
  if (col_rename) {
    if (verbose) message("[load_specimen] Table de synonymes utilisée pour le renommage.")
    
    mapping <- sapply(names(synonymes_clean), function(canonique) {
      idx <- match(synonymes_clean[[canonique]], noms_clean)
      idx <- idx[!is.na(idx)]
      if (length(idx) > 0) {
        col <- noms_originaux[idx[1]]
        if (verbose) message("[load_specimen] Colonne ‘", col, "' reconnue comme ‘", canonique, "'.")
        return(col)
      } else {
        return(NA)
      }
    }, USE.NAMES = TRUE)
    
    col_mapping_valid <- !is.na(mapping[colonnes_obligatoires])
    if (!all(col_mapping_valid)) {
      col_absentes <- names(mapping[colonnes_obligatoires])[!col_mapping_valid]
      stop("Colonnes obligatoires manquantes : ", paste(col_absentes, collapse = ", "))
    }
    
    n <- nrow(specimen_raw)
    specimen <- as.data.frame(matrix(NA, nrow = n, ncol = 0))
    for (col in names(mapping)) {
      col_source <- mapping[[col]]
      if (!is.na(col_source)) {
        specimen[[col]] <- specimen_raw[[col_source]]
      } else {
        valeur_na <- if (col %in% colonnes_num) NA_real_ else NA_character_
        specimen[[col]] <- rep(valeur_na, n)
        if (verbose) message("[load_specimen] Colonne ‘", col, "' absente, ajoutée comme NA.")
      }
    }
    
  } else {
    noms_clean_direct <- make_clean_names(names(specimen_raw))
    canoniques_clean <- sapply(synonymes_clean[colonnes_obligatoires], `[[`, 1)
    assert_subset(canoniques_clean, noms_clean_direct,
                             .var.name = "Colonnes obligatoires manquantes")
    specimen <- specimen_raw
  }
  
  # --- Nettoyage des statuts ---
  specimen <- specimen |>
    mutate(
      st_valide = case_when(is.na(st_valide) | st_valide %in% c("IND", "-", "VALIDE") ~ "O", TRUE ~ st_valide),
      st_hasard = case_when(is.na(st_hasard) | st_hasard %in% c("IND", "-") ~ "O", TRUE ~ st_hasard)
    )
  
  # --- Nettoyage et conversion des colonnes ---
  specimen <- specimen |>
    mutate(
      maturite = replace_na(maturite, "IND"),
      sexe     = replace_na(sexe, "IND"),
      marquage = replace_na(marquage, "NMA"),
      
      annee = case_when(
        nchar(annee) == 5 ~ as.integer(year(as.Date(as.numeric(annee), origin = "1899-12-30"))),
        TRUE              ~ suppressWarnings(as.integer(annee))
      ),
      
      sexe     = factor(sexe, levels = c("F", "M", "IND")),
      maturite = factor(maturite, levels = c("O", "N", "IND")),
      marquage = factor(marquage, levels = c("MA", "NMA")),
      
      across(
        intersect(c("no_lac", "typ_pech", "no_station", "st_hasard", "st_valide", "no_specimen", "sp", 
                    "ind_insec", "ind_benth", "ind_planc", "ind_chyme", "ind_vide", "ind_poiss",
                    "poiss1", "poiss2"), names(specimen)),
        as.factor
      ),
      
      across(
        intersect(c("ltm", "lf", "masse", "age"), names(specimen)),
        as.numeric
      ),
      
      comments_specimen = if ("comments_specimen" %in% names(specimen)) {
        as.character(comments_specimen)
      } else {
        rep(NA_character_, nrow(specimen))
      }
    ) |>
    arrange(across("no_specimen")) |>
    distinct()
  
  return(specimen)
}
