#' Charger les données du feuillet "Lac" d'un fichier Excel
#'
#' Cette fonction importe, nettoie et structure les données lacustres contenues
#' dans un fichier Excel, selon une structure attendue. Elle est utilisée pour
#' alimenter les modules de l'application AquaPop.
#'
#' @param path Chemin vers le fichier `.xlsx` à lire.
#' @param namesheet Nom du feuillet contenant les données des lacs (par défaut `"Lac"`).
#'
#' @return Un `data.frame` contenant les informations nettoyées sur les lacs :
#' \describe{
#'   \item{region_admin, no_lac, nom_lac, typ_pech, sp_pen, terr_faun, zon_pech}{variables catégorielles (facteurs)}
#'   \item{annee}{année de pêche, convertie en entier}
#'   \item{long_dd.dec, lat_dd.dec, superficie_ha, perimetre_km, prof_max_m, prof_moy_m}{variables numériques}
#'   \item{comments}{chaîne de caractères}
#'   \item{ID}{identifiant unique de l’entrée, concaténant nom de lac, année et type de pêche}
#' }
#'
#' @details
#' La fonction effectue les étapes suivantes :
#' \enumerate{
#'   \item Lecture du fichier Excel avec `readxl::read_excel()`
#'   \item Attribution des noms de colonnes standardisés
#'   \item Conversion robuste de l'année au format entier (y compris formats Excel)
#'   \item Transformation des types de colonnes selon leur nature
#'   \item Création d’un identifiant unique
#'   \item Suppression des doublons
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom lubridate year
#' @importFrom dplyr mutate across distinct case_when
#' @export
load_lac <- function(path, namesheet = "Lac") {
  lac <- readxl::read_excel(
    path,
    col_names = TRUE,
    sheet = namesheet,
    na = c("", "NULL", "NA", " ", "-"),
    col_types = "text"
  ) %>%
    as.data.frame()
  
  # Renommer les colonnes
  colnames(lac) <- c(
    "region_admin", "no_lac", "nom_lac", "typ_pech", "annee",
    "sp_pen", "long_dd.dec", "lat_dd.dec", "terr_faun", "zon_pech",
    "superficie_ha", "perimetre_km", "prof_max_m", "prof_moy_m", "comments"
  )
  
  # Conversion des types + gestion robuste de l’année
  lac <- lac %>%
    dplyr::mutate(
      annee = dplyr::case_when(
        nchar(annee) == 5 ~ as.integer(lubridate::year(as.Date(as.numeric(annee), origin = "1899-12-30"))),
        TRUE              ~ suppressWarnings(as.integer(annee))
      ),
      across(
        c(region_admin, no_lac, nom_lac, typ_pech, sp_pen, terr_faun, zon_pech),
        as.factor
      ),
      across(
        c(long_dd.dec, lat_dd.dec, superficie_ha, perimetre_km, prof_max_m, prof_moy_m),
        as.numeric
      ),
      comments = as.character(comments)
    )
  
  # Créer un identifiant unique
  lac <- lac %>%
    dplyr::mutate(ID = paste0(nom_lac, " - ", annee, " - ", typ_pech))
  
  # Supprimer les doublons
  lac <- lac %>% dplyr::distinct()
  
  return(lac)
}
