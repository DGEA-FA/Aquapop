#' Charger et structurer les données du feuillet "Récolte"
#'
#' Cette fonction importe les données brutes du feuillet "Récolte" d’un fichier Excel,
#' applique des transformations pour nettoyer et convertir les colonnes au bon format,
#' puis retourne un tableau prêt à l’analyse.
#'
#' @param path Chemin complet vers le fichier Excel (.xlsx) à importer.
#' @param namesheet Nom du feuillet contenant les données de récolte (par défaut `"Recolte"`).
#'
#' @return Un `data.frame` contenant :
#' \describe{
#'   \item{no_lac, nom_lac, typ_pech, no_station, sp}{Facteurs}
#'   \item{annee}{Année numérique, convertie à partir du format Excel si nécessaire}
#'   \item{nb_capture, nb_pese}{Numériques}
#'   \item{comments}{Chaîne de caractères}
#' }
#'
#' @details
#' La fonction effectue les étapes suivantes :
#' \enumerate{
#'   \item Lecture du fichier Excel avec toutes les colonnes en texte
#'   \item Renommage des colonnes selon le format attendu
#'   \item Conversion des colonnes en types appropriés (facteurs, numériques, caractères)
#'   \item Conversion de l’année au format Excel si nécessaire
#'   \item Suppression des doublons
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom lubridate year
#' @importFrom dplyr mutate distinct
#' @export
load_recolte <- function(path, namesheet) {
  
  # 1. Lecture du fichier Excel
  recolte <- readxl::read_excel(
    path,
    col_names = TRUE,
    sheet = namesheet,
    na = c("", "NULL", "NA", " ", "-"),
    col_types = "text"
  ) %>%
    as.data.frame()
  
  # 2. Renommage des colonnes
  colnames(recolte) <- c(
    'no_lac',        # 1ère colonne : No plan d'eau
    'nom_lac',       # 2ème colonne : Nom plan d'eau
    'typ_pech',      # 3ème colonne : Type de pêche
    'annee',         # 4ème colonne : Année
    'no_station',    # 5ème colonne : No station
    'sp',            # 6ème colonne : Espèce
    'nb_capture',    # 7ème colonne : Nbre capturé
    'nb_pese',       # 8ème colonne : Nbre pesé
    'comments'       # 9ème colonne : Commentaires
  )
  
  # 3. Conversion de l'année (Excel → entier si nécessaire)
  recolte$annee <- as.integer(recolte$annee)
  recolte$annee <- ifelse(
    nchar(recolte$annee) == 5,
    as.integer(lubridate::year(as.Date(recolte$annee, origin = "1899-12-30"))),
    recolte$annee
  )
  
  # 4. Conversion des types
  recolte <- recolte %>%
    dplyr::mutate(
      no_lac     = as.factor(no_lac),
      nom_lac    = as.factor(nom_lac),
      typ_pech   = as.factor(typ_pech),
      no_station = as.factor(no_station),
      sp         = as.factor(sp),
      nb_capture = as.numeric(nb_capture),
      nb_pese    = as.numeric(nb_pese),
      comments   = as.character(comments)
    )
  
  # 5. Suppression des doublons
  recolte <- recolte %>% dplyr::distinct()
  
  return(recolte)
}