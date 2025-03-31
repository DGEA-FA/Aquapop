#' Créer la table des captures combinant stations et récoltes
#'
#' Cette fonction effectue une jointure entre les stations valides et hasardeuses
#' et les données de récolte correspondantes, puis nettoie les colonnes redondantes
#' ou conflictuelles, et remplace les valeurs manquantes par 0 dans les colonnes
#' numériques de capture.
#' Créer une table des captures par station (valide et aléatoire)
#'
#' Cette fonction combine les informations des stations et des récoltes pour générer 
#' un tableau de captures structuré, où chaque ligne correspond à une combinaison 
#' unique de station et espèce, pour une année donnée. 
#' Seules les stations considérées comme valides (`st_valide == "O"`) et 
#' sélectionnées aléatoirement (`st_hasard == "O"`) sont prises en compte.
#' 
#' La fonction applique une jointure complète (`full_join`) entre `data_station` 
#' et `data_recolte` sur les colonnes : `no_station`, `annee`, `no_lac`, `typ_pech`.
#' 
#' 🔄 Comportements spécifiques :
#' - Si une station valide n'a aucune capture associée, elle apparaîtra quand même dans 
#'   le tableau final avec `nb_capture = 0` et `nb_pese = 0`.
#' - Si plusieurs espèces ont été capturées à la même station, il y aura une ligne 
#'   par espèce (`sp`) pour cette station.
#' - Si les colonnes `nom_lac` ou `comments` sont présentes dans les deux jeux de données, 
#'   elles sont comparées et/ou renommées pour éviter les conflits.
#' 
#' 📥 Entrées :
#' @param data_station `data.frame` contenant les métadonnées des stations, dont :
#'   - `no_station`, `annee`, `no_lac`, `typ_pech`, `nom_lac`
#'   - `st_valide`, `st_hasard` : filtres appliqués pour limiter aux stations retenues
#'   - autres variables descriptives conservées dans la jointure
#' 
#' @param data_recolte `data.frame` contenant les données de captures, dont :
#'   - `no_station`, `annee`, `no_lac`, `typ_pech`, `sp`
#'   - `nb_capture`, `nb_pese`, `comments` (commentaires récolte)
#' 
#' 🧾 Sortie :
#' @return Un `data.frame` combiné comprenant les informations des stations valides et 
#' aléatoires, enrichies des données de captures (même si absentes).
#' 
#' Le tableau de sortie inclut :
#' - toutes les colonnes de `data_station` pour les stations retenues
#' - les colonnes de `data_recolte` si disponibles
#' - `nb_capture` et `nb_pese` mis à zéro si absents (station sans capture)
#' - une ligne par station x espèce si plusieurs espèces capturées
#' - colonnes `nom_lac` et `comments` harmonisées si en double
#'
#' @examples
#' capture <- create_capture(data_station, data_recolte)
#' 
#' @export
create_capture <- function(data_station, data_recolte) {
  
  # 1. Filtrer les stations valides et hasardeuses, puis faire une jointure complète
  capture <- dplyr::full_join(
    x = dplyr::filter(data_station, st_valide == "O", st_hasard == "O"),
    y = data_recolte,
    by = c("no_station", "annee", "no_lac", "typ_pech"),
    relationship = "one-to-many"
  ) |>
    base::droplevels() |>
    dplyr::distinct()
  
  # 2. Harmoniser la colonne "nom_lac" en cas de présence dans les deux tables
  if ("nom_lac.x" %in% names(capture) && "nom_lac.y" %in% names(capture)) {
    capture <- capture |>
      dplyr::mutate(
        nom_lac = dplyr::if_else(nom_lac.x != nom_lac.y, nom_lac.x, nom_lac.y)
      ) |>
      dplyr::select(-nom_lac.x, -nom_lac.y)
  }
  
  # 3. Renommer les colonnes "comments" si elles existent dans les deux jeux de données
  if ("comments.x" %in% names(capture) && "comments.y" %in% names(capture)) {
    capture <- capture |>
      dplyr::rename(
        comments_station = comments.x,
        comments_recolte = comments.y
      )
  }
  
  # 4. Remplacer les valeurs manquantes de captures par 0 (aucune capture)
  capture <- capture |>
    dplyr::mutate(
      nb_capture = tidyr::replace_na(nb_capture, 0),
      nb_pese    = tidyr::replace_na(nb_pese, 0)
    )
  
  return(capture)
}
