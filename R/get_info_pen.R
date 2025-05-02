#' Obtenir les informations biologiques associées à une espèce ou un type de pêche
#'
#' Cette fonction retourne les métadonnées définies dans `pen_constants` pour une espèce cible.
#' L’entrée doit être un code d’espèce (`"SANA"`, `"SAFO"`, `"SAVI"`) ou un type de pêche
#' (`"PENT"`, `"PENOF"`, `"PENDJ"`), auquel cas le code d’espèce sera automatiquement déduit.
#'
#' @return Une liste nommée contenant :
#' \describe{
#'   \item{code_sp}{Code de l’espèce (ex: `"SAFO"`)}
#'   \item{nom_sp}{Nom complet de l’espèce (ex: `"Omble de fontaine"`)}
#'   \item{binwidth}{Largeur des classes pour histogramme}
#'   \item{breaks}{Vecteur des bornes de classes}
#'   \item{break_labels}{Étiquettes associées aux classes} 
#' }
#' 
#' Retourne `NULL` si le code d’espèce est inconnu dans `pen_constants`.
#'
#' @importFrom dplyr pull filter
#' @importFrom tibble tibble
#' @examples
#' get_info_pen("SAFO")
#' get_info_pen("PENT")
#'
#' @export
get_info_pen <- function(input) {
  
  
  # --- Traduction type de pêche → code espèce ---
  mapping_typ_pech <- tibble(
    typ_pech = c("PENT", "PENOF", "PENDJ"),
    sp       = c("SANA", "SAFO", "SAVI")
  )
  
  sp_code <- if (input %in% mapping_typ_pech$typ_pech) {
    mapping_typ_pech |> filter(typ_pech == input) |> pull(sp)
  } else {
    input
  }
  
  # --- Extraction dans pen_constants ---
  info <- pen_constants |> filter(sp == sp_code)
  
  # --- Retour NULL si espèce non trouvée ---
  if (nrow(info) == 0) return(NULL)
  
  # --- Résultat structuré ---
  list(
    code_sp     = info$sp,
    nom_sp      = info$nom_sp,
    binwidth    = info$binwidth,
    breaks      = info$breaks[[1]],
    break_labels = info$break_labels[[1]]
  )
  
}
