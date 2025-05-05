#' Obtenir les métadonnées biologiques d'une espèce ou d'un type de pêche
#'
#' Cette fonction retourne les métadonnées associées à une espèce cible, à partir
#' du tableau `pen_constants`. Elle accepte soit un code d’espèce (`"SANA"`, `"SAFO"`, `"SAVI"`),
#' soit un type de pêche (`"PENT"`, `"PENOF"`, `"PENDJ"`), auquel cas le code espèce est automatiquement déduit.
#'
#' @param input Un code d’espèce (ex: `"SANA"`) ou un type de pêche (ex: `"PENT"`), au format `character(1)`.
#' @return Une liste nommée contenant :
#' \describe{
#'   \item{code_sp}{Code de l’espèce (ex: `"SAFO"`)}
#'   \item{nom_sp}{Nom complet de l’espèce (ex: `"Omble de fontaine"`)}
#'   \item{binwidth}{Largeur des classes pour les histogrammes}
#'   \item{breaks}{Vecteur des bornes de classes numériques}
#'   \item{break_labels}{Étiquettes textuelles associées aux bornes}
#' }
#' Retourne `NULL` si l’entrée est invalide ou inconnue dans `pen_constants`.
#'
#' @details
#' Le tableau `pen_constants` est une table interne (data.frame ou tibble)
#' contenant les métadonnées des espèces ciblées. Il doit inclure les colonnes :
#' `sp`, `nom_sp`, `binwidth`, `breaks`, `break_labels`.
#'
#' @importFrom dplyr filter pull
#' @importFrom tibble tibble
#' @export
get_info_pen <- function(input) {
  # --- Validation de l’entrée ---
  if (missing(input) || !is.character(input) || length(input) != 1 || is.na(input) || input == "") {
    stop("`input` doit être une chaîne de caractères non vide représentant un code d'espèce ou un type de pêche.")
  }
  
  # --- Traduction typ_pech → code d’espèce ---
  mapping_typ_pech <- tibble(
    typ_pech = c("PENT", "PENOF", "PENDJ"),
    sp       = c("SANA", "SAFO", "SAVI")
  )
  
  sp_code <- if (input %in% mapping_typ_pech$typ_pech) {
    mapping_typ_pech |> filter(typ_pech == input) |> pull(sp)
  } else {
    input
  }
  
  # --- Extraction dans le tableau pen_constants ---
  info_sp <- pen_constants |> filter(sp == sp_code)
  
  if (nrow(info_sp) == 0) return(NULL)
  
  # --- Construction de la liste de sortie ---
  return(list(
    code_sp      = info_sp$sp,
    nom_sp       = info_sp$nom_sp,
    binwidth     = info_sp$binwidth,
    breaks       = info_sp$breaks[[1]],
    break_labels = info_sp$break_labels[[1]]
  ))
}
