#' Obtenir les informations associées à un code d’espèce ou un type de pêche
#'
#' @param input Soit un code d’espèce (`"SANA"`, `"SAFO"`, etc.), soit un type de pêche (`"PENT"`, etc.)
#'
#' @return Une liste contenant `sp`, `nom_sp`, `binwidth`, et `breaks`, ou `NULL` si inconnu.
#' @export
get_info_pen <- function(input) {
  mapping_typ_pech <- tibble::tibble(
    typ_pech = c("PENT", "PENOF", "PENDJ"),
    sp       = c("SANA", "SAFO", "SAVI")
  )
  
  # Déduire le code d’espèce si l’entrée est un type de pêche
  sp_code <- if (input %in% mapping_typ_pech$typ_pech) {
    mapping_typ_pech %>% filter(typ_pech == input) %>% pull(sp)
  } else {
    input
  }
  
  info <- pen_constants %>% filter(sp == sp_code)
  
  if (nrow(info) == 0) return(NULL)
  
  list(
    code_sp       = info$sp,
    nom_sp   = info$nom_sp,
    binwidth = info$binwidth,
    breaks   = info$breaks[[1]]
  )
}
