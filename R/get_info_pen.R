#' Obtenir les infos associées au type de pêche sélectionné
#'
#' @param input_typ_pech Code du type de pêche (ex: "PENT", "PENOF", "PENDJ")
#'
#' @return Une liste contenant code_sp, nom_sp et binwidth, ou NULL si le type de pêche est inconnu.
#' @export
get_info_pen <- function(input_typ_pech) {
  pen_info <- tibble::tibble(
    typ_pech = c("PENT", "PENOF", "PENDJ"),
    code_sp  = c("SANA", "SAFO", "SAVI"),
    nom_sp   = c("touladis", "ombles de fontaine", "dorés jaunes"),
    binwidth = c(50, 20, 50)
  )
  
  info <- pen_info %>%
    dplyr::filter(typ_pech == input_typ_pech)
  
  if (nrow(info) == 0) return(NULL)
  
  list(
    code_sp  = info$code_sp,
    nom_sp   = info$nom_sp,
    binwidth = info$binwidth
  )
}
