create_sp_pen <- function(input_typ_pech) {
  if (input_typ_pech == "PENT") {
    return("SANA")
  } else if (input_typ_pech == "PENOF") {
    return("SAFO")
  } else if (input_typ_pech == "PENDJ") {
    return("SAVI")
  } else {
    return(NULL)
  }
}