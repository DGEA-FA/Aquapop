get_zobs <- function(PP, death, agemax) {
  #Selon pepino
  mortalite <-
    agesurv(
      type = 1,
      age = death$age,
      full = PP,
      last = agemax,
      estimate = c("z"),
      method = c("he", "lr", "wlr", "cr", "crcb", "pois")
    )
  TabZobs <-  mortalite$results
  Zobs <-   mortalite$results[4, 3]
  
  Zobs
  
}
