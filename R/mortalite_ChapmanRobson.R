mortalite_ChapmanRobson <- function(pp, agemax_val, data) {
  mortalite <- agesurv(
    type = 1,
    age = data$age,
    full = pp,
    last = agemax_val,
    estimate = c("z"),
    method = c("cr")
  )
  TabZobs <-  mortalite$results
  
  TabZobs <- TabZobs   %>% mutate(Method = plyr::mapvalues(
    Method,
    from = c("cr"),
    to = c("Chapman-Robson")
  ))
  
  TabZobs <- TabZobs %>% rename("Méthode" = Method)
  TabZobs <- TabZobs %>% rename("Z" = Estimate)
  TabZobs <- TabZobs %>% dplyr::select(-c(Parameter))
  
  TabZobs
}
