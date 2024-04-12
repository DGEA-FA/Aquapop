mortalite_ChapmanRobson <- function(pp, agemax_val, data) {
  mortalite <- fishmethods::agesurv(
    type = 1,
    age = data$age,
    full = pp,
    last = agemax_val,
    estimate = c("z"),
    method = c("cr")
  )
  TabZobs <-  mortalite$results

  
  TabZobs <- TabZobs %>% rename("Méthode" = Method)
  TabZobs <- TabZobs %>% rename("Z" = Estimate)
  TabZobs <- TabZobs %>% dplyr::select(-c(Parameter))
  
  
  #ajouter A
  TabZobs <-
    TabZobs %>% mutate("A" = round((1 - exp(-Z)) * 100, digits = 1))
  TabZobs <- TabZobs %>% mutate("lowerZ" = Z - SE,
                              "highZ" = Z + SE)
  TabZobs <-
    TabZobs %>% mutate("lowerA" = round((1 - exp(-lowerZ)) * 100, digits = 1),
                      "Ahigh" = round((1 - exp(-highZ)) * 100, digits = 1))
  TabZobs <- TabZobs %>% mutate("IC 95%" = glue("[{lowerA}-{Ahigh}]"))
  TabZobs <- TabZobs %>% dplyr::select(-c("lowerA", "Ahigh", "lowerZ", "highZ"))
  
  
  # mortalite <- fishmethods::agesurv(
  #   type = 1,
  #   age = data$age,
  #   full = pp,
  #   last = agemax_val,
  #   estimate = c("s"), #"s" for annual survival
  #   method = c("cr")
  # )
  # TabZobs2 <-  mortalite$results
  # TabZobs2 <- TabZobs2 %>% rename("A" = Estimate)
  # TabZobs2 <- TabZobs2 %>% dplyr::select(-c(Parameter,Method))
  # 
  # CLEAN <- cbind(TabZobs,TabZobs2)
  # CLEAN
}
