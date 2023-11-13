verif_n_recolte_specimen <-
  function(capture, specimen, espece) {
    datacapt <-
      capture %>%  dplyr::filter(sp == espece)  %>% droplevels()
    datacapt <-
      datacapt %>% dplyr::select("no_station", "nb_capture",  "nb_pese")
    
    n_capture <- sum(datacapt$nb_capture)
    dataspec <-
      specimen %>%  dplyr::filter(sp == espece)  %>% droplevels()
    n_specimen <-
      length(specimen$no_specimen) %>% as.numeric()
    
    tableau <- cbind("Data" = c("Recolte", "Specimen"),
                     "N" = c(n_capture, n_specimen))
    
    tableau
  }
  