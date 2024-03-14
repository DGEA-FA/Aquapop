table_recap <- function(datalac, data_station) {

  temp1 <- datalac %>% reframe(
    "Type de pêche" = unique(typ_pech),
    "No de lac" = unique(no_lac),
    "Nom du lac" = unique(nom_lac),
    "Superficie du lac (ha)" = unique(superficie_ha),
    "Année(s) de l’inventaire (aaaa)" = toString(unique(annee))
  )
  
  temp2 <- data_station %>% reframe(
    "Date de début de l’inventaire (aaaa-mm-jj)" = {
      if (length(date_pose) == 0 || all(is.na(date_pose))) {
        "Aucune donnée disponible"
      } else {
        min(date_pose, na.rm = TRUE)
      }
    },
    "Date de fin de l’inventaire (aaaa-mm-jj)" = {
      if (length(date_leve) == 0 || all(is.na(date_leve))) {
        "Aucune donnée disponible"
      } else {
        max(date_leve, na.rm = TRUE)
      }
    }
  )
  
  temp3 <- data_station %>% filter(st_hasard == "O") %>% reframe("N stations aléatoires" = n())
  temp4 <- data_station %>% filter(st_hasard == "N") %>% reframe("N stations dirigées" = n())
  temp5 <- data_station %>% filter(st_valide == "O") %>% reframe("N stations valides" = n())
  temp6 <- data_station %>% filter(st_valide == "N") %>% reframe("N stations invalides" = n())
  
  
  # Nombre de stations différentes
  temp7 <- data.frame("N.stations.total" = data_station %>% 
                                       distinct(no_station) %>% 
                                       nrow())
  temp7 <- temp7 %>% rename(`N stations total` = N.stations.total)
  
  sp.nice <- unique(datalac$sp_pen)
  clean <- bind_cols(temp1, temp2, temp3, temp4, temp5, temp6, temp7)
  clean[-1] %>% t() %>% as.data.frame() %>% setNames(clean[, 1]) %>%
    tibble::rownames_to_column("Type de pêche")
}