table_recap <- function(datalac, capture) {
  temp1 <- datalac %>% summarise(
    "Type de pêche" = unique(typ_pech),
    "No de lac" = unique(no_lac),
    "Nom du lac" = unique(nom_lac),
    "Superficie du lac (ha)" = unique(superficie_ha),
    "Année de l’inventaire (aaaa)" = paste(annee, collapse = ", ")
  )
  temp2 <-
    capture %>% summarise(
      "Date de début de l’inventaire (aaaa-mm-jj)" = paste(min(date_pose)),
      "Date de fin de l’inventaire (aaaa-mm-jj)" = paste(max(date_leve))
    )
  temp3 <-
    capture %>% filter(st_hasard == "O") %>% summarise("N stations aléatoires" = n())
  temp4 <-
    capture %>% filter(st_hasard == "N") %>% summarise("N stations dirigées" = n())
  sp.nice <- unique(datalac$sp_pen)
  clean <- bind_cols(temp1, temp2, temp3, temp4)
  clean[-1] %>% t() %>% as.data.frame() %>% setNames(clean[, 1]) %>%
    tibble::rownames_to_column("Type de pêche")
}