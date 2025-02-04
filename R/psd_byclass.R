psd_byclass <- function(data, espece) {
  df  <- data %>% filter(sp == espece) %>% droplevels()# selectionner slmt le data necessaire
  Classename <-
    c("Sous-stock",
      "Stock",
      "Qualité",
      "Préférée",
      "Mémorable",
      "Trophée")
  
  if (espece == "SANA") {
    breakClass <- c(0, 300, 500, 650, 800, 1000)
  } else if (espece == "SAFO") {
    breakClass <- c(0, 150, 250, 325, 400, 500)
  } else if (espece == "SAVI") {
    breakClass <- c(0, 250, 380, 510, 630, 760)
  }
  
  limInfStock <-
    breakClass [2] # limite inférieure de la classe Stock pour cette sp.
  
  bunch <-  df  %>%
    filter(ltm >= limInfStock) %>%
    mutate(gcat = FSA::lencat(
      ltm,
      breaks = breakClass,
      droplevels = TRUE
    ))
  
  bunch <- bunch %>% mutate(
           Classe = plyr::mapvalues(gcat, 
                                    from = breakClass,
                                    to = Classename, warn_missing = FALSE)) #classe de taille texte
  
  if (espece == "SANA") {
    bunch  <-
      mutate(bunch , range = plyr::mapvalues(
        gcat,
        from = breakClass,
        to = c(
          "<300",
          "300-499",
          "500-649",
          "650-799",
          "800-999",
          ">=1000"
        ), 
        warn_missing = FALSE
      ))
  } else if (espece == "SAFO") {
    bunch  <-
      mutate(bunch , range = plyr::mapvalues(
        gcat,
        from = breakClass,
        to = c("<150", "150-249", "250-324", "325-399", "400-499", ">=500"), 
        warn_missing = FALSE
      ))
  } else if (espece == "SAVI") {
    bunch  <-
      mutate(bunch ,
             range = plyr::mapvalues(
               gcat,
               from = breakClass,
               to = c("<250", "250-379", "380-509", "510-629", "630-759", ">=760"),
               warn_missing = FALSE
             ))
  }
  y <-
    bunch %>% group_by(gcat, Classe, range) %>% summarise(n = n(),
                                                          .groups = "keep"
                                                          ) %>% droplevels()#nb de poissons dans ch classe de taille
  bunch <-
    merge(bunch, y, by = c("gcat", "Classe", "range")) #merge le df avec le nb de poissons
  
  gfreq <-  xtabs(~ gcat, data = bunch)
  psdtable <- prop.table(gfreq) * 100
  psdtable <- psdtable %>% as.data.frame()
  bunch <-
    merge(bunch, psdtable, by = "gcat") #merge le df avec le nb de poissons
  bunch  <- bunch %>% dplyr::select(c(Classe, range, n, Freq))
  bunch <-
    bunch %>% group_by(Classe, range, n, Freq) %>% summarise(
      .groups = "keep"
      )
  virgin <- tibble(Classe = Classename)
  if (espece == "SANA") {
    virgin$range <-
      c("<300",
        "300-499",
        "500-649",
        "650-799",
        "800-999",
        ">=1000")
  } else if (espece == "SAFO") {
    virgin$range <-
      c("<150", "150-249", "250-324", "325-399", "400-499", ">=500")
  } else if (espece == "SAVI") {
    virgin$range <-
      c("<250", "250-379", "380-509", "510-629", "630-759", ">=760")
  }
  complet <-
    merge(virgin,
          bunch,
          by = c("Classe", "range"),
          all.x = TRUE) #merge le df avec le nb de poissons
  complet$Classe <- as.factor(complet$Classe)
  complet$Classe <- factor(complet$Classe, levels = Classename)
  complet <- complet %>% arrange(Classe)
  complet$Freq <- as.numeric(complet$Freq) %>% round(digits = 0)
  complet[1, 3] <-
    data %>% filter(sp == espece) %>%  filter(ltm < limInfStock) %>% summarise(n())
  colnames(complet)[2] <- "Intervalle (mm)"
  complet <-
    complet %>% mutate(Freq = ifelse(is.na(Freq), "0", Freq))
  colnames(complet)[4] <- "%"
  complet$n <- complet$n %>% tidyr::replace_na(0)
  complet
}