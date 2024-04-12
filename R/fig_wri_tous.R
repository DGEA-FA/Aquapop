fig_wri_tous <- function(data, espece) {
  #TableWeightRef
  t <- FSA::wsVal("Lake Trout") #obtenir les valeurs de reference pour cette espece selon la liste de Ogle
  a <- FSA::wsVal("Brook Trout") #obtenir les valeurs de reference pour cette espece selon la liste de Ogle
  b <- FSA::wsVal("Walleye") #obtenir les valeurs de reference pour cette espece selon la liste de Ogle
  TableWeightRef <- rbind(t,a,b)
  
  rm(list= c( "t", "a", "b"))
  library(dplyr)
  TableWeightRef <- TableWeightRef %>% mutate(sp= c("SANA", "SAFO", "SAVI"))
  
  
  
  
  init <-
    data %>% filter(sp == espece)# selectionner slmt le data necessaire
  init <-
    subset(init , !is.na(masse) &
             !is.na(ltm)) # removing all records where mesures of either length or weight were missing
  init <-
    init  %>% dplyr::select(no_specimen, sp, ltm, masse, age, sexe, maturite)
  init$sexe <-
    factor(
      init$sexe,
      levels = c(
        "F",
        "M",
        "IND"
      )
    )
  
  slope <-
    TableWeightRef$slope[TableWeightRef$sp == espece] %>% as.numeric() #isoler la valeur de la pente
  int <-
    TableWeightRef$int[TableWeightRef$sp == espece] %>% as.numeric() #isoler la valeur de l'intercept
  ltmmin <-
    TableWeightRef$min.TL[TableWeightRef$sp == espece] %>% as.numeric() # isoler la limite minimum de taille
  
  init <-
    init %>% filter(ltm >= ltmmin) #filter les donnees pour retirer les specimens dont la taille est hors limite
  init <-
    mutate(init , prediction = 10 ^ (int + (slope * log10(ltm)))) #valeur de masse predite
  init <-
    mutate(init , Wri = masse * 100 / prediction) #valeur de masse relative
  
  meanmale <-
    init %>% filter(sexe == "M") %>% summarise(moymale = mean(Wri)) %>% as.numeric()
  meanfemale <-
    init %>% filter(sexe == "F") %>% summarise(moyfemale = mean(Wri)) %>% as.numeric()
  meantous <-
    init  %>% summarise(moytous = mean(Wri)) %>% as.numeric()
  
  
  ggplot2::ggplot(data = init, aes(x = ltm, y = Wri, color = sexe)) +
    geom_point() +
    scale_color_manual(
      values = c(
        "F" = "#084594",
        "M" = "#99CCFF",
        "IND" = "#4d4d4d"
      ),
      name = "",
      labels = c("Femelle", "Mâle", "Indéterminé")
    ) +
    theme_classic() +
    labs(x = "Longueur totale maximale (mm)",
         y = "Indice de condition (%)") +
    theme(panel.background = element_rect(fill = "white", colour = "black")) +
    annotate(
      "segment",
      x = -Inf,
      xend = Inf,
      y = 100,
      yend = 100,
      linewidth = 0.5,
      color = "lightgrey",
      linetype = 2 #pointille
    )  +
    annotate(
      "segment",
      x = -Inf,
      xend = Inf,
      y = meanmale,
      yend = meanmale,
      linewidth = 0.5,
      color = "#99CCFF",
      linetype = 2 #pointille
    )  +
    annotate(
      "segment",
      x = -Inf,
      xend = Inf,
      y = meantous,
      yend = meantous,
      linewidth = 0.5,
      color = "red",
      linetype = 2 #pointille
    )  +
    annotate(
      "segment",
      x = -Inf,
      xend = Inf,
      y = meanfemale,
      yend = meanfemale,
      linewidth = 0.5,
      color = "#084594",
      linetype = 2 #pointille
    )
}