relation_masse_longueur <- function(data, espece) {
  dfAllometrie <-
    data  %>% filter(sp == espece) %>% droplevels() #seulement l'espece PEN
  dfAllometrie  <-
    subset(dfAllometrie,!is.na(ltm))# removing all records where mesures ltm were missing
  dfAllometrie <-
    subset(dfAllometrie,!is.na(masse))# removing all records where mesures masse were missing
  dfAllometrie <- dfAllometrie %>%
    mutate(logW = log10(masse), #quantitative response variable
           logL = log10(ltm)) #quantitative explanatory variable
  #Surtout inspiré de Ogle p.134 IFAR
  
  fit1 <-
    lm(logW ~ logL,  data = dfAllometrie) #simple linear regression fitted with lm
  a <-
    coef(fit1)[1] %>% as.numeric() %>% round(digits = 3) #extration des parametres du modele #UPDATE CEST LOG 10 de A
  b <-
    coef(fit1)[2] %>% as.numeric() %>% round(digits = 3) #extration des parametres du modele
  
  #visualising the fit
  tmp <-
    range(dfAllometrie$logL) #etendue des valeurs de logL (get min et max)
  xs <-
    seq(tmp[1], tmp[2], length.out = 99)  #get sequence de valeurs de min a max by 1 (pour l'axe des X)
  ys <-
    predict(fit1, data.frame(logL = xs)) #predire val de Y pour valeurs de X avec le modele fit1 calcule
  
  #the predicted log-weigth are back transformed to the original scale.
  #Mais comme les valeurs qui sont retransformées d'une échelle log sont biaisée,
  #on fait a common correction for allometric equations (sprugel 1983) (voir Ogle p.136 dans IFAR). Ogle computed this equation in his package using logbtcf().
  cf <- FSA::logbtcf(fit1, 10) #facteur de correction.
  #the corrected back-transformed predicted value of the response variable is then calculated by multiplying the back-transformed predicted value by this correction factor
  btys <-
    cf * 10 ^ predict(fit1, data.frame(logL = xs), interval = "prediction") # same as Ogle p.138 IFAR
  btxs <- 10 ^ xs
  PREDICT <- data.frame("btxs" = btxs, btys)
  PREDICT2 <- data.frame("xs" = xs, "ys" = ys)
  #faire le graphique. Inspirée de Ogle p.139 IFAR, mais que jai traduit en ggplot2
  # Labeltext <- substitute( y==a~x^{b}   )            #il faudrait plutot ecrire lequation en log 10 NON PLUTOT
  # Plutot mettre log10a = et b =
  # Labeltext <- map(paste('<b>log10a = </b>', a, '<b>b = </b>', b,'<br>'), HTML)
  dfAllometrie <- dfAllometrie %>%  dplyr::rename(LTmax = ltm,
                                                  Masse = masse)

  # Création du graphique avec suppression des avertissements
  
    ggRelationML <- suppressWarnings(
    ggplot() +
    geom_point(data = dfAllometrie, aes(
      x = LTmax,
      y = Masse,
      text = paste0("<b># spécimen:</b> ", no_specimen, "<br>")
      
    )) +
    geom_line(data = PREDICT, aes(x = btxs, y = fit)) +
    geom_line(data = PREDICT, aes(x = btxs, y = lwr), linetype = 2) +
    geom_line(data = PREDICT, aes(x = btxs, y = upr), linetype = 2) +
    theme_classic() +
    ggtitle(dfAllometrie$ID) +
    labs(x = "Longueur totale maximale (mm)",
         y = "Masse (g)") +
    theme(panel.background = element_rect(fill = "white", colour = "black"))   +
    annotate(
      "text",
      label = paste0("log10a = ", a, "\n",
                     "b = ", b),
      x = min(dfAllometrie$LTmax) + 200,
      y =  max(dfAllometrie$Masse),
      color = "black",
      vjust = "inward",
      hjust = "inward"
    ) )
  return(ggRelationML)
}