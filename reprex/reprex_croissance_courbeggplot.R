# Voici ce qu'on obtient avec PENT-01480-1994

library(dplyr)
library(ggplot2)
library(FSA)


#Deux references utiles
#https://fishr-core-team.github.io/fishR/blog/posts/2019-12-31_vonB_plots_1/
#https://github.com/droglenc/AGF/blob/master/Chapter12/Chapter_12_Boxes.R

# Create the dataframe 'initcroissance' --------------------------------------------------------

initcroissance <- data.frame(
  ltm = c(649, 195, 219, 260, 266, 447, 752, 687, 600, 608, 397, 448, 405, 425, 878, 723, 673, 668, 470, 500, 407, 263, 728, 665, 680, 687, 627, 638, 647, 439, 482, 780, 526, 463, 444, 396, 480, 639, 481, 416, 384, 624, 630, 337, 442, 432, 447, 476, 647, 656, 379, 201, 728, 660, 588, 640, 671, 427, 411, 663, 510, 502, 627, 484, 155, 200, 275, 325, 282, 257, 497, 439, 205, 246, 685, 635, 433, 619, 667, 505, 428, 465, 690, 432, 497, 585, 517, 470, 492, 674, 507, 410, 522, 468, 710, 666, 751, 358, 791, 646, 538, 478, 687, 304, 281, 258, 745, 405, 480, 625, 258, 234, 184, 274, 705, 660, 498, 481, 402, 645, 482, 432, 397, 223, 375, 219, 254, 485, 172, 160, 270, 265, 254, 215, 198, 204, 206, 193, 295, 288, 272, 215, 400, 339, 620, 788, 527, 477, 507, 606, 718, 410, 470, 605, 210, 509, 454, 611, 652, 715, 513, 446, 436, 389, 615, 588, 594, 637, 652, 642, 711, 200, 255, 454, 489, 390, 673, 505, 502, 408, 435, 423, 622, 750, 518, 523, 627, 750, 663, 540, 627, 670, 729, 804, 271, 589, 595, 594, 567, 630, 790, 410, 464, 463, 504, 425, 638, 427, 446, 447, 462, 463, 758, 715, 268, 315, 254, 777, 427, 480, 515, 592, 534, 668, 454, 568, 604, 696, 702, 746, 252, 453, 496, 418, 673, 469, 645, 497, 376, 390, 347, 777, 797, 220, 249, 464, 445, 615, 467, 495, 680, 264, 300, 247, 310, 752, 341),
  age = c(6, 2, 2, 2, 2, 4, 12, 7, 6, 6, 4, 4, 4, 4, 13, 10, 8, 7, 4, 4, 4, 2, 7, 7, 6, 6, 6, 6, 6, 4, 4, 11, 4, 4, 4, 4, 4, 7, 5, 4, 4, 8, 6, 4, 4, 4, 4, 4, 7, 6, 4, 2, 12, 9, 7, 6, 6, 5, 4, 8, 4, 4, 7, 4, 2, 2, 2, 2, 2, 2, 4, 4, 2, 2, 9, 6, 4, 6, 6, 4, 4, 4, 10, 4, 4, 6, 4, 4, 4, 6, 4, 4, 4, 4, 8, 6, 11, 3, 14, 6, 4, 4, 6, 2, 2, 2, 10, 4, 4, 6, 2, 2, 2, 2, 8, 6, 4, 4, 4, 7, 4, 4, 4, 2, 4, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 4, 4, 6, 12, 4, 4, 4, 6, 12, 4, 4, 5, 2, 4, 4, 6, 7, 12, 4, 4, 4, 4, 6, 6, 6, 6, 7, 8, 12, 2, 2, 4, 4, 4, 8, 4, 4, 4, 4, 4, 10, 10, 4, 4, 6, 10, 7, 5, 6, 7, 10, 12, 2, 6, 6, 6, 7, 9, 11, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 9, 9, 2, 2, 2, 10, 4, 4, 4, 6, 6, 7, 4, 6, 7, 8, 9, 10, 2, 4, 4, 4, 7, 4, 6, 4, 4, 4, 4, 11, 12, 2, 2, 4, 4, 6, 4, 4, 6, 2, 2, 2, 2, 9, 2)
)

# Create the dataframe 'croissance1' --------------------------------------------------------

croissance1 <- data.frame(
  Modeles = c("Gompertz", "Logistique", "Von Bertalanffy"),
  Linf = c(776, 749, 829),
  L_infty_IC95 = c("[756-799]", "[732-768]", "[798-864]"),
  K = c(0.360, 0.520, 0.211),
  K_IC95 = c("[0.36-0.423]", "[0.52-0.604]", "[0.211-0.263]"),
  t0 = c(2.279, 3.143, 0.373),
  t0_IC95 = c("[2.279-2.479]", "[3.143-3.371]", "[0.373-0.703]"),
  AICc = c(2675.13, 2677.59, 2683.67),
  Delta_AICc = c(0.00, 2.47, 8.55),
  Poids_d_Akaike = c(0.77, 0.22, 0.01),
  Convergence = c("convergé", "convergé", "convergé")
)


# VONBERT courbe croissance ----------------------------------------------------------
    init <- initcroissance
    tablemodele <- croissance1
    
    colnames(tablemodele)[1] <- "Methode"
    model <- tablemodele %>% dplyr::filter(Methode == "Von Bertalanffy")
    
    #pour avoir les ranges d'ages
    dfbase <-
      FSA::Summarize(ltm ~ age, data = init)  #truc de FSA pour avoir la ltm moy par age
    agemin <- min(dfbase$age) #age minimum
    agemax <- max(dfbase$age)
    ages <- c(agemin:agemax)
    ageGRAPH <- c(0, (ceiling(agemax / 5) * 5) + 1)
    ageGRAPHmin <- ageGRAPH[1]
    ageGRAPHmax <- ageGRAPH[2]
    ageGRAPHbreak <- c(ageGRAPHmin:ageGRAPHmax)
    
    sv0 <- FSA::vbStarts(ltm ~ age, data = init)
    
    sv0$Linf <- model$Linf
    sv0$K <- model$K
    sv0$t0 <- model$t0
    
    fit0 <-   nls(ltm ~ Linf * (1 - exp(-K * (age - t0))), data = init, start = sv0) #####cette ligne fait quon predict avec une equation de vonB meme si je mets les param des 2 autres

    preds <- data.frame(age = ageGRAPHbreak,
                        investr::predFit(fit0, data.frame(age = ageGRAPHbreak),
                                         interval = "confidence"))
    
    ggplot() +
      geom_ribbon(data = preds,
                  aes(x = age, ymin = lwr, ymax = upr),
                  fill = "gray80") +
      geom_point(data = init,
                 aes(y = ltm, x = age),
                 size = 2,
                 alpha = 0.1) +
      geom_line(
        data = preds,
        aes(x = age, y = fit),
        linewidth = 1,
        linetype = "dashed"
      ) +
      geom_line(
        data = dplyr::filter(preds, age >= agemin, age <= agemax),
        aes(x = age, y = fit),
        linewidth = 1
      ) +
      scale_y_continuous(name = "Longueur totale maximale (mm)", expand = c(0, 0)) +
      scale_x_continuous(
        name = "Âge (année)",
        breaks = ageGRAPHbreak,
        limits = ageGRAPH,
        expand = c(0, 0)
      ) +
      theme_bw() +
      theme(panel.grid = element_blank()) +
      annotate(
        "segment",
        x = -Inf,
        xend = Inf,
        y =  sv0$Linf ,
        yend = sv0$Linf  ,
        linewidth = 0.5,
        color = "red",
        linetype = 2
      )
    
    #https://fishr-core-team.github.io/fishR/blog/posts/2019-12-31_vonB_plots_1/
  


# GOMP courbe croissance ----------------------------------------------------------
  init <- initcroissance
  tablemodele <- croissance1
  
  colnames(tablemodele)[1] <- "Methode"
  model <- tablemodele %>% filter(Methode == "Gompertz")
  
  #pour avoir les ranges dages
  dfbase <-
    FSA::Summarize(ltm ~ age, data = init)  #truc de FSA pour avoir la ltm moy par age
  agemin <- min(dfbase$age) #age minimum
  agemax <- max(dfbase$age)
  ages <- c(agemin:agemax)
  ageGRAPH <- c(0, (ceiling(agemax / 5) * 5) + 1)
  ageGRAPHmin <- ageGRAPH[1]
  ageGRAPHmax <- ageGRAPH[2]
  ageGRAPHbreak <- c(ageGRAPHmin:ageGRAPHmax)
  
  vb <- FSA::vbFuns(param = "Typical")
  (sv0 <- FSA::vbStarts(ltm ~ age, data = init))
  
  sv0$Linf <- model$Linf
  sv0$K <- model$K
  sv0$t0 <- model$t0
  
  fit0 <- nls(ltm~vb(age,Linf,K,t0),data=init,start=sv0)
  # fit0 <-   nls(ltm ~ Linf * (1 - exp(-K * (age - t0))), data = init, start = sv0) #CHANGÉ PAR ROX, SINON NE TROUVE PAS LA FONCTION vb DANS SHINY
  
  preds <- data.frame(age = ageGRAPHbreak,
                      investr::predFit(fit0, data.frame(age = ageGRAPHbreak),
                                       interval = "confidence"))
  
  ggplot() +
    geom_ribbon(data = preds,
                aes(x = age, ymin = lwr, ymax = upr),
                fill = "gray80") +
    geom_point(data = init,
               aes(y = ltm, x = age),
               size = 2,
               alpha = 0.1) +
    geom_line(
      data = preds,
      aes(x = age, y = fit),
      linewidth = 1,
      linetype = "dashed"
    ) +
    geom_line(
      data = filter(preds, age >= agemin, age <= agemax),
      aes(x = age, y = fit),
      linewidth = 1
    ) +
    scale_y_continuous(name = "Longueur totale maximale (mm)", expand = c(0, 0)) +
    scale_x_continuous(
      name = "Âge (année)",
      breaks = ageGRAPHbreak,
      limits = ageGRAPH,
      expand = c(0, 0)
    ) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    annotate(
      "segment",
      x = -Inf,
      xend = Inf,
      y =  sv0$Linf ,
      yend = sv0$Linf  ,
      linewidth = 0.5,
      color = "red",
      linetype = 2
    )
  
  #https://fishr-core-team.github.io/fishR/blog/posts/2019-12-31_vonB_plots_1/




# LOGISTIC courbe croissance ----------------------------------------------------------
  init <- initcroissance
  tablemodele <- croissance1
  
  colnames(tablemodele)[1] <- "Methode"
  model <- tablemodele %>% filter(Methode == "Logistique")
  
  #pour avoir les ranges dages
  dfbase <-
    FSA::Summarize(ltm ~ age, data = init)  #truc de FSA pour avoir la ltm moy par age
  agemin <- min(dfbase$age) #age minimum
  agemax <- max(dfbase$age)
  ages <- c(agemin:agemax)
  ageGRAPH <- c(0, (ceiling(agemax / 5) * 5) + 1)
  ageGRAPHmin <- ageGRAPH[1]
  ageGRAPHmax <- ageGRAPH[2]
  ageGRAPHbreak <- c(ageGRAPHmin:ageGRAPHmax)
  
  vb <- FSA::vbFuns(param = "Typical")
  (sv0 <- FSA::vbStarts(ltm ~ age, data = init))
  
  sv0$Linf <- model$Linf
  sv0$K <- model$K
  sv0$t0 <- model$t0
  
  #fit0 <- nls(ltm~vb(age,Linf,K,t0),data=init,start=sv0)
  fit0 <- nls(ltm ~ Linf * (1 - exp(-K * (age - t0))), data = init, start = sv0) #CHANGÉ PAR ROX, SINON NE TROUVE PAS LA FONCTION vb DANS SHINY
  
  preds <- data.frame(age = ageGRAPHbreak,
                      investr::predFit(fit0, data.frame(age = ageGRAPHbreak),
                                       interval = "confidence"))
  
  ggplot() +
    geom_ribbon(data = preds,
                aes(x = age, ymin = lwr, ymax = upr),
                fill = "gray80") +
    geom_point(data = init,
               aes(y = ltm, x = age),
               size = 2,
               alpha = 0.1) +
    geom_line(
      data = preds,
      aes(x = age, y = fit),
      linewidth = 1,
      linetype = "dashed"
    ) +
    geom_line(
      data = filter(preds, age >= agemin, age <= agemax),
      aes(x = age, y = fit),
      linewidth = 1
    ) +
    scale_y_continuous(name = "Longueur totale maximale (mm)", expand = c(0, 0)) +
    scale_x_continuous(
      name = "Âge (année)",
      breaks = ageGRAPHbreak,
      limits = ageGRAPH,
      expand = c(0, 0)
    ) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    annotate(
      "segment",
      x = -Inf,
      xend = Inf,
      y =  sv0$Linf ,
      yend = sv0$Linf  ,
      linewidth = 0.5,
      color = "red",
      linetype = 2
    )
  


