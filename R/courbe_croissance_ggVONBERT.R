courbe_croissance_ggVONBERT <-
  function(dfspecimen, sp_pen, tablemodele) {
    # Filtrer les données pour l'espèce spécifiée
    init <- dfspecimen %>% filter(sp == sp_pen)
    
    # Supprimer les enregistrements avec des valeurs manquantes pour ltm et age
    init <- init %>%
      filter(!is.na(ltm) & !is.na(age))
    
    # Sélectionner uniquement les colonnes nécessaires
    init <- init %>% select(ltm, age, no_specimen)
    
    # Renommer les rangées séquentiellement de 1 à n
    rownames(init) <- seq(nrow(init))
    
    
    model <- tablemodele %>% dplyr::filter(methode == "Von Bertalanffy")
    
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
    
    sv0$Linf <- model$l_inf
    sv0$K <- model$k
    sv0$t0 <- model$t0
    
    # fit0 <- nls(ltm ~ Linf * (1 - exp(-K * (age - t0))), data = init, start = sv0) #####cette ligne fait quon predict avec une equation de vonB meme si je mets les param des 2 autres
    fit0 <- nls(ltm~vBert(age,Linf,K,t0),data=init,start=sv0)
    
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
  }
