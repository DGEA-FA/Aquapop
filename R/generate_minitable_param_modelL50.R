generate_minitable_param_modelL50 <- function(model, df = NULL, model_type = c("add", "int", "l")) {
  model_type <- match.arg(model_type)
  
  if (model_type == "add") {
    a <- coef(model)[["(Intercept)"]]
    b <- coef(model)[["ltm"]]
    sexe <- coef(model)[["sexeM"]]
    
    L50_F <- (-a / b) %>% round(digits = 0)
    L50_M <- ((-a - sexe) / b) %>% round(digits = 0)
    
    LI_F <- deltaMethod(coef(model), "-(Intercept)/ltm", vcov. = vcov(model))$`2.5 %` %>% round(digits = 0)
    LS_F <- deltaMethod(coef(model), "-(Intercept)/ltm", vcov. = vcov(model))$`97.5 %` %>% round(digits = 0)
    LI_M <- deltaMethod(coef(model), "-(Intercept+sexeM)/(ltm)", vcov. = vcov(model))$`2.5 %` %>% round(digits = 0)
    LS_M <- deltaMethod(coef(model), "-(Intercept+sexeM)/(ltm)", vcov. = vcov(model))$`97.5 %` %>% round(digits = 0)
    
    minitable <- rbind(
      c("L~50~ Mâle (mm)", L50_M, glue("[{LI_M }-{LS_M }]")),
      c("L~50~ Femelle (mm)", L50_F, glue("[{LI_F }-{LS_F }]")),
      c("a", round(a , digits = 3), NA),
      c("b", round(b , digits = 3), NA),
      c("sexe", round(sexe , digits = 3), NA)
    )
  } else if (model_type == "int") {
    a <- coef(model)[["(Intercept)"]]
    b <- coef(model)[["ltm"]]
    sexe <- coef(model)[["sexeM"]]
    interaction <- coef(model)[["ltm:sexeM"]]
    LINKK <- model$family$link
    
    dfM <- df %>% dplyr::filter(sexe == "M") %>% droplevels()
    modelM <- glm((as.numeric(maturite) - 1) ~ ltm, data = dfM, family = binomial(link = LINKK))
    l50arrayM <- dose.p(modelM, cf = 1:2, p = 0.5)
    L50M <- round(as.numeric(l50arrayM), 0)
    sel50M <- round(as.numeric(attr(l50arrayM, "SE")), 0)
    Delta_CI_M <- L50M + 1.96 * c(-1, 1) * sel50M
    LI_M <- round(Delta_CI_M[1], 0)
    LS_M <- round(Delta_CI_M[2], 0)
    
    dfF <- df %>% dplyr::filter(sexe == "F") %>% droplevels()
    modelF <- glm((as.numeric(maturite) - 1) ~ ltm, data = dfF, family = binomial(link = LINKK))
    l50arrayF <- dose.p(modelF, cf = 1:2, p = 0.5)
    L50F <- round(as.numeric(l50arrayF), 0)
    sel50F <- round(as.numeric(attr(l50arrayF, "SE")), 0)
    Delta_CI_F <- L50F + 1.96 * c(-1, 1) * sel50F
    LI_F <- round(Delta_CI_F[1], 0)
    LS_F <- round(Delta_CI_F[2], 0)
    
    minitable <- rbind(
      c("L~50~ Mâle (mm)", L50M, glue("[{LI_M}-{LS_M}]")),
      c("L~50~ Femelle (mm)", L50F, glue("[{LI_F}-{LS_F}]")),
      c("a", round(a, digits = 3), NA),
      c("b", round(b, digits = 3), NA),
      c("sexe", round(sexe, digits = 3), NA),
      c("interaction", round(interaction, digits = 3), NA)
    )
  } else if (model_type == "l") {
    a <- coef(model)[["(Intercept)"]]
    b <- coef(model)[["ltm"]]
    
    L50 <- (-a / b) %>% round(digits = 0)
    
    LI <- deltaMethod(coef(model), "-(Intercept)/ltm", vcov. = vcov(model))$`2.5 %` %>% round(digits = 0)
    LS <- deltaMethod(coef(model), "-(Intercept)/ltm", vcov. = vcov(model))$`97.5 %` %>% round(digits = 0)
    
    minitable <- rbind(
      c("L~50~ (mm)", L50, glue("[{LI }-{LS }]")),
      c("a", round(a , digits = 3), NA),
      c("b", round(b , digits = 3), NA)
    )
  }
  
  minitable <- as.data.frame(minitable)
  colnames(minitable) <- c("Paramètre", "Valeur", "IC 95%")
  
  return(minitable)
}
