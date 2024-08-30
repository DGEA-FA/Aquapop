generate_minitable_param_model_a50 <- function(model, df = NULL, model_type = "L") {
  a <- coef(model)[["(Intercept)"]]
  b <- coef(model)[["age"]]
  
  if (model_type == "L") {
    # Pour le modèle linéaire simple
    A50 <- (-a / b) %>% round(digits = 0)
    LI <- deltaMethod(coef(model), "-(Intercept)/age", vcov. = vcov(model))$`2.5 %` %>% round(digits = 0)
    LS <- deltaMethod(coef(model), "-(Intercept)/age", vcov. = vcov(model))$`97.5 %` %>% round(digits = 0)
    
    minitable <- rbind(
      c("A~50~ (mm)", A50, glue::glue("[{LI }-{LS }]")),
      c("a", round(a , digits = 3), NA),
      c("b", round(b , digits = 3), NA)
    ) %>% as.data.frame()
    
  } else if (model_type == "ADD") {
    # Pour le modèle additif (âge + sexe)
    sexe <- coef(model)[["sexeM"]]
    
    A50_F <- (-a / b) %>% round(digits = 0)
    A50_M <- ((-a - sexe) / b) %>% round(digits = 0)
    
    LI_F <- deltaMethod(coef(model), "-(Intercept)/age", vcov. = vcov(model))$`2.5 %` %>% round(digits = 0)
    LS_F <- deltaMethod(coef(model), "-(Intercept)/age", vcov. = vcov(model))$`97.5 %`  %>% round(digits = 0)
    LI_M <- deltaMethod(coef(model), "-(Intercept+sexeM)/(age)", vcov. = vcov(model))$`2.5 %`  %>% round(digits = 0)
    LS_M <- deltaMethod(coef(model), "-(Intercept+sexeM)/(age)", vcov. = vcov(model))$`97.5 %` %>% round(digits = 0)
    
    minitable <- rbind(
      c("A~50~ Mâle (mm)", A50_M, glue("[{LI_M }-{LS_M }]")),
      c("A~50~ Femelle (mm)", A50_F, glue("[{LI_F }-{LS_F }]")),
      c("a", round(a , digits = 3), NA),
      c("b", round(b , digits = 3), NA),
      c("sexe", round(sexe , digits = 3), NA)
    ) %>% as.data.frame()
    
  } else if (model_type == "INT") {
    # Pour le modèle avec interaction (âge * sexe)
    sexe <- coef(model)[["sexeM"]]
    interaction <- coef(model)[["age:sexeM"]]
    LINKK <- model$family$link
    
    dfM <- df %>% dplyr::filter(sexe == "M") %>% droplevels()
    modelM <- glm((as.numeric(maturite) - 1) ~ age, data = dfM, family = binomial(link = LINKK))
    a50arrayM <- as.array(dose.p(modelM, cf = 1:2, p = 0.5))
    A50_M <- round(as.numeric(a50arrayM), digits = 0)
    sea50M <- round(as.numeric(attr(a50arrayM, "SE")), 2)
    Delta_CI_M <- A50_M + 1.96 * c(-1, 1) * sea50M
    LI_M <- round(Delta_CI_M[1], 0)
    LS_M <- round(Delta_CI_M[2], 0)
    
    dfF <- df %>% dplyr::filter(sexe == "F") %>% droplevels()
    modelF <- glm((as.numeric(maturite) - 1) ~ age, data = dfF , family = binomial(link = LINKK))
    a50arrayF <- as.array(dose.p(modelF, cf = 1:2, p = 0.5))
    A50_F <- round(as.numeric(a50arrayF), digits = 0)
    sea50F <- round(as.numeric(attr(a50arrayF, "SE")), 2)
    Delta_CI_F <- A50_F + 1.96 * c(-1, 1) * sea50F
    LI_F <- round(Delta_CI_F[1], 0)
    LS_F <- round(Delta_CI_F[2], 0)
    
    minitable <- rbind(
      c("A~50~ Mâle (mm)", A50_M, glue("[{LI_M}-{LS_M}]")),
      c("A~50~ Femelle (mm)", A50_F, glue("[{LI_F}-{LS_F}]")),
      c("a", round(a, digits = 3), NA),
      c("b", round(b, digits = 3), NA),
      c("sexe", round(sexe, digits = 3), NA),
      c("interaction", round(interaction, digits = 3), NA)
    ) %>% as.data.frame()
  }
  
  colnames(minitable)[1] <- "Paramètre"
  colnames(minitable)[2] <- "Valeur"
  colnames(minitable)[3] <- "IC 95%"
  
  minitable
}
