

##### Fonction pour extraction des valeurs du modèles (L50, IC, b0, b1, b2, b3)

maturite_extract_resultats_modele <- function(mod, id = NULL) {

  if (is.null(id)) {
    stop("id doit être fourni.")
  }
  
  type_modele <- sub("_.*$", "", id)
  is_separated <- grepl("^[MF]_", id)
  variable <- c("ltm", "age")
  
    # Coefficients
  coef_mod <- stats::coef(mod)
  coef_names <- names(coef_mod)
  
  b0 <- if ("(Intercept)" %in% coef_names) {
    unname(coef_mod["(Intercept)"])
  } else {
    NA_real_
  }
  
  b1 <- NA_real_
  b2 <- NA_real_
  b3 <- NA_real_
  
  if (type_modele == "COM") {
    
    idx_b1 <- grep(paste0("^(", paste(variable, collapse = "|"), "):sexeF$"),
      coef_names)      
    if (length(idx_b1) > 0) {
      b1 <- unname(coef_mod[idx_b1[1]])
    }
    
    idx_b2 <- grep(
      paste0("^(", paste(variable, collapse = "|"), "):sexeM$"),
      coef_names)
    
    if (length(idx_b2) > 0) {
      b2 <- unname(coef_mod[idx_b2[1]])
    }  
    
  } else {
    
    idx_b1 <- which(coef_names %in% c("ltm", "age"))
    
  if (length(idx_b1) > 0) {
    b1 <- unname(coef_mod[idx_b1[1]])
  }
  
  idx_sexe <- grep("^sexe", coef_names)
  if (length(idx_sexe) > 0) {
    b2 <- unname(coef_mod[idx_sexe[1]])
  }
  
  idx_inter <- grep(":sexeM$", coef_names)
  if (length(idx_inter) > 0) {
    b3 <- unname(coef_mod[idx_inter[1]])
  }
  }
  
  # Point50 et IC95
  
  point50 <- NA_real_
  
  point50_F <- NA_real_
  point50_M <- NA_real_
  
  point50_IC95_inf <- NA_real_
  point50_IC95_sup <- NA_real_
  
  conv <- isTRUE(mod$converged)
  
  if (
    conv &&
    is.finite(b0) &&
    is.finite(b1) &&
    b1 != 0
  ) {
    
    # ------------------------------------------------------------------------
    # Modèles séparés et TLO
    # ------------------------------------------------------------------------
    
    if (is_separated || type_modele == "TLO") {
      
      ci_L <- tryCatch(
        confint_L(
          object = mod,
          method = "montecarlo",
          interval_type = "bca"
        ),
        error = function(e) {
          print(e)
          NULL
       }
    )
      
      if (
        !is.null(ci_L) &&
        is.matrix(ci_L) &&
        nrow(ci_L) >= 1 &&
        all(c("estimate", "lower", "upper") %in% colnames(ci_L))
      ) {
        
        point50 <- as.numeric(
          ci_L[1, "estimate"]
        )
        
        point50_IC95_inf <- as.numeric(
          ci_L[1, "lower"]
        )
        
        point50_IC95_sup <- as.numeric(
          ci_L[1, "upper"]
        )
      }
      
    } else {
      
      # ----------------------------------------------------------------------
      # ADD / COM / INT
      # ----------------------------------------------------------------------
      
      lien <- as.character(mod$family$link)
      
      kappa <- if (lien == "cloglog") {
        0.3665129
      } else {
        0
      }
      
      if (type_modele == "ADD") {
        
        point50_F <- (-b0 - kappa) / b1
        
        point50_M <- (-b0 - b2 - kappa) / b1
        
      } else if (type_modele == "COM") {
        
        point50_F <- (-b0 - kappa) / b1
        
        point50_M <- (-b0 - kappa) / b2
        
      } else if (type_modele == "INT") {
        
        point50_F <- (-b0 - kappa) / b1
        
        point50_M <- (-b0 - b2 - kappa) / (b1 + b3)
      }
    }
  }
  
  return(
    list(
      b0 = b0,
      b1 = b1,
      b2 = b2,
      b3 = b3,
      point50 = point50,
      point50_F = point50_F,
      point50_M = point50_M,
      point50_IC95_inf = point50_IC95_inf,
      point50_IC95_sup = point50_IC95_sup
    )
  )
}

####################################################
##### Fonction pour ajustement du modèle (o.r.test, p_link, AICc et delta_AICc)

maturite_evaluer_ajustement <- function(mod) {
  
  conv <- isTRUE(mod$converged)
  
  p_fit <- tryCatch(
    o.r.test(mod),
    error = function(e) NA_real_
  )
  
  p_link <- tryCatch(
    maturite_test_lien(mod),
    error = function(e) NA_real_
  )
  
  aicc <- tryCatch(
    MuMIn::AICc(mod),
    error = function(e) NA_real_
  )
  
  ajust <- conv &&
    (is.na(p_fit) || p_fit > 0.05) &&
    (is.na(p_link) || p_link > 0.05)
  
  list(
    convergence = conv,
    pearson_x2_pval = p_fit,
    goodness_of_link_pval = p_link,
    ajust = ajust,
    aicc = aicc
  )
}
  
  