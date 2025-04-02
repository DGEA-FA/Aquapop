myspinner <- 6

#' Constantes associées aux espèces suivies par PEN
#'
#' Contient le nom commun, le binwidth recommandé pour les histogrammes,
#' les seuils de classes PSD (`breaks`) et leurs libellés (`break_labels`) par espèce (`sp`).
#'
#' @export
pen_constants <- tibble::tibble(
  sp       = c("SANA", "SAFO", "SAVI"),
  nom_sp   = c("touladis", "ombles de fontaine", "dorés jaunes"),
  binwidth = c(50, 20, 50),
  breaks   = list(
    c(0, 300, 500, 650, 800, 1000),
    c(0, 150, 250, 325, 400, 500),
    c(0, 250, 380, 510, 630, 760)
  ),
  break_labels = list(
    c("<300", "300-499", "500-649", "650-799", "800-999", ">=1000"),
    c("<150", "150-249", "250-324", "325-399", "400-499", ">=500"),
    c("<250", "250-379", "380-509", "510-629", "630-759", ">=760")
  )
)

#' Noms standardisés des classes PSD
#'
#' Utilisés dans les fonctions PSD (psd_indice, psd_byclass, psd_plot)
#'
#' @export
psd_classnames <- c("Sous-stock", "Stock", "Qualité", "Préférée", "Mémorable", "Trophée")



kable_wri <- function(data) {
  req(data)
  data %>% 
    kable( align = c("r","c","c","c","c","c","c","c","c","r"),
           caption = "Indice de masse relative (Wr)"
    ) %>%
    kable_styling(full_width = FALSE,
                  font_size = 12,
                  html_font="sans-serif", 
                  position="center") %>% 
    column_spec(1, #row
                border_right = TRUE) %>% 
    column_spec(2, #tous
                border_right = TRUE) %>% 
    column_spec(4, #male
                border_right = TRUE)
}

gt_abondance <- function(data) {
  # Extraire les labels des colonnes
  column_labels <- sapply(data, function(col) attr(col, "label"))
  
  data %>%
    gt() %>%
    tab_header(
      title = md("**Tableau d'abondance**")
    ) %>%
    # Utiliser les labels extraits dans cols_label
    cols_label(
      group = column_labels["group"],
      abundance = column_labels["abundance"],
      proportion = column_labels["proportion"],
      cpue = column_labels["cpue"],
      ic95 = column_labels["ic95"],
      mf_ratio = column_labels["mf_ratio"]
    ) %>%
    cols_align(
      align = "center",
      columns = everything()
    ) %>%
    fmt_number(
      columns = c(proportion, cpue),
      decimals = 2
    ) %>%
    tab_options(
      table.width = "auto",  # Ajuster automatiquement la largeur du tableau
      table.font.size = px(12)
    )
}

gt_biomasse <- function(data) {
  # Extraire les labels des colonnes
  column_labels <- sapply(data, function(col) attr(col, "label"))
  
  data %>%
    gt() %>%
    tab_header(
      title = md("**Tableau de biomasse**")
    ) %>%
    # Utiliser les labels extraits dans cols_label
    cols_label(
      groupe = column_labels["groupe"],
      biomasse = column_labels["biomasse"],
      percent = column_labels["percent"],
      bpue = column_labels["bpue"],
      ic95 = column_labels["ic95"]
    ) %>%
    cols_align(
      align = "center",
      columns = everything()
    ) %>%
    tab_options(
      table.width = "auto",  # Ajuster automatiquement la largeur du tableau
      table.font.size = px(12)
    )
}


kable_CPUEtous <- function(data) {
  req(data)
  data %>% 
    kable( align = c("r","c","c","c","r"),
           caption = "Comparaison des modèles : tous les spécimens", 
           row.names = FALSE    ) %>%
    kable_styling(full_width = FALSE,
                  font_size = 12,
                  html_font="sans-serif", 
                  position="center") 
}

kable_CPUEFmature <- function(data) {
  req(data)
  data %>% 
    kable( align = c("r","c","c","c","r"),
           caption = "Comparaison des modèles : femelles reproductrices actives", 
           row.names = FALSE) %>%
    kable_styling(full_width = FALSE,
                  font_size = 12,
                  html_font="sans-serif", 
                  position="center") 
}

kable_mortalite1 <- function(data) {
  req(data)
  data %>% 
    kable( #align = c("r","c","c","c","c","c","c","c","c","r"),
           caption = "Table de sélection des modèles de l’estimation de la mortalité", 
           row.names = FALSE    ) %>%
    kable_styling(full_width = FALSE,
                  font_size = 12,
                  html_font="sans-serif", 
                  position="center") 
}

gt_mortalite2 <- function(data) {
  req(data)
  
  # Utilisation des labels comme noms de colonnes dans gt
  data %>%
    gt() %>%
    tab_header(
      title = md("**Estimations obtenues à partir du modèle de Robson-Chapman**"),
      subtitle = "À titre comparatif seulement"
    ) %>%
    cols_label(
      methode = var_label(data$methode),
      z = var_label(data$z),
      se = var_label(data$se),
      a = var_label(data$a),
      ic_95 = var_label(data$ic_95)
    ) %>%
    cols_align(
      align = "center",
      columns = everything()
    ) %>%
    tab_options(
      table.width = pct(100),
      table.font.size = px(12),
      table.font.names = "sans-serif",
      table.align = "center"
    )
}


# Copy report to temporary directory. This is mostly important when
# deploying the app, since often the working directory won't be writable
report_path <- tempfile(fileext = ".Rmd")
file.copy("report.Rmd", report_path, overwrite = TRUE)

render_report <- function(input, output, params) {
  rmarkdown::render(input,
                    output_file = output,
                    params = params,
                    envir = new.env(parent = globalenv())
  )
}


# Fonction pour générer le rapport Word
generate_report <- function(data_brut, output_file, data_comment = NULL, result_table = NULL) {
  
  # Créer une liste de paramètres pour le rapport
  params_list <- list(
    data_brut = data_brut   # Données brutes
  )
  
  # Ajouter les commentaires s'ils sont fournis
  if (!is.null(data_comment)) {
    params_list$data_comment <- data_comment
  }
  
  # Ajouter le tableau des résultats s'il est fourni
  if (!is.null(result_table)) {
    params_list$result_table <- result_table
  }
  
  # Générer le rapport Word
  rmarkdown::render(
    input = "report_template.Rmd",  # Chemin vers le fichier R Markdown
    output_file = output_file,
    params = params_list,
    envir = new.env(parent = globalenv())
  )
}


verifier_dataframes <- function(dataframe, nom_dataframe) {
  if (nrow(dataframe) == 0) {
    return(paste(nom_dataframe, "est vide."))
  }
  return(NULL)
}


calculate_mf_ratio <- function(male_count, female_count) {
  if (male_count == 0 && female_count == 0) {
    return(NA)  # Si les deux comptages sont 0, retourner NA
  }
  # Simplifier le ratio
  ratio <- MASS::fractions(c(male_count, female_count))
  return(paste0(ratio[1], ":", ratio[2]))
}

labelled_data <- function(data) {
  # Obtenir les labels des colonnes
  labels <- labelled::var_label(data)
  
  # Remplacer les noms des colonnes par leurs labels
  colnames(data) <- unlist(labels)
  
  return(data)
}

agemax <- function(data) {
  age_max <-
    max(na.omit(data$age)) #Trouver le plus vieil âge et ignorer les NA de votre jeu de données s’il en contient (sinon = erreur)
  age_max
}


death <- function(data, espece) {
  data %>%
    dplyr::filter(sp == espece) %>%
    droplevels() %>%
    dplyr::filter(!is.na(age))
}

get_zobs <- function(PP, death, agemax) {
  # Calcul de la mortalité selon plusieurs méthodes
  mortalite <- agesurv(
    type = 1,
    age = death$age,
    full = PP,
    last = agemax,
    estimate = "z",
    method = c("he", "lr", "wlr", "cr", "crcb", "pois")
  )
  
  # Extraction de la valeur de Z pour la méthode "cr"
  zobs <- mortalite$results[4, "Estimate"]
  
  return(zobs)
}

#Fonctions pour Maturite sexuelle

## Osius and Rojek GOODNESS-OF-FIT test
o.r.test <- function(obj) {
  mf <- obj$model
  trials <- rep(1, times = nrow(mf))
  if(any(colnames(mf) == "(weights)")) 
    trials <- mf[[ncol(mf)]]
  prop <- mf[[1]]
  # the double bracket (above) gets the index of items within an object
  if (is.factor(prop)) 
    prop = as.numeric(prop) == 2  # Converts 1-2 factor levels to logical 0/1 values
  pi.hat <- obj$fitted.values 
  y <- trials*prop
  yhat <- trials*pi.hat
  nu <- yhat*(1-pi.hat)
  pearson <- sum((y - yhat)^2/nu)
  cc <- (1 - 2*pi.hat)/nu
  exclude <- c(1,which(colnames(mf) == "(weights)"))
  vars <- data.frame(cc, mf[,-exclude]) 
  wlr <- lm(formula = cc ~ ., weights = nu, data = vars)
  rss <- sum(nu*residuals(wlr)^2 )
  J <- nrow(mf)
  A <- 2*(J - sum(1/trials))
  z <- (pearson - (J - ncol(vars) - 1))/sqrt(A + rss)
  p.value <- 2*(1 - pnorm(abs(z)))
  return(p.value)
}

## Fonction IC
confint_L <- function(object, p = 0.5, cf = 1:2, level = 0.95, nboot = 10000,
                      method = c("delta","fieller","proflik",
                                 "parboot","nonparboot",
                                 "bayesian","montecarlo"),
                      interval_type = c("eti","hdi","bca","all"), ...) {
  method <- match.arg(method)
  if(method %in% c("parboot","nonparboot","montecarlo")) {
    interval_type <- match.arg(interval_type)
  }
  switch(method,
         "delta" = ld_delta(object = object, p = p, cf = cf, level = level),
         "fieller" = ld_fieller(object = object, cf = cf, p = p, level = level),
         "proflik" = ld_proflik(object = object, cf = cf, p = p, level = level, ...),
         "parboot" = ld_boot(object = object, cf = cf, p = p, level = level, nboot = nboot, interval_type = interval_type),
         "nonparboot" = ld_boot_nonpar(object = object, cf = cf, p = p, level = level, nboot = nboot, interval_type = interval_type),
         "bayesian" = ld_bayesian(object = object, cf = cf, p = p, level = level, ...),
         "montecarlo" = ld_montecarlo(object = object, cf = cf, p = p, level = level, nboot = nboot, interval_type = interval_type))
}

## Delta Method
ld_delta <- function(object, p, cf, level) {
  dose_object <- MASS::dose.p(object, p = p, cf = cf)
  parm <- seq_along(dose_object)
  nam <- names(dose_object)[parm]
  se <- attr(dose_object, "SE")[parm]
  p <- attr(dose_object, "p")[parm]
  dose_object <- as.vector(dose_object[parm])
  z <- sqrt(qchisq(level, 1))
  res <- cbind(lower = dose_object - z*se,
               estimate = dose_object,
               upper = dose_object + z*se)
  row.names(res) <- paste("p = ", format(p), ":", sep = "")
  return(res)
}

## Fieller's Method
fieller_ofun <- function(xt, b, xi, v, chi2) {
  (b[1] + b[2]*xt - xi)^2/(v[1,1] + 2*xt*v[1,2] + xt^2*v[2,2]) - chi2
}


fieller_ci <- function(b, xi, v, chi2) {
  xhat <- (xi - b[1])/b[2]
  xl <- xhat - 1
  maxit <- 10000
  iter <- 1
  while(fieller_ofun(xl, b, xi, v, chi2) < 0) {
    xl <- xl - 1
    iter <- iter + 1
    if(iter > maxit) stop("maximum number of iterations exceeded")
  }
  low <- uniroot(fieller_ofun, interval = c(xl, min(xhat, xl + 1)),
                 b = b, xi = xi, v = v, chi2 = chi2)$root
  xu <- xhat + 1
  iter <- 1
  while(fieller_ofun(xu, b, xi, v, chi2) < 0) {
    xu <- xu + 1
    iter <- iter + 1
    if(iter > maxit) stop("maximum number of iterations exceeded")
  }
  upp <- uniroot(fieller_ofun, interval = c(max(xhat, xu - 1), xu),
                 b = b, xi = xi, v = v, chi2 = chi2)$root
  return(c(lower = as.vector(low), estimate = as.vector(xhat), upper = as.vector(upp)))
}

ld_fieller <- function(object, cf = 1:2, p = 0.5, level = 0.95) {
  b <- coef(object)[cf]
  V <- vcov(object)[cf, cf]
  xiv <- family(object)$linkfun(p)
  chi2 <- qchisq(level, df = 1)
  
  R <- NULL
  for(xi in xiv) {
    R <- rbind(R, fieller_ci(b, xi, V, chi2))
  }
  row.names(R) <- paste("p = ", format(p), ":", sep = "")
  structure(R, p = p, class = "Fieller")
}

print.Fieller <- function(x, ...) {
  attr(x, "p") <- class(x) <- NULL
  NextMethod("print", x, ...)
}

## Profile likelihood

prof_ofun <- Vectorize(function(theta_p, fam, Y, X0, x, etastart, wts, control, chi2, D0, off) {
  glm.fit(x = cbind(X0, x-theta_p), y = Y, weights = wts,
          etastart = etastart, offset = off, family = fam,
          control = control, intercept = TRUE)$deviance - D0 - chi2
}, "theta_p")

prof_ci <- function(b, xi, fam, Y, X0, x, etastart, wts, control, chi2, D0, off) {
  theta_p <- (xi - b[1])/b[2]
  theta_p_l <- theta_p-1
  iter <- 1
  maxit <- 10000
  while(prof_ofun(theta_p_l, fam, Y, X0, x, etastart, wts, control, chi2, D0, off) < 0) {
    theta_p_l <- theta_p_l - 1
    iter <- iter + 1
    if(iter > maxit) stop("maximum number of iterations exceeded")
  }
  low <- uniroot(prof_ofun, interval = c(theta_p_l, min(theta_p, theta_p_l+1)),
                 fam = fam, Y = Y, X0 = X0, x = x, etastart = etastart,
                 wts = wts, control = control, chi2 = chi2, D0 = D0, off = off)$root
  theta_p_u <- theta_p+1
  iter <- 1
  while(prof_ofun(theta_p_u, fam, Y, X0, x, etastart, wts, control, chi2, D0, off) < 0) {
    theta_p_u <- theta_p_u + 1
    iter <- iter + 1
    if(iter > maxit) stop("maximum number of iterations exceeded")
  }
  upp <- uniroot(prof_ofun, interval = c(max(theta_p, theta_p_u-1), theta_p_u),
                 fam = fam, Y = Y, X0 = X0, x = x, etastart = etastart,
                 wts = wts, control = control, chi2 = chi2, D0 = D0, off = off)$root
  c(lower = as.vector(low), estimate = as.vector(theta_p), upper = as.vector(upp))
}

prof <- function(b, xi, R, fam, Y, X0, x, etastart, wts, control, chi2, D0, off, ...) {
  R <- R[1,]
  theta_p <- (xi - b[1])/b[2]
  inc <- diff(range(R))
  dc <- D0 + chi2
  dev.x <- seq(theta_p - inc, theta_p + inc, length=50)
  dev.y <- prof_ofun(dev.x, fam, Y, X0, x, etastart, wts, control, chi2, D0, off) + dc
  plot(dev.x, dev.y, type="l", ylab="Deviance", las=1, ...)
  abline(h=dc, lty=2)
  arrows(R, c(0,0,0), R, c(dc,min(dev.y),dc), 
         code = 3, length = 0, lty = 2)
  points(R[c(1,3)], rep(par("usr")[3], 2), xpd = TRUE, pch = 16, cex = .8)
  points(R[2], par("usr")[3], xpd = TRUE, pch = "*", cex = 1.8)
  #axis(1, at = R[c(1,3)], labels = FALSE)
  text(x = R[c(1,3)], par("usr")[3], cex = .7,
       labels = c("lower limit","upper limit"), 
       xpd = TRUE, srt = 45, pos = 1)
}

ld_proflik <- function(object, cf = 1:2, p = 0.5, level = 0.95, profile = FALSE, ...) {
  fam <- family(object)
  Y <- object$y
  X <- model.matrix(object)
  X0 <- X[, -cf]
  x <- X[, cf[2]]
  b <- as.vector(coef(object)[cf])
  etastart <- object$linear.predictors
  wts <- weights(object)
  originalOffset <- if(is.null(o <- object$offset)) {
    0
  } else {
    o
  }
  control <- object$control
  xiv <- fam$linkfun(p)
  chi2 <- qchisq(level, 1)
  D0 <- deviance(object)
  R <- NULL
  for(xi in xiv) {
    off <- originalOffset + xi
    R <- rbind(R, prof_ci(b, xi, fam, Y, X0, x, etastart, wts, control, chi2, D0, off))
  }
  if(profile) {
    if(length(p) > 1) cat("Profile produced only for p = ", p[1], sep="", "\n")
    xi <- xiv[1]
    off <- originalOffset + xi
    prof(b, xi, R, fam, Y, X0, x, etastart, wts, control, chi2, D0, off, ...)
  }
  row.names(R) <- paste("p = ", format(p), ":", sep = "")
  structure(R, p = p, class = "LR_glm_dose")
}

print.LR_glm_dose <- function(x, ...) {
  attr(x, "p") <- class(x) <- NULL
  NextMethod("print", x, ...)
}

## Parametric Bootstrap

ld_boot <- function(object, p = 0.5, cf = 1:2, level = 0.95, nboot = 1000,
                    interval_type = c("eti","hdi")) {
  data <- object$data
  fmla <- formula(delete.response(terms(formula(object))))
  X <- model.matrix(fmla, data = data)
  original_d_hat <- MASS::dose.p(object, p = p, cf = cf)
  
  d_hat <- matrix(NA, ncol = length(p), nrow = nboot)
  
  for(i in 1:nboot) {
    new_y <- as.matrix(simulate(object))
    new_fit <- glm(new_y ~ 0 + X, family = object$family)
    d_hat[i,] <- as.numeric(MASS::dose.p(new_fit, p = p, cf = cf))
  }
  
  res <- switch(interval_type,
                "eti" = cbind(lower = apply(d_hat, 2, quantile, (1-level)/2),
                              estimate = original_d_hat,
                              upper = apply(d_hat, 2, quantile, (1+level)/2)),
                "hdi" = {
                  if(length(level) == 1) {
                    hdi_interval <- apply(d_hat, 2, hdi, level)
                    return(
                      cbind(lower = hdi_interval[1,],
                            estimate = original_d_hat,
                            upper = hdi_interval[2,])
                    )
                  } else {
                    hdi_interval <- list()
                    for(l in 1:length(level)) {
                      hdi_interval[[l]] <- as.numeric(apply(d_hat, 2, hdi, level[l]))
                    }
                    ret <- do.call(rbind, hdi_interval)
                    ret <- cbind(ret, rep(original_d_hat, length(level)))
                    ret <- ret[,c(1,3,2)]
                    colnames(ret) <- c("lower","estimate","upper")
                    return(ret)}
                },
                "bca" = {
                  bca_interval <- get_bca(object = object, p = p, cf = cf,
                                          d_hat = d_hat, original_d_hat = original_d_hat,
                                          nboot = nboot, level = level)
                  bca_interval <- matrix(bca_interval, ncol = 2, nrow = length(level), byrow = FALSE)
                  return(cbind(lower = bca_interval[,1],
                               estimate = original_d_hat,
                               upper = bca_interval[,2]))
                },
                "all" = {
                  perc <- cbind(lower = apply(d_hat, 2, quantile, (1-level)/2),
                                estimate = original_d_hat,
                                upper = apply(d_hat, 2, quantile, (1+level)/2))
                  if(length(level) == 1) {
                    hdi_interval <- apply(d_hat, 2, hdi, level)
                    hdi_int <- cbind(lower = hdi_interval[1,],
                                     estimate = original_d_hat,
                                     upper = hdi_interval[2,])
                  } else {
                    hdi_interval <- list()
                    for(l in 1:length(level)) {
                      hdi_interval[[l]] <- as.numeric(apply(d_hat, 2, hdi, level[l]))
                    }
                    ret <- do.call(rbind, hdi_interval)
                    ret <- cbind(ret, rep(original_d_hat, length(level)))
                    ret <- ret[,c(1,3,2)]
                    colnames(ret) <- c("lower","estimate","upper")
                    hdi_int <- ret
                  }
                  bca_interval <- get_bca(object = object, p = p, cf = cf,
                                          d_hat = d_hat, original_d_hat = original_d_hat,
                                          nboot = nboot, level = level)
                  bca_interval <- matrix(bca_interval, ncol = 2, nrow = length(level), byrow = FALSE)
                  bca_int <- cbind(lower = bca_interval[,1],
                                   estimate = original_d_hat,
                                   upper = bca_interval[,2])
                  return(list("eti" = perc, "hdi" = hdi_int, "bca" = bca_int))
                })
  return(res)
}

## Non-parametric Bootstrap

ld_boot_nonpar <- function(object, p = 0.5, cf = 1:2, level = 0.95, nboot = 1000,
                           interval_type = c("eti","hdi","bca","all")) {
  data <- object$data
  fmla <- formula(delete.response(terms(formula(object))))
  X <- model.matrix(fmla, data = data)
  original_d_hat <- MASS::dose.p(object, p = p, cf = cf)
  
  d_hat <- matrix(NA, ncol = length(p), nrow = nboot)
  
  for(i in 1:nboot) {
    sampled_rows <- sample(1:nrow(data), nrow(data), replace = TRUE)
    new_data <- data[sampled_rows,]
    new_fit <- update(object, data = new_data)
    d_hat[i,] <- as.numeric(MASS::dose.p(new_fit, p = p, cf = cf))
  }
  
  res <- switch(interval_type,
                "eti" = cbind(lower = apply(d_hat, 2, quantile, (1-level)/2),
                              estimate = original_d_hat,
                              upper = apply(d_hat, 2, quantile, (1+level)/2)),
                "hdi" = {
                  if(length(level) == 1) {
                    hdi_interval <- apply(d_hat, 2, hdi, level)
                    return(
                      cbind(lower = hdi_interval[1,],
                            estimate = original_d_hat,
                            upper = hdi_interval[2,])
                    )
                  } else {
                    hdi_interval <- list()
                    for(l in 1:length(level)) {
                      hdi_interval[[l]] <- as.numeric(apply(d_hat, 2, hdi, level[l]))
                    }
                    ret <- do.call(rbind, hdi_interval)
                    ret <- cbind(ret, rep(original_d_hat, length(level)))
                    ret <- ret[,c(1,3,2)]
                    colnames(ret) <- c("lower","estimate","upper")
                    return(ret)}
                },
                "bca" = {
                  bca_interval <- get_bca(object = object, p = p, cf = cf,
                                          d_hat = d_hat, original_d_hat = original_d_hat,
                                          nboot = nboot, level = level)
                  bca_interval <- matrix(bca_interval, ncol = 2, nrow = length(level), byrow = FALSE)
                  return(cbind(lower = bca_interval[,1],
                               estimate = original_d_hat,
                               upper = bca_interval[,2]))
                },
                "all" = {
                  perc <- cbind(lower = apply(d_hat, 2, quantile, (1-level)/2),
                                estimate = original_d_hat,
                                upper = apply(d_hat, 2, quantile, (1+level)/2))
                  if(length(level) == 1) {
                    hdi_interval <- apply(d_hat, 2, hdi, level)
                    hdi_int <- cbind(lower = hdi_interval[1,],
                                     estimate = original_d_hat,
                                     upper = hdi_interval[2,])
                  } else {
                    hdi_interval <- list()
                    for(l in 1:length(level)) {
                      hdi_interval[[l]] <- as.numeric(apply(d_hat, 2, hdi, level[l]))
                    }
                    ret <- do.call(rbind, hdi_interval)
                    ret <- cbind(ret, rep(original_d_hat, length(level)))
                    ret <- ret[,c(1,3,2)]
                    colnames(ret) <- c("lower","estimate","upper")
                    hdi_int <- ret
                  }
                  bca_interval <- get_bca(object = object, p = p, cf = cf,
                                          d_hat = d_hat, original_d_hat = original_d_hat,
                                          nboot = nboot, level = level)
                  bca_interval <- matrix(bca_interval, ncol = 2, nrow = length(level), byrow = FALSE)
                  bca_int <- cbind(lower = bca_interval[,1],
                                   estimate = original_d_hat,
                                   upper = bca_interval[,2])
                  return(list("eti" = perc, "hdi" = hdi_int, "bca" = bca_int))
                })
  return(res)
}

## Bayesian credible intervals

ld_jags <- function(object, cf, p, level, progress.bar = "none",
                    n.chains = 3, n.burnin = 500, n.iter = 1000, n.thin = 5) {
  
  the_link <- object$family$link
  
  if(the_link == "logit") {
    model_code <- "
model
{
  # likelihood
  for (i in 1:N) {
    y[i] ~ dbinom(p[i], m[i])
    logit(p[i]) <- inprod(X[i,], beta)
  }
  
  # priors
  for (i in 1:N_betas) {
    beta[i] ~ dnorm(0, 0.01)
  }
  
  ld <- (p_const - beta[cf[1]]) / beta[cf[2]]
}
"
  } else if(the_link == "probit") {
    model_code <- "
model
{
  # likelihood
  for (i in 1:N) {
    y[i] ~ dbinom(p[i], m[i])
    probit(p[i]) <- inprod(X[i,], beta)
  }
  
  # priors
  for (i in 1:N_betas) {
    beta[i] ~ dnorm(0, 0.01)
  }
  
  ld <- (p_const - beta[cf[1]]) / beta[cf[2]]
}
"
  } else if(the_link == "cloglog") {
    model_code <- "
model
{
  # likelihood
  for (i in 1:N) {
    y[i] ~ dbinom(p[i], m[i])
    p[i] <- 1 - exp(-exp(inprod(X[i,], beta)))
  }
  
  # priors
  for (i in 1:N_betas) {
    beta[i] ~ dnorm(0, 0.01)
  }
  
  ld <- (p_const - beta[cf[1]]) / beta[cf[2]]
}
"
  } else {
    stop("only implemented for logit, probit or cloglog") 
  }
  
  linkfun <- object$family$linkfun
  p_const <- linkfun(p)
  
  model_data <- list(y = as.numeric(object$y * object$prior.weights),
                     m = as.numeric(object$prior.weights),
                     X = model.matrix(object),
                     N = nrow(model.matrix(object)),
                     N_betas = ncol(model.matrix(object)),
                     cf = cf, p_const = p_const)
  
  model_parameters <- "ld"
  
  model_run <- jags(data = model_data,
                    parameters.to.save = model_parameters,
                    model.file = textConnection(model_code),
                    progress.bar = progress.bar,
                    n.chains = n.chains, n.burnin = n.burnin, n.iter = n.iter, n.thin = n.thin,
                    inits = list(list(beta = coef(object) + rnorm(model_data$N_betas, 0, .001)),
                                 list(beta = coef(object) + rnorm(model_data$N_betas, 0, .001)),
                                 list(beta = coef(object) + rnorm(model_data$N_betas, 0, .001))))
  
  ld_post <- model_run$BUGSoutput$sims.list$ld
  ld_quantile <- as.numeric(apply(ld_post, 2, quantile, prob = c((1-level)/2, (1+level)/2)))
  
  return(c(ld_quantile[1], mean(ld_post), ld_quantile[2]))
}

ld_bayesian <- function(object, cf, p, level, ...) {
  R <- matrix(NA, ncol = 3, nrow = length(p))
  colnames(R) <- c("lower","estimate","upper")
  for(i in 1:length(p)) {
    R[i,] <- ld_jags(object = object, cf = cf, p = p[i], level = level, ...)
  }
  row.names(R) <- paste("p = ", format(p), ":", sep = "")
  structure(R, p = p, class = "ld_bayesian")
}

print.ld_bayesian <- function(x, ...) {
  attr(x, "p") <- class(x) <- NULL
  NextMethod("print", x, ...)
}

## Monte Carlo

ld_montecarlo <- function(object, p = 0.5, cf = 1:2, level = 0.95, nboot = 1000,
                          interval_type = c("eti","hdi","bca","all")) {
  original_d_hat <- MASS::dose.p(object, p = p, cf = cf)
  varcovar_mat <- vcov(object)[cf,cf]
  mean_vec <- coef(object)[cf]
  
  beta_sim <- mvtnorm::rmvnorm(n = nboot, mean = mean_vec, sigma = varcovar_mat)
  
  linkfun <- object$family$linkfun
  p_const <- linkfun(p)
  
  d_hat <- matrix(NA, ncol = length(p), nrow = nboot)
  
  for(i in 1:length(p)) {
    d_hat[,i] <- (p_const[i] - beta_sim[,1]) / beta_sim[,2]
  }
  
  res <- switch(interval_type,
                "eti" = cbind(lower = apply(d_hat, 2, quantile, (1-level)/2),
                              estimate = original_d_hat,
                              upper = apply(d_hat, 2, quantile, (1+level)/2)),
                "hdi" = {
                  if(length(level) == 1) {
                    hdi_interval <- apply(d_hat, 2, hdi, level)
                    return(
                      cbind(lower = hdi_interval[1,],
                            estimate = original_d_hat,
                            upper = hdi_interval[2,])
                    )
                  } else {
                    hdi_interval <- list()
                    for(l in 1:length(level)) {
                      hdi_interval[[l]] <- as.numeric(apply(d_hat, 2, hdi, level[l]))
                    }
                    ret <- do.call(rbind, hdi_interval)
                    ret <- cbind(ret, rep(original_d_hat, length(level)))
                    ret <- ret[,c(1,3,2)]
                    colnames(ret) <- c("lower","estimate","upper")
                    return(ret)}
                },
                "bca" = {
                  bca_interval <- get_bca(object = object, p = p, cf = cf,
                                          d_hat = d_hat, original_d_hat = original_d_hat,
                                          nboot = nboot, level = level)
                  bca_interval <- matrix(bca_interval, ncol = 2, nrow = length(level), byrow = FALSE)
                  return(cbind(lower = bca_interval[,1],
                               estimate = original_d_hat,
                               upper = bca_interval[,2]))
                },
                "all" = {
                  perc <- cbind(lower = apply(d_hat, 2, quantile, (1-level)/2),
                                estimate = original_d_hat,
                                upper = apply(d_hat, 2, quantile, (1+level)/2))
                  if(length(level) == 1) {
                    hdi_interval <- apply(d_hat, 2, hdi, level)
                    hdi_int <- cbind(lower = hdi_interval[1,],
                                     estimate = original_d_hat,
                                     upper = hdi_interval[2,])
                  } else {
                    hdi_interval <- list()
                    for(l in 1:length(level)) {
                      hdi_interval[[l]] <- as.numeric(apply(d_hat, 2, hdi, level[l]))
                    }
                    ret <- do.call(rbind, hdi_interval)
                    ret <- cbind(ret, rep(original_d_hat, length(level)))
                    ret <- ret[,c(1,3,2)]
                    colnames(ret) <- c("lower","estimate","upper")
                    hdi_int <- ret
                  }
                  bca_interval <- get_bca(object = object, p = p, cf = cf,
                                          d_hat = d_hat, original_d_hat = original_d_hat,
                                          nboot = nboot, level = level)
                  bca_interval <- matrix(bca_interval, ncol = 2, nrow = length(level), byrow = FALSE)
                  bca_int <- cbind(lower = bca_interval[,1],
                                   estimate = original_d_hat,
                                   upper = bca_interval[,2])
                  return(list("eti" = perc, "hdi" = hdi_int, "bca" = bca_int))
                })
  return(res)
}

get_bca <- function(object, p, cf, d_hat, original_d_hat, nboot, level) {
  alpha <- c((1-level)/2, level + (1-level)/2)
  n <- nrow(object$data)
  u <- rep(0, n)
  for(i in 1:n) {
    u[i] <- as.numeric(MASS::dose.p(update(object, data = object$data[-i,]), p = p, cf = cf))
  }
  z0 <- qnorm(sum(d_hat < original_d_hat)/nboot)
  uu <- mean(u) - u
  acc <- sum(uu * uu * uu)/(6 * (sum(uu * uu))^1.5)
  zalpha <- qnorm(alpha)
  tt <- pnorm(z0 + (z0 + zalpha)/(1 - acc * (z0 + zalpha)))
  confpoints <- as.numeric(quantile(x = d_hat, probs = tt, type = 1))
  return(confpoints)
}


library(expss)

afficher_avec_labels <- function(df) {
  df_temp <- df  # Copie du data.frame pour ne pas modifier l'original
  names(df_temp) <- sapply(df, var_lab)  # Remplace les noms par les labels
  print(df_temp)  # Affiche le data.frame avec les labels
}


get_best_L50_model <- function(best_L50, sexe = "M") {
  # Vérification de l'entrée
  if (!sexe %in% c("M", "F")) {
    stop("Le paramètre 'sexe' doit être soit 'M' soit 'F'.")
  }
  
  # Construction du nom de l'élément à récupérer
  model_name <- paste0("best_model_", sexe)
  
  # Extraction du modèle correspondant
  if (model_name %in% names(best_L50)) {
    return(best_L50[[model_name]])
  } else {
    stop("Le modèle spécifié n'existe pas dans la liste fournie.")
  }
}




