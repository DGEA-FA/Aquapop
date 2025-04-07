# Chargement des bibliothèques --------------------------------------------

library(tidyverse)
library(MASS)    # Pour glm avec binomial
library(MuMIn)   # Pour AICc
library(mvtnorm) # Pour méthodes Monte Carlo
library(FSA)     # Pour Summarize()
library(glue)    # Pour mise en forme des chaînes de caractères
library(ggplot2) # Pour visualisation des données

# Définition des fonctions ------------------------------------------------

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

#### Fonction IC ####
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
  dose_object <- dose.p(object, p = p, cf = cf)
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
  original_d_hat <- dose.p(object, p = p, cf = cf)
  
  d_hat <- matrix(NA, ncol = length(p), nrow = nboot)
  
  for(i in 1:nboot) {
    new_y <- as.matrix(simulate(object))
    new_fit <- glm(new_y ~ 0 + X, family = object$family)
    d_hat[i,] <- as.numeric(dose.p(new_fit, p = p, cf = cf))
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
  original_d_hat <- dose.p(object, p = p, cf = cf)
  
  d_hat <- matrix(NA, ncol = length(p), nrow = nboot)
  
  for(i in 1:nboot) {
    sampled_rows <- sample(1:nrow(data), nrow(data), replace = TRUE)
    new_data <- data[sampled_rows,]
    new_fit <- update(object, data = new_data)
    d_hat[i,] <- as.numeric(dose.p(new_fit, p = p, cf = cf))
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
  original_d_hat <- dose.p(object, p = p, cf = cf)
  varcovar_mat <- vcov(object)[cf,cf]
  mean_vec <- coef(object)[cf]
  
  beta_sim <- rmvnorm(n = nboot, mean = mean_vec, sigma = varcovar_mat)
  
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
    u[i] <- as.numeric(dose.p(update(object, data = object$data[-i,]), p = p, cf = cf))
  }
  z0 <- qnorm(sum(d_hat < original_d_hat)/nboot)
  uu <- mean(u) - u
  acc <- sum(uu * uu * uu)/(6 * (sum(uu * uu))^1.5)
  zalpha <- qnorm(alpha)
  tt <- pnorm(z0 + (z0 + zalpha)/(1 - acc * (z0 + zalpha)))
  confpoints <- as.numeric(quantile(x = d_hat, probs = tt, type = 1))
  return(confpoints)
}


# donnees ------------------------------------------------------------------

# Jeu de données fourni
# Création du dataframe df_l50 # Répartition exacte de PENOF 05533 2009
df_l50 <- data.frame(
  ltm = c(140, 224, 242, 255, 274, 296, 212, 201, 191, 177, 203, 184,
          166, 193, 194, 161, 184, 142, 180, 242, 229, 246, 266, 278,
          267, 264, 266, 244, 229, 220, 217, 205, 225, 259, 201, 212,
          205, 184, 199, 134, 258, 266, 242, 227, 223, 214, 199, 209,
          188, 177, 170, 166, 129, 280, 228, 205, 217, 185, 188, 212,
          196, 176, 164, 161, 150, 152, 146, 147, 131, 136, 133, 260,
          280, 180, 210, 288, 275, 261, 270, 128, 127, 174, 175, 185,
          200, 206, 227, 285, 240, 278, 264, 256, 254, 250, 220, 200,
          254, 222, 180, 175, 179, 190, 152, 122, 264, 249, 283, 275,
          265, 268, 226, 247, 195, 184, 181, 163, 170, 239, 248, 263,
          215, 210, 219, 172, 159, 155, 118, 246, 178, 230, 276, 218,
          220, 245, 233, 194, 176, 160, 149, 156, 147, 157, 140, 170,
          178, 177, 221, 224, 204, 185, 179, 176, 179, 217, 238, 185,
          169, 180, 169, 164, 159, 133, 241, 222, 247, 227, 230, 208,
          243, 226, 173, 178, 250, 163, 177, 156, 159, 184, 170, 237,
          265, 338, 243, 224, 236, 230, 241, 244, 263, 218, 189, 206,
          193, 191, 181, 155, 163, 151, 182, 168),
  sexe = c(rep( "F", times = 105), rep( "M", times = 95)),
  maturite = c("N", "O", "O", "O", "O", "O", "O", "O", "N", "O", "O", "N",
               "N", "N", "N", "N", "N", "N", "N", "O", "O", "O", "O", "O",
               "O", "N", "O", "O", "O", "O", "N", "N", "O", "O", "O", "N",
               "N", "N", "O", "N", "O", "O", "O", "O", "O", "O", "N", "N",
               "N", "N", "N", "N", "N", "O", "O", "O", "O", "N", "N", "O",
               "O", "N", "O", "N", "N", "N", "N", "N", "N", "N", "N", "O",
               "O", "N", "O", "O", "O", "O", "O", "N", "N", "N", "N", "N",
               "N", "N", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O",
               "O", "O", "N", "N", "N", "N", "N", "N", "O",
               "O", "O", "O", "O", "N", "O", "N", "N", "O", "O", "N", "N",
               "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "O",
               "O", "N", "N", "O", "O", "N", "O", "N", "N", "N", "N", "N",
               "N", "N", "N", "N", "N", "O", "N", "N", "O", "N", "N", "O",
               "N", "N", "N", "N", "O", "N", "N", "O", "N", "N", "N", "O",
               "N", "O", "N", "O", "N", "O", "O", "N", "O", "N", "N", "O",
               "N", "N", "O", "O", "O", "N", "O", "O", "N", "N", "N", "N",
               "O", "O", "O", "O", "O", "O", "O", "N", "O", "N", "O"
  )
)

# Organisation des niveaux du facteur maturité ---------------------------------

# Immature = niveau de référence
df_l50 <- df_l50 %>%
  mutate(
    maturite = factor(maturite, levels = c("N", "O"), ordered = TRUE),
    ltm = as.numeric(ltm),
    sexe = as.factor(sexe)
  )

# Ajustement du modèle TLO avec lien probit ----------------------------------------

modele <- glm(maturite ~ ltm, family = binomial(link = "probit"), data = df_l50)

# Tests d'ajustement du modèle ------------------------------------------------

## B.1 Osius and Rojek Standardizec Pearson X2 GOODNESS-OF-FIT test
pval.fit <- o.r.test(modele)

## B.2 GOODNESS-OF-LINK test of McCullagh and Nelder (1989) 
eta2 <- predict(modele)^2
model_eta2 <- update(modele, . ~ . + eta2)
pval.link <- anova(modele, model_eta2, test = "Chisq")$`Pr(>Chi)`[2]

# Vérification de la convergence du modèle et ajustement ----------------------

comment<-NA
if (modele$converged == FALSE) {
  comment <- "Ce modèle ne converge pas et devrait être rejeté."
}

if (is.na(comment) && (pval.fit < 0.05 || pval.link < 0.05)) {
  comment <- "Ce modèle ne s'ajuste pas bien aux données. Il est préférable de choisir un autre modèle"
}

# Sélection du modèle via AIC -------------------------------------------------

aicc<-AICc(modele)

### Détermination des valeurs de l50 ----------------------------------------------

# Estimation de l50 avec la fonction confint_L()

l50_ic <- confint_L(modele, method = "montecarlo", interval_type = "bca", nboot = 100000)

l50 <- l50_ic[2] %>% round(digits = 0)
li <- l50_ic[1]  %>% round(digits = 0)
ls <- l50_ic[3]  %>% round(digits = 0)

# Calcul des valeurs prédites pour la visualisation ----------------------------

ltmmin <- min(df_l50$ltm) %>% as.numeric()
ltmmax <- max(df_l50$ltm) %>% as.numeric()

newDF <- data.frame(ltm = seq(from = ltmmin, to = ltmmax, by = 1))

newDFpred <- predict(modele,
                     newDF,
                     full = TRUE,
                     type = "link",
                     se.fit = TRUE)

DATAogive <- newDF %>%
  mutate(
    maturite = pnorm(newDFpred$fit),
    lim_inf = pnorm(newDFpred$fit - (1.96 * newDFpred$se.fit)),
    lim_sup = pnorm(newDFpred$fit + (1.96 * newDFpred$se.fit))
  )


# Extraction des coefficients du modèle ----------------------------------------

b0 <- coef(modele)["(Intercept)"] 
b1 <- coef(modele)["ltm"] 

# Création d'une table récapitulative -----------------------------------------

minitable <- data.frame(
  l50 = l50,
  Intervalle = glue::glue("[{li}-{ls}]"),
  b0 = b0 %>% round(digits = 3),
  b1 = b1 %>% round(digits = 3)
)

# Génération du graphique ----------------------------------------------------

plot <- ggplot(data = DATAogive, aes(x = ltm, y = maturite)) +
  geom_line(color = "black") +
  geom_ribbon(aes(ymin = lim_inf, ymax = lim_sup), alpha = 0.1, fill = "blue") +
  annotate("segment", x = l50, xend = l50, y = 0, yend = 0.5, color = "black", lty = 2) +
  annotate("segment", x = ltmmin, xend = l50, y = 0.5, yend = 0.5, color = "black", lty = 2) +
  geom_point(data = df_l50 ,
             mapping = aes(x = ltm, y = as.numeric(maturite)-1, color = "blue"), alpha = 0.5) +
  theme_classic() +
  labs(x = "Longueur totale maximale (mm)", y = "Proportion reproducteurs actifs", 
       title = glue("Ogive de maturité")) +
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        legend.position = "none")
plot

# Incoherence L50 ----------------------------------------------------

# L50 estimé par confint_L()
cat("L50 estimé par confint_L() :", l50, "\n")
cat("Intervalle de confiance (IC 95%) de confint_L() : [", li, "-", ls, "]\n")

# L50 estimé visuellement (en cherchant l'intersection y = 0.5 sur le graphique) 
L50_manual <- DATAogive %>%
  filter(abs(maturite - 0.5) == min(abs(maturite - 0.5))) %>%
  pull(ltm) %>%
  mean()  # Moyenne si plusieurs valeurs sont proches de 0.5

cat("L50 estimé visuellement : ", round(L50_manual, 2), "\n")

# L50 calculé à partir des coefficients b0 et b1
L50_coeff <- round(-b0/b1)
cat("L50 estimé à partir des coefficients : ", L50_coeff, "\n")

# Comparaison
if (!(l50 == L50_manual && L50_manual == L50_coeff)) {
  cat("\n ⚠  Incohérence détectée entre les L50 estimés selon les différentes méthodes. \n")
}


