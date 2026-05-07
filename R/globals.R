Sys.setlocale("LC_TIME", "French")  # Pour définir le format de la date en français
options(shiny.maxRequestSize = 10 * 1024^2)  # Limite de la taille des fichiers uploadés
myspinner <- 6

utils::globalVariables(c(

  "quantile", "hdi", "jags"
))

