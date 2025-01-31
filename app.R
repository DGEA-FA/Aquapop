# librairies --------------------------------------------------------------
library(shiny)
library(car)
library(DT)
library(kableExtra)
library(reactable)
library(FSA)
library(nlstools)
library(shinyBS)
library(gghighlight)
library(htmltools)
library(markdown)
library(readxl)
library(ggplot2)
library(scales)
library(dplyr)
library(patchwork)
library(reactlog)
library(stringr)
library(chron)
library(purrr)
library(writexl)
library(shinycssloaders)
library(glue)
library(fishmethods)
library(hnp)
library(MASS)
library(glmmTMB)
library(MuMIn)
library(plotly)
library(gapminder)
library(AER)
library(pROC)
library(DescTools)
library(emdbook)
library(AICcmodavg)
library(investr)

library(gt)
library(officer)
library(flextable)
library(forcats)
library(labelled)


# to_activate_before_play -------------------------------------------------
reactlog::reactlog_enable() # Pour voir le graphe des réactifs
Sys.setlocale("LC_TIME", "French")  # Pour définir le format de la date en français
options(knitr.kable.NA = '-')  # Options pour la fonction kable de knitr
options(shiny.maxRequestSize = 10 * 1024^2)  # Limite de la taille des fichiers uploadés

# Votre application Shiny commence ici
shinyApp(
  ui = app_ui,
  server = app_server
)
