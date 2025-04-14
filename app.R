source("R/load_packages.R")

source("R/utils.R")
source("./texte/text_elements.R", local = TRUE)   # Load the text elements

# to_activate_before_play ----
reactlog::reactlog_enable() # Pour voir le graphe des réactifs
Sys.setlocale("LC_TIME", "French")  # Pour définir le format de la date en français
options(knitr.kable.NA = '-')  # Options pour la fonction kable de knitr
options(shiny.maxRequestSize = 10 * 1024^2)  # Limite de la taille des fichiers uploadés

shinyApp(
  ui = app_ui,
  server = app_server
)

# reactlogShow()
