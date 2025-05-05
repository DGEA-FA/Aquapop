# source("R/load_packages.R")
devtools::load_all()

# source("R/utils.R")

# to_activate_before_play ----
reactlog::reactlog_enable() # Pour voir le graphe des réactifs
Sys.setlocale("LC_TIME", "French")  # Pour définir le format de la date en français
options(shiny.maxRequestSize = 10 * 1024^2)  # Limite de la taille des fichiers uploadés

shinyApp(
  ui = app_ui,
  server = app_server
)

# reactlogShow()
