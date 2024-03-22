# librairies --------------------------------------------------------------
# Package names
packages <-
  c(
    "shiny",
    "car",
    "DT",
    "kableExtra",
    "reactable",
    "FSA",
    "nlstools",
    "shinyBS",
    "gghighlight",
    "htmltools",
    "markdown",
    "readxl",
    "ggplot2",
    "scales",
    "dplyr",
    "patchwork",
    "reactlog",
    "stringr",
    "chron",
    "purrr",
    "writexl",
    "shinycssloaders",
    "glue",
    "fishmethods",
    "hnp",
    "MASS",
    "glmmTMB",
    "MuMIn",
    "plotly",
    "gapminder",
    "AER"
  )

# # Install packages not yet installed
# installed_packages <- packages %in% rownames(installed.packages())
# if (any(installed_packages == FALSE)) {
#   install.packages(packages[!installed_packages])
# }
# 
# # Packages loading
# invisible(lapply(packages, library, character.only = TRUE))

# Install and load packages
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  suppressMessages(library(pkg, character.only = TRUE))
}



# to_activate_before_play -------------------------------------------------
reactlog::reactlog_enable() #to see the reactive graph
Sys.setlocale("LC_TIME", "French")  #https://stackoverflow.com/questions/39340185/how-to-set-the-default-language-of-date-in-r
# on utilise https://style.tidyverse.org/index.html
options(knitr.kable.NA = '-')
options(shiny.maxRequestSize = 10 * 1024^2)


shinyApp(app_ui, app_server)