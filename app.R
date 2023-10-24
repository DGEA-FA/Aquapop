# librairies --------------------------------------------------------------
library(shiny)
library(car)
library(kableExtra)
library(reactable)
library(FSA)
library(nlstools)
library(shinyBS) #modal
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

# to_activate_before_play -------------------------------------------------
#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
reactlog::reactlog_enable() #to see the reactive graph
Sys.setlocale("LC_TIME", "French")  #https://stackoverflow.com/questions/39340185/how-to-set-the-default-language-of-date-in-r
# on utilise https://style.tidyverse.org/index.html
options(knitr.kable.NA = '-')





shinyApp(app_ui, app_server)