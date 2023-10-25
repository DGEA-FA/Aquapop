instructions_upload <- c("Le fichier doit être en format *.xlsx, conformément à la procédure d'extraction de IFA (fourni par la DEFA).",
                         "Sans modification de votre part, le fichier excel extrait est déjà formatté pour l'Application",
                         "Les onglets du classeur excel doivent être dans un ordre précis, soit : Lac, Stations, Recolte, Specimens, Profil, Paramètres.",
                         "Dans chaque onglet, les colonnes doivent également être dans un ordre précis. Voir les onglets pour un exemple de chaque feuille.", 
                         "Les variantes de données absentes (i.e. NA, '', ' ', NULL, NaN) seront toutes transformées en NA dans R",
                         " **dire que les données excel doivent toutes être en format texte à leur sortie de IFA (leur format sera modifié dans R)"
                        )


brut_options <- list(pageLength = 10, autoWidth = TRUE, searching = FALSE)

bienvenue <- "Yo les chumz, on mets le texte d'accueil ici"

myspinner <- 6


agemax <- function(data) {
  age_max <- max(na.omit(data$age)) #Trouver le plus vieil âge et ignorer les NA de votre jeu de données s’il en contient (sinon = erreur)
  age_max
}



death <- function(data, espece) {
  
  death <- data %>%  dplyr::filter(sp == espece)  %>% droplevels() 
  #death <- death %>% filter(CARCT_ENG == "Exp") %>% droplevels() #on lavait mis dans Nord, aussi le cas ici ? 
  death <- subset(death, !is.na(age) ) # this data frame needed to be “cleaned”
}

peakplus <- function(data) {
  
  #Largement inspiré de Guide de normalisation et manuel JMainguy
  PeakPlus <- function() {
    uniqv <- unique(data$age)
    Peak <- uniqv[which.max(tabulate(match(data$age, uniqv)))] 
    Peak + 1 }     
  
  PP <- PeakPlus()
  PP
  
}



kable_ltmpoidsage <- function(data) {
  req(data)
  data %>% 
    kable( align = c("r","r","c","c","c","c","r"),
           caption = "Aperçu des données morphologiques"
    ) %>%
    kable_styling(full_width = FALSE,
                  #lightable_options = "basic",
                  font_size = 12,
                  html_font="sans-serif", 
                  position="center") %>% 
    column_spec(2, #sexe
                border_right = TRUE) %>% 
    kableExtra::collapse_rows(columns = 1, valign = "top")

}

kable_wri <- function(data) {
  req(data)
  data %>% 
    kable( align = c("r","c","c","c","c","c","c","c","c","r"),
           caption = "Indice de masse relative (Wr)"
    ) %>%
    kable_styling(full_width = FALSE,
                  #lightable_options = "basic",
                  font_size = 12,
                  html_font="sans-serif", 
                  position="center") %>% 
    column_spec(1, #row
                border_right = TRUE) %>% 
    column_spec(2, #tous
                border_right = TRUE) %>% 
    column_spec(4, #male
                border_right = TRUE)  
  
  # %>%      kableExtra::collapse_rows(columns = 1, valign = "top")
  
}

kable_abondance <- function(data) {
  req(data)
  data %>% 
    kable( align = c("r","c","c","c","c","r"),
           caption = "Abondance"
    ) %>%
    kable_styling(full_width = FALSE,
                  #lightable_options = "basic",
                  font_size = 12,
                  html_font="sans-serif", 
                  position="center") %>% 
  kableExtra::row_spec(1, extra_css = "border-bottom: 0.5px solid") %>%
    kableExtra::row_spec(4, extra_css = "border-bottom: 0.5px solid")  %>%
    kableExtra::row_spec(8, extra_css = "border-bottom: 0.5px solid")
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














