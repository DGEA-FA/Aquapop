brut_options <- list(pageLength = 10, autoWidth = TRUE, searching = FALSE)
myspinner <- 6



kable_psd1 <- function(data) {
  req(data)
  data %>% 
    kable( align = c("c","c"),
           caption = "Nombre et fréquence relative (%) des classes de taille du PSD", 
           row.names = FALSE) %>%
    kable_styling(full_width = FALSE,
                  font_size = 12,
                  html_font="sans-serif", 
                  position="left") 
}
kable_psd2 <- function(data) {
  req(data)
  data %>% 
    kable( align = c("r","c","c","r"),
           caption = "Fréquence relative (%) de chaque classe de taille", 
           row.names = FALSE) %>%
    kable_styling(full_width = FALSE,
                  font_size = 12,
                  html_font="sans-serif", 
                  position="left") 
}
kable_ltmpoidsage <- function(data) {
  req(data)
  colnames(data)[1] <- 'Groupe' #renommer la 1 colonne
  colnames(data)[2] <- 'N'
  colnames(data)[3] <- 'Moy.'
  colnames(data)[4] <- 'ET'
  colnames(data)[5] <- 'Min'
  colnames(data)[6] <- 'Max'
  colnames(data)[7] <- 'N'
  colnames(data)[8] <- 'Moy.'
  colnames(data)[9] <- 'ET'
  colnames(data)[10] <- 'Min'
  colnames(data)[11] <- 'Max'
  colnames(data)[12] <- 'N'
  colnames(data)[13] <- "Moy."
  colnames(data)[14] <- 'ET'
  colnames(data)[15] <- 'Min'
  colnames(data)[16] <- 'Max'
  data %>% 
    kable( align = c("r","c","c","c","c","c","c","c","c","c","c","c","c","c","c","r"),
           caption = "Aperçu des données morphologiques"
    ) %>%
    kable_styling(full_width = FALSE,
                  font_size = 12,
                  html_font="sans-serif", 
                  position="center") %>% 
    column_spec(1, #sexe
                border_right = TRUE) %>% 
    add_header_above(c(" ", "LTMax (mm)" = 5, "Masse (g)" = 5, "Âge" = 5)) %>%
    kableExtra::collapse_rows(columns = 1, valign = "top")  %>%
    kableExtra::row_spec(1, extra_css = "border-bottom: 0.5px solid") %>%
    kableExtra::row_spec(4, extra_css = "border-bottom: 0.5px solid") %>%
    kableExtra::row_spec(8, extra_css = "border-bottom: 0.5px solid")
}

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

kable_abondance <- function(data) {
  req(data)
  data %>%
    kable(align = c("r", "c", "c", "c", "c", "r"),
          caption = "Abondance") %>%
    kable_styling(
      full_width = FALSE,
      font_size = 12,
      html_font = "sans-serif",
      position = "center"
    ) # %>%
    # kableExtra::row_spec(1, extra_css = "border-bottom: 0.5px solid") %>%
    # kableExtra::row_spec(4, extra_css = "border-bottom: 0.5px solid")  %>%
    # kableExtra::row_spec(8, extra_css = "border-bottom: 0.5px solid")
}

kable_biomasse <- function(data) {
  req(data)
  data %>%
    kable(
      align = c("r", "c", "c", "c", "r"),
      caption = "Biomasse",
      row.names = FALSE
    ) %>%
    kable_styling(
      full_width = FALSE,
      font_size = 12,
      html_font = "sans-serif",
      position = "center"
    ) %>%
    kableExtra::row_spec(1, extra_css = "border-bottom: 0.5px solid") %>%
    kableExtra::row_spec(4, extra_css = "border-bottom: 0.5px solid")  %>%
    kableExtra::row_spec(8, extra_css = "border-bottom: 0.5px solid")
}


kable_CPUEtous <- function(data) {
  req(data)
  data %>% 
    kable( align = c("r","c","c","c","r"),
           caption = "Comparaison de modèles CPUE tous", 
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
           caption = "Comparaison de modèles Reprod. actifs ♀", 
           row.names = FALSE) %>%
    kable_styling(full_width = FALSE,
                  font_size = 12,
                  html_font="sans-serif", 
                  position="center") 
}

kable_mortalite1 <- function(data) {
  req(data)
  data %>% 
    kable( align = c("r","c","c","c","c","c","c","c","c","r"),
           caption = "Sélection de modèles mortalité TITRE TBD", 
           row.names = FALSE    ) %>%
    kable_styling(full_width = FALSE,
                  font_size = 12,
                  html_font="sans-serif", 
                  position="center") 
}

kable_mortalite2 <- function(data) {
  req(data)
  data %>% 
    kable( align = c("r","c","r"),
           caption = "Info pour Robson-Chapman TITRE TBD", 
           row.names = FALSE    ) %>%
    kable_styling(full_width = FALSE,
                  #lightable_options = "basic",
                  font_size = 12,
                  html_font="sans-serif", 
                  position="center") 
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