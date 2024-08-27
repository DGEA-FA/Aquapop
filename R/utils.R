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


gt_ltmpoidsage <- function(data) {
  data %>%
    gt() %>%
    tab_header(
      title = md("**Aperçu des données morphologiques**")
    ) %>%
    cols_label(
      Sexe = "Groupe",
      ltm_N = "N",
      ltm_Moyenne = "Moy.",
      ltm_ET = "ET",
      ltm_Minimum = "Min",
      ltm_Maximum = "Max",
      masse_N = "N",
      masse_Moyenne = "Moy.",
      masse_ET = "ET",
      masse_Minimum = "Min",
      masse_Maximum = "Max",
      age_N = "N",
      age_Moyenne = "Moy.",
      age_ET = "ET",
      age_Minimum = "Min",
      age_Maximum = "Max"
    ) %>%
    tab_spanner(
      label = "LTMax (mm)",
      columns = c(ltm_N, ltm_Moyenne, ltm_ET, ltm_Minimum, ltm_Maximum)
    ) %>%
    tab_spanner(
      label = "Masse (g)",
      columns = c(masse_N, masse_Moyenne, masse_ET, masse_Minimum, masse_Maximum)
    ) %>%
    tab_spanner(
      label = "Âge",
      columns = c(age_N, age_Moyenne, age_ET, age_Minimum, age_Maximum)
    ) %>%
    cols_align(
      align = "center",
      columns = everything()
    ) %>%
    fmt_number(
      columns = c(ltm_Moyenne, ltm_ET, ltm_Minimum, ltm_Maximum, 
                  masse_Moyenne, masse_ET, masse_Minimum, masse_Maximum,
                  age_Moyenne, age_ET, age_Minimum, age_Maximum),
      decimals = 2
    ) %>%
    tab_options(
      table.width = pct(100),
      table.font.size = px(12)
    )
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
           caption = "Comparaison des modèles : tous les spécimens ", 
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
           caption = "Comparaison des modèles : femelles reproductrices actives ", 
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
           caption = "Table de sélection des modèles de l’estimation de la mortalité", 
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
           caption = "Estimations obtenues à partir du modèle de Robson-Chapman (à titre comparatif seulement)", 
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