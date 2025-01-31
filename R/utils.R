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
      ltm_nb = "N",
      ltm_moy = "Moy.",
      ltm_e_t = "ET",
      ltm_min = "Min",
      ltm_max = "Max",
      masse_nb = "N",
      masse_moy = "Moy.",
      masse_e_t = "ET",
      masse_min = "Min",
      masse_max = "Max",
      age_nb = "N",
      age_moy = "Moy.",
      age_e_t = "ET",
      age_min = "Min",
      age_max = "Max"
    ) %>%
    tab_spanner(
      label = "LTMax (mm)",
      columns = c(ltm_nb, ltm_moy, ltm_e_t, ltm_min, ltm_max)
    ) %>%
    tab_spanner(
      label = "Masse (g)",
      columns = c(masse_nb, masse_moy, masse_e_t, masse_min, masse_max)
    ) %>%
    tab_spanner(
      label = "Âge",
      columns = c(age_nb, age_moy, age_e_t, age_min, age_max)
    ) %>%
    cols_align(
      align = "center",
      columns = everything()
    ) %>%
    
    tab_options(
      table.width = pct(100),
      table.font.size = px(10)
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



gt_abondance <- function(data) {
  # Extraire les labels des colonnes
  column_labels <- sapply(data, function(col) attr(col, "label"))
  
  data %>%
    gt() %>%
    tab_header(
      title = md("**Tableau d'abondance**")
    ) %>%
    # Utiliser les labels extraits dans cols_label
    cols_label(
      group = column_labels["group"],
      abundance = column_labels["abundance"],
      proportion = column_labels["proportion"],
      cpue = column_labels["cpue"],
      ic95 = column_labels["ic95"],
      mf_ratio = column_labels["mf_ratio"]
    ) %>%
    cols_align(
      align = "center",
      columns = everything()
    ) %>%
    fmt_number(
      columns = c(proportion, cpue),
      decimals = 2
    ) %>%
    tab_options(
      table.width = "auto",  # Ajuster automatiquement la largeur du tableau
      table.font.size = px(12)
    )
}

gt_biomasse <- function(data) {
  # Extraire les labels des colonnes
  column_labels <- sapply(data, function(col) attr(col, "label"))
  
  data %>%
    gt() %>%
    tab_header(
      title = md("**Tableau de biomasse**")
    ) %>%
    # Utiliser les labels extraits dans cols_label
    cols_label(
      groupe = column_labels["groupe"],
      biomasse = column_labels["biomasse"],
      percent = column_labels["percent"],
      bpue = column_labels["bpue"],
      ic95 = column_labels["ic95"]
    ) %>%
    cols_align(
      align = "center",
      columns = everything()
    ) %>%
    tab_options(
      table.width = "auto",  # Ajuster automatiquement la largeur du tableau
      table.font.size = px(12)
    )
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

gt_mortalite2 <- function(data) {
  req(data)
  
  # Utilisation des labels comme noms de colonnes dans gt
  data %>%
    gt() %>%
    tab_header(
      title = md("**Estimations obtenues à partir du modèle de Robson-Chapman**"),
      subtitle = "À titre comparatif seulement"
    ) %>%
    cols_label(
      methode = var_label(data$methode),
      z = var_label(data$z),
      se = var_label(data$se),
      a = var_label(data$a),
      ic_95 = var_label(data$ic_95)
    ) %>%
    cols_align(
      align = "center",
      columns = everything()
    ) %>%
    tab_options(
      table.width = pct(100),
      table.font.size = px(12),
      table.font.names = "sans-serif",
      table.align = "center"
    )
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


verifier_dataframes <- function(dataframe, nom_dataframe) {
  if (nrow(dataframe) == 0) {
    return(paste(nom_dataframe, "est vide."))
  }
  return(NULL)
}


calculate_mf_ratio <- function(male_count, female_count) {
  if (male_count == 0 && female_count == 0) {
    return(NA)  # Si les deux comptages sont 0, retourner NA
  }
  # Simplifier le ratio
  ratio <- MASS::fractions(c(male_count, female_count))
  return(paste0(ratio[1], ":", ratio[2]))
}


get_binwidth <- function(espece) {
  if (espece == "SANA") {
    return(50)
  } else if (espece %in% c("SAFO", "SAVI")) {
    return(20)
  } else {
    return(NULL)
  }
}

get_nomsp <- function(espece) {
  if (espece == "SANA") {
    return("touladis")
  } else if (espece == "SAFO") {
    return("ombles de fontaine")
  } else if (espece == "SAVI") {
    return("dorés jaunes")
  } else {
    return(NULL)
  }
}


labelled_data <- function(data) {
  # Obtenir les labels des colonnes
  labels <- labelled::var_label(data)
  
  # Remplacer les noms des colonnes par leurs labels
  colnames(data) <- unlist(labels)
  
  return(data)
}

agemax <- function(data) {
  age_max <-
    max(na.omit(data$age)) #Trouver le plus vieil âge et ignorer les NA de votre jeu de données s’il en contient (sinon = erreur)
  age_max
}


death <- function(data, espece) {
  data %>%
    dplyr::filter(sp == espece) %>%
    droplevels() %>%
    dplyr::filter(!is.na(age))
}

create_sp_pen <- function(input_typ_pech) {
  if (input_typ_pech == "PENT") {
    return("SANA")
  } else if (input_typ_pech == "PENOF") {
    return("SAFO")
  } else if (input_typ_pech == "PENDJ") {
    return("SAVI")
  } else {
    return(NULL)
  }
}

get_zobs <- function(PP, death, agemax) {
  # Calcul de la mortalité selon plusieurs méthodes
  mortalite <- agesurv(
    type = 1,
    age = death$age,
    full = PP,
    last = agemax,
    estimate = "z",
    method = c("he", "lr", "wlr", "cr", "crcb", "pois")
  )
  
  # Extraction de la valeur de Z pour la méthode "cr"
  zobs <- mortalite$results[4, "Estimate"]
  
  return(zobs)
}
