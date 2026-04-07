#' mortalite UI Function
#'
#' @description Module Shiny pour l'analyse de mortalité.
#'
#' @param id Identifiant du module.
#'
#' @noRd
mod_mortalite_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Mortalité",
    
    uiOutput(ns("mortalite_message")),
    
    h4("Paramètre avancé : recalcul avec un autre âge de départ"),
    p("Vous pouvez forcer un recalcul avec une autre valeur."),
    uiOutput(ns("ui_custom_peak_plus")),
    actionButton(ns("recalculer_mortalite"), "Recalculer avec cet âge de départ"),
    em(textOutput(ns("texte_pp_utilise"))),
    br(), br(),
    
    h3("Test de sur-dispersion du modèle Poisson"),
    p("Ce test évalue si les données de mortalité par âge violent l'hypothèse d'équidispersion du modèle de Poisson."),
    strong("Interprétation :"),
    verbatimTextOutput(ns("dispersion_msg")),
    br(),
    div(
      style = "max-width: 900px; margin: auto;",
      withSpinner(plotOutput(ns("plot_dispersion_poisson"), height = "500px"), type = myspinner),
      br(),
      downloadButton(ns("download_plot_dispersion_poisson"), "Téléchargement du graphique")
    ),
    br(),
    
    h3("Table de sélection du modèle de mortalité"),
    p("Le tableau suivant présente les résultats pour l'ensemble des modèles testés."),
    withSpinner(reactableOutput(ns("comparaison_mortalite_table")), type = myspinner),
    download_button_ui(ns("download_comparaison_mortalite_table")),
    textOutput(ns("phrase_mortalite")),
    br(),
    
    h3("Distribution d'âge et modèle de mortalité retenu"),
    div(
      style = "max-width: 900px; margin: auto;",
      withSpinner(plotOutput(ns("plot_mortalite"), height = "500px"), type = myspinner),
      br(),
      downloadButton(ns("download_plot_mortalite"), "Téléchargement du graphique")
    ),
    br(),
    
    h3("Chapman-Robson"),
    p("La mortalité estimée selon la méthode de Chapman-Robson est présentée à titre comparatif seulement."),
    uiOutput(ns("table_chaprobson")),
    download_button_ui(ns("download_chaprob_df"))
  )
}

#' mortalite Server Function
#'
#' @param id Identifiant du module.
#' @param specimen Expression réactive contenant les spécimens valides.
#' @param filename_suffix Expression réactive pour suffixe des fichiers à exporter.
#'
#' @noRd
mod_mortalite_server <- function(id, specimen, filename_suffix) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Validation de base des données spécimens ----
    specimen_info <- reactive({
      data <- specimen()
      
      if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
        return(list(
          success = FALSE,
          message = "Aucun spécimen valide n'est disponible pour l'analyse de mortalité.",
          data = NULL
        ))
      }
      
      if (!"age" %in% names(data)) {
        return(list(
          success = FALSE,
          message = "Les données doivent contenir une colonne `age` pour l'analyse de mortalité.",
          data = NULL
        ))
      }
      
      list(
        success = TRUE,
        message = NULL,
        data = data
      )
    })
    
    # Résultat âge maximal ----
    age_max_res <- reactive({
      info <- specimen_info()
      
      if (isFALSE(info$success)) {
        return(list(
          success = FALSE,
          message = info$message,
          value = NULL
        ))
      }
      
      mortalite_get_age_max(data = info$data)
    })
    
    # Résultat peak plus automatique ----
    peak_plus_auto_res <- reactive({
      info <- specimen_info()
      
      if (isFALSE(info$success)) {
        return(list(
          success = FALSE,
          message = info$message,
          value = NULL
        ))
      }
      
      mortalite_get_peak_plus(data = info$data)
    })
    
    # Message principal du module ----
    output$mortalite_message <- renderUI({
      info_data <- specimen_info()
      info_age_max <- age_max_res()
      info_peak_plus <- peak_plus_auto_res()
      
      if (isFALSE(info_data$success)) {
        return(div(class = "alert alert-warning", info_data$message))
      }
      
      if (isFALSE(info_age_max$success)) {
        return(div(class = "alert alert-warning", info_age_max$message))
      }
      
      if (isFALSE(info_peak_plus$success)) {
        return(
          div(
            class = "alert alert-warning",
            p(info_peak_plus$message),
            p("Vous pouvez toutefois sélectionner manuellement un âge de départ ci-dessous.")
          )
        )
      }
      
      if (!is.null(info_peak_plus$value) &&
          !is.null(info_age_max$value) &&
          info_peak_plus$value >= info_age_max$value) {
        return(
          div(
            class = "alert alert-warning",
            p("L'âge de départ automatique estimé n'est pas compatible avec l'âge maximal observé."),
            p("Veuillez sélectionner manuellement un âge de départ inférieur à l'âge maximal.")
          )
        )
      }
      
      NULL
    })
    
    # UI du choix manuel de peak plus ----
    output$ui_custom_peak_plus <- renderUI({
      info_age_max <- age_max_res()
      info_peak_plus <- peak_plus_auto_res()
      
      req(isTRUE(info_age_max$success))
      req(!is.null(info_age_max$value))
      
      age_max <- as.integer(info_age_max$value)
      
      if (age_max <= 0) {
        return(NULL)
      }
      
      peak_plus_auto <- info_peak_plus$value
      
      selected_value <- if (!is.null(peak_plus_auto) && peak_plus_auto < age_max) {
        peak_plus_auto
      } else {
        max(0, age_max - 1L)
      }
      
      selectInput(
        inputId = ns("custom_peak_plus"),
        label = "Recalculer avec un autre âge de départ (facultatif)",
        choices = seq(0, age_max - 1L),
        selected = selected_value
      )
    })
    
    # Peak plus final utilisé ----
    peak_plus_final <- reactive({
      info_age_max <- age_max_res()
      info_peak_plus <- peak_plus_auto_res()
      
      req(isTRUE(info_age_max$success))
      req(!is.null(info_age_max$value))
      
      age_max <- as.numeric(info_age_max$value)
      peak_plus_auto <- info_peak_plus$value
      
      if (input$recalculer_mortalite == 0 &&
          !is.null(peak_plus_auto) &&
          peak_plus_auto < age_max) {
        return(peak_plus_auto)
      }
      
      custom_pp <- suppressWarnings(as.numeric(input$custom_peak_plus))
      
      if (is.null(custom_pp) || is.na(custom_pp)) {
        return(NULL)
      }
      
      if (custom_pp >= age_max) {
        return(NULL)
      }
      
      custom_pp
    })
    
    output$texte_pp_utilise <- renderText({
      info_peak_plus <- peak_plus_auto_res()
      pp_final <- peak_plus_final()
      
      if (is.null(pp_final)) {
        return("Aucun âge de départ valide n'est actuellement disponible pour l'analyse.")
      }
      
      if (input$recalculer_mortalite == 0 &&
          isTRUE(info_peak_plus$success) &&
          !is.null(info_peak_plus$value) &&
          identical(pp_final, info_peak_plus$value)) {
        return(
          glue(
            "Analyse effectuée avec la valeur automatique d'âge de départ : {pp_final}"
          ) |>
            as.character()
        )
      }
      
      glue(
        "Analyse effectuée avec la valeur personnalisée d'âge de départ : {pp_final}"
      ) |>
        as.character()
    })
    
    # Préparation des données corrigées ----
    df_age_corrigee_res <- reactive({
      info_data <- specimen_info()
      info_age_max <- age_max_res()
      pp_final <- peak_plus_final()
      
      if (isFALSE(info_data$success)) {
        return(list(
          success = FALSE,
          message = info_data$message,
          data = NULL
        ))
      }
      
      if (isFALSE(info_age_max$success)) {
        return(list(
          success = FALSE,
          message = info_age_max$message,
          data = NULL
        ))
      }
      
      if (is.null(pp_final)) {
        return(list(
          success = FALSE,
          message = "Veuillez sélectionner un âge de départ valide inférieur à l'âge maximal.",
          data = NULL
        ))
      }
      
      mortalite_prepare_corr(
        data = info_data$data,
        age_peak_plus = pp_final,
        age_max = info_age_max$value
      )
    })
    
    # Préparation des données étendues ----
    df_age_etendue_res <- reactive({
      res_corrigee <- df_age_corrigee_res()
      info_age_max <- age_max_res()
      
      if (isFALSE(res_corrigee$success)) {
        return(list(
          success = FALSE,
          message = res_corrigee$message,
          data = NULL
        ))
      }
      
      if (isFALSE(info_age_max$success)) {
        return(list(
          success = FALSE,
          message = info_age_max$message,
          data = NULL
        ))
      }
      
      mortalite_prepare_extended(
        df_corrigee = res_corrigee$data,
        age_max = info_age_max$value
      )
    })
    
    # Test de surdispersion ----
    res_test_surdisp <- reactive({
      res_corrigee <- df_age_corrigee_res()
      
      if (isFALSE(res_corrigee$success)) {
        return(list(
          success = FALSE,
          message = res_corrigee$message,
          dispersion = NULL,
          plot = NULL
        ))
      }
      
      mortalite_test_surdispersion_poisson(res_corrigee$data)
    })
    
    output$dispersion_msg <- renderText({
      res <- res_test_surdisp()
      res$message
    })
    
    render_plot_ggplot(
      "plot_dispersion_poisson",
      reactive({
        res <- res_test_surdisp()
        res$plot
      })
    )
    
    render_download_plot(
      "download_plot_dispersion_poisson",
      reactive({
        res <- res_test_surdisp()
        res$plot
      }),
      filename = "dispersion_poisson"
    )
    
    # Comparaison des modèles ----
    mortalite_compare_modele_res <- reactive({
      res_etendue <- df_age_etendue_res()
      
      if (isFALSE(res_etendue$success)) {
        return(list(
          success = FALSE,
          message = res_etendue$message,
          data = NULL,
          flextable = NULL
        ))
      }
      
      mortalite_compare_modele(data = res_etendue$data)
    })
    
    table_modeles_mortalite <- reactive({
      res <- mortalite_compare_modele_res()
      res$data
    })
    
    best_model_mortalite <- reactive({
      table <- table_modeles_mortalite()
      mortalite_select_best_modele(table)
    })
    
    default_model_index_mortalite <- reactive({
      table <- table_modeles_mortalite()
      
      if (is.null(table) || nrow(table) == 0) {
        return(NULL)
      }
      
      best_model <- best_model_mortalite()
      
      if (is.null(best_model)) {
        return(1)
      }
      
      idx <- match(best_model, table$methode)
      
      if (is.na(idx)) {
        return(1)
      }
      
      idx
    })
    
    output$comparaison_mortalite_table <- renderReactable({
      table <- table_modeles_mortalite()
      idx <- default_model_index_mortalite()
      
      req(!is.null(table))
      req(nrow(table) > 0)
      
      reactable(
        table,
        selection = "single",
        onClick = "select",
        defaultSelected = idx,
        defaultColDef = colDef(
          align = "center",
          headerStyle = list(textAlign = "center")
        )
      )
    })
    
    selected_model_mortalite <- reactive({
      table <- table_modeles_mortalite()
      
      if (is.null(table) || nrow(table) == 0) {
        return(NULL)
      }
      
      selected <- getReactableState("comparaison_mortalite_table", "selected")
      
      if (is.null(selected) || length(selected) == 0) {
        idx <- default_model_index_mortalite()
        
        if (is.null(idx)) {
          return(NULL)
        }
        
        return(table[idx, "methode", drop = TRUE])
      }
      
      table[selected, "methode", drop = TRUE]
    })
    
    output$phrase_mortalite <- renderText({
      mortalite_phrase_resume(
        data_comparaison = table_modeles_mortalite(),
        modele_nom = best_model_mortalite()
      )
    })
    
    # Ajustement du modèle sélectionné ----
    modele_fit_mortalite <- reactive({
      res_etendue <- df_age_etendue_res()
      methode <- selected_model_mortalite()
      
      if (isFALSE(res_etendue$success)) {
        return(NULL)
      }
      
      if (is.null(methode)) {
        return(NULL)
      }
      
      mortalite_fit_best_modele(
        data = res_etendue$data,
        methode = methode
      )
    })
    
    plot_selectedmodel_mortalite <- reactive({
      info_data <- specimen_info()
      modele <- modele_fit_mortalite()
      info_modele <- table_modeles_mortalite()
      
      if (isFALSE(info_data$success)) {
        return(NULL)
      }
      
      if (is.null(modele)) {
        return(NULL)
      }
      
      mortalite_plot_modele(
        specimen = info_data$data,
        modele = modele,
        info_modele = info_modele
      )
    })
    
    render_plot_ggplot(
      "plot_mortalite",
      reactive(plot_selectedmodel_mortalite())
    )
    
    render_download_plot(
      "download_plot_mortalite",
      plot_selectedmodel_mortalite,
      filename = "courbe_mortalite"
    )
    
    # Chapman-Robson ----
    res_chaprob <- reactive({
      info_data <- specimen_info()
      info_age_max <- age_max_res()
      pp_final <- peak_plus_final()
      
      if (isFALSE(info_data$success)) {
        return(list(
          success = FALSE,
          message = info_data$message,
          data = NULL,
          flextable = NULL
        ))
      }
      
      if (isFALSE(info_age_max$success)) {
        return(list(
          success = FALSE,
          message = info_age_max$message,
          data = NULL,
          flextable = NULL
        ))
      }
      
      if (is.null(pp_final)) {
        return(list(
          success = FALSE,
          message = "Veuillez sélectionner un âge de départ valide pour Chapman-Robson.",
          data = NULL,
          flextable = NULL
        ))
      }
      
      mortalite_chaprob(
        specimen = info_data$data,
        pp = pp_final,
        age_max = info_age_max$value
      )
    })
    
    render_table_flextable(
      "table_chaprobson",
      reactive({
        res <- res_chaprob()
        res$flextable
      })
    )
    
    render_download_table(
      "download_chaprob_df",
      data = reactive({
        res <- res_chaprob()
        res$data
      }),
      filename = reactive(build_export_filename("chapman_robson", filename_suffix()))
    )
    
    render_download_table(
      "download_comparaison_mortalite_table",
      data = reactive(table_modeles_mortalite()),
      filename = reactive(build_export_filename("mortalite_comparaison", filename_suffix()))
    )
  })
}