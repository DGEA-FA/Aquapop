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
    uiOutput(ns("mortalite_param_section")),
    withSpinner(
      uiOutput(ns("mortalite_results_section")),
      type = myspinner
    )
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
    
    # Validation de base des données spécimens ====
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
    
    # Résultat âge maximal ====
    age_max_res <- reactive({
      info <- specimen_info()
      
      if (isFALSE(info$success)) {
        return(list(
          success = FALSE,
          message = info$message,
          value = NULL
        ))
      }
      
      res <- mortalite_get_age_max(data = info$data)
      
      if (isFALSE(res$success)) {
        return(list(
          success = FALSE,
          message = "Il n'y a pas d'âge disponible dans ce jeu de données, ce qui empêche la modélisation de la mortalité.",
          value = NULL
        ))
      }
      
      res
    })
    
    # Peak Plus proposé automatiquement ====
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
    
    # Message principal du module ====
    output$mortalite_message <- renderUI({
      info_data <- specimen_info()
      info_age_max <- age_max_res()
      
      if (isFALSE(info_data$success)) {
        return(div(class = "alert alert-danger", info_data$message))
      }
      
      if (isFALSE(info_age_max$success)) {
        return(div(class = "alert alert-danger", info_age_max$message))
      }
      
      NULL
    })
    
    # Section paramètre : affichée seulement si les données minimales sont admissibles ====
    output$mortalite_param_section <- renderUI({
      info_data <- specimen_info()
      info_age_max <- age_max_res()
      info_peak_plus <- peak_plus_auto_res()
      
      if (isFALSE(info_data$success) || isFALSE(info_age_max$success)) {
        return(NULL)
      }
      
      age_max <- as.integer(info_age_max$value)
      
      if (is.null(age_max) || is.na(age_max) || age_max <= 0) {
        return(NULL)
      }
      
      peak_plus_auto <- info_peak_plus$value
      
      selected_value <- if (!is.null(peak_plus_auto) &&
                            !is.na(peak_plus_auto) &&
                            peak_plus_auto < age_max) {
        as.integer(peak_plus_auto)
      } else {
        max(0L, age_max - 1L)
      }
      
      help_text <- if (isTRUE(info_peak_plus$success)) {
        "Une valeur automatique de Peak Plus a été proposée à partir des données. Vous pouvez la conserver ou la modifier avant de lancer l'analyse."
      } else {
        "La valeur automatique de Peak Plus n'a pas pu être déterminée. Veuillez en sélectionner une manuellement avant de lancer l'analyse."
      }
      
      tagList(
        h4("Âge de départ (Peak Plus)"),
        p(help_text),
        numericInput(
          inputId = ns("peak_plus"),
          label = "Valeur de Peak Plus à utiliser",
          value = selected_value,
          min = 0,
          max = age_max - 1L,
          step = 1
        ),
        actionButton(
          inputId = ns("lancer_mortalite"),
          label = "Lancer l'analyse de mortalité"
        ),
        br(),
        br()
      )
    })
    
    # Peak Plus choisi par l'utilisatrice ====
    peak_plus_selected <- reactive({
      info_age_max <- age_max_res()
      
      req(isTRUE(info_age_max$success))
      req(!is.null(info_age_max$value))
      
      age_max <- as.numeric(info_age_max$value)
      peak_plus <- suppressWarnings(as.numeric(input$peak_plus))
      
      if (is.null(peak_plus) || is.na(peak_plus)) {
        return(NULL)
      }
      
      if (peak_plus < 0 || peak_plus >= age_max) {
        return(NULL)
      }
      
      if (!identical(peak_plus, floor(peak_plus))) {
        return(NULL)
      }
      
      as.integer(peak_plus)
    })
    
    # Analyse déclenchée uniquement au clic ====
    analyse_mortalite_res <- eventReactive(input$lancer_mortalite, {
      info_data <- specimen_info()
      info_age_max <- age_max_res()
      pp_selected <- peak_plus_selected()
      
      if (isFALSE(info_data$success)) {
        return(list(
          success = FALSE,
          message = info_data$message,
          peak_plus = NULL,
          df_corrigee = NULL,
          df_etendue = NULL,
          surdisp = NULL,
          comparaison = NULL,
          best_model = NULL,
          has_converged_model = FALSE,
          chaprob = NULL
        ))
      }
      
      if (isFALSE(info_age_max$success)) {
        return(list(
          success = FALSE,
          message = info_age_max$message,
          peak_plus = NULL,
          df_corrigee = NULL,
          df_etendue = NULL,
          surdisp = NULL,
          comparaison = NULL,
          best_model = NULL,
          has_converged_model = FALSE,
          chaprob = NULL
        ))
      }
      
      if (is.null(pp_selected)) {
        return(list(
          success = FALSE,
          message = "Veuillez sélectionner un âge de départ valide inférieur à l'âge maximal.",
          peak_plus = NULL,
          df_corrigee = NULL,
          df_etendue = NULL,
          surdisp = NULL,
          comparaison = NULL,
          best_model = NULL,
          has_converged_model = FALSE,
          chaprob = NULL
        ))
      }
      
      df_corrigee_res <- mortalite_prepare_corr(
        data = info_data$data,
        age_peak_plus = pp_selected,
        age_max = info_age_max$value
      )
      
      if (isFALSE(df_corrigee_res$success)) {
        return(list(
          success = FALSE,
          message = df_corrigee_res$message,
          peak_plus = pp_selected,
          df_corrigee = NULL,
          df_etendue = NULL,
          surdisp = NULL,
          comparaison = NULL,
          best_model = NULL,
          has_converged_model = FALSE,
          chaprob = NULL
        ))
      }
      
      df_etendue_res <- mortalite_prepare_extended(
        df_corrigee = df_corrigee_res$data,
        age_max = info_age_max$value
      )
      
      if (isFALSE(df_etendue_res$success)) {
        return(list(
          success = FALSE,
          message = df_etendue_res$message,
          peak_plus = pp_selected,
          df_corrigee = df_corrigee_res$data,
          df_etendue = NULL,
          surdisp = NULL,
          comparaison = NULL,
          best_model = NULL,
          has_converged_model = FALSE,
          chaprob = NULL
        ))
      }
      
      comparaison_res <- mortalite_compare_modele(data = df_etendue_res$data)
      
      if (isFALSE(comparaison_res$success) || is.null(comparaison_res$data)) {
        return(list(
          success = FALSE,
          message = comparaison_res$message,
          peak_plus = pp_selected,
          df_corrigee = df_corrigee_res$data,
          df_etendue = df_etendue_res$data,
          surdisp = NULL,
          comparaison = comparaison_res,
          best_model = NULL,
          has_converged_model = FALSE,
          chaprob = NULL
        ))
      }
      
      table_modeles <- comparaison_res$data
      
      modeles_convergents <- table_modeles |>
        dplyr::filter(.data$convergence %in% TRUE)
      
      has_converged_model <- nrow(modeles_convergents) > 0
      
      best_model <- if (has_converged_model) {
        mortalite_select_best_modele(table_modeles)
      } else {
        NULL
      }
      
      surdisp_res <- if (has_converged_model) {
        mortalite_test_surdispersion_poisson(df_corrigee_res$data)
      } else {
        NULL
      }
      
      chaprob_res <- if (has_converged_model) {
        mortalite_chaprob(
          specimen = info_data$data,
          pp = pp_selected,
          age_max = info_age_max$value
        )
      } else {
        NULL
      }
      
      list(
        success = TRUE,
        message = NULL,
        peak_plus = pp_selected,
        df_corrigee = df_corrigee_res$data,
        df_etendue = df_etendue_res$data,
        surdisp = surdisp_res,
        comparaison = comparaison_res,
        best_model = best_model,
        has_converged_model = has_converged_model,
        chaprob = chaprob_res
      )
    })
    
    # Section résultats ====
    output$mortalite_results_section <- renderUI({
      info_data <- specimen_info()
      info_age_max <- age_max_res()
      
      if (isFALSE(info_data$success) || isFALSE(info_age_max$success)) {
        return(NULL)
      }
      
      if (is.null(input$lancer_mortalite) || input$lancer_mortalite == 0) {
        return(NULL)
      }
      
      analyse <- analyse_mortalite_res()
      
      if (isFALSE(analyse$success)) {
        return(
          div(
            class = "alert alert-danger",
            analyse$message
          )
        )
      }
      
      if (isFALSE(analyse$has_converged_model)) {
        return(
          tagList(
            div(
              class = "alert alert-warning",
              p("Aucun des modèles de mortalité n'a convergé avec la valeur de Peak Plus sélectionnée."),
              p("Le tableau comparatif est affiché à titre informatif. Les autres sections ne sont pas présentées car elles seraient vides ou non interprétables.")
            ),
            h3("Table de sélection du modèle de mortalité"),
            p("Le tableau suivant présente les résultats pour l'ensemble des modèles testés."),
            withSpinner(reactableOutput(ns("comparaison_mortalite_table")), type = myspinner),
            download_button_ui(ns("download_comparaison_mortalite_table"))
          )
        )
      }
      
      tagList(
        div(
          class = "alert alert-info",
          glue("Analyse effectuée avec la valeur de Peak Plus : {analyse$peak_plus}") |>
            as.character()
        ),
        
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
    })
    
    # Tableau de comparaison ====
    table_modeles_mortalite <- reactive({
      analyse <- analyse_mortalite_res()
      
      if (isFALSE(analyse$success) ||
          is.null(analyse$comparaison) ||
          is.null(analyse$comparaison$data)) {
        return(NULL)
      }
      
      analyse$comparaison$data
    })
    
    default_model_index_mortalite <- reactive({
      analyse <- analyse_mortalite_res()
      table <- table_modeles_mortalite()
      
      if (is.null(table) || nrow(table) == 0) {
        return(NULL)
      }
      
      if (isFALSE(analyse$has_converged_model) || is.null(analyse$best_model)) {
        return(1)
      }
      
      idx <- match(analyse$best_model, table$methode)
      
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
      analyse <- analyse_mortalite_res()
      table <- table_modeles_mortalite()
      
      if (isFALSE(analyse$has_converged_model)) {
        return(NULL)
      }
      
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
      analyse <- analyse_mortalite_res()
      
      if (isFALSE(analyse$has_converged_model)) {
        return(NULL)
      }
      
      mortalite_phrase_resume(
        data_comparaison = table_modeles_mortalite(),
        modele_nom = analyse$best_model
      )
    })
    
    # Test de surdispersion ====
    output$dispersion_msg <- renderText({
      analyse <- analyse_mortalite_res()
      
      req(isTRUE(analyse$has_converged_model))
      req(!is.null(analyse$surdisp))
      
      analyse$surdisp$message
    })
    
    render_plot_ggplot(
      "plot_dispersion_poisson",
      reactive({
        analyse <- analyse_mortalite_res()
        
        if (isFALSE(analyse$has_converged_model) || is.null(analyse$surdisp)) {
          return(NULL)
        }
        
        analyse$surdisp$plot
      })
    )
    
    render_download_plot(
      "download_plot_dispersion_poisson",
      reactive({
        analyse <- analyse_mortalite_res()
        
        if (isFALSE(analyse$has_converged_model) || is.null(analyse$surdisp)) {
          return(NULL)
        }
        
        analyse$surdisp$plot
      }),
      filename = "dispersion_poisson"
    )
    
    # Ajustement du modèle sélectionné ====
    modele_fit_mortalite <- reactive({
      analyse <- analyse_mortalite_res()
      methode <- selected_model_mortalite()
      
      if (isFALSE(analyse$has_converged_model)) {
        return(NULL)
      }
      
      if (is.null(analyse$df_etendue) || is.null(methode)) {
        return(NULL)
      }
      
      mortalite_fit_best_modele(
        data = analyse$df_etendue,
        methode = methode
      )
    })
    
    plot_selectedmodel_mortalite <- reactive({
      analyse <- analyse_mortalite_res()
      info_data <- specimen_info()
      modele <- modele_fit_mortalite()
      info_modele <- table_modeles_mortalite()
      
      if (isFALSE(analyse$has_converged_model)) {
        return(NULL)
      }
      
      if (isFALSE(info_data$success) || is.null(modele)) {
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
    
    # Chapman-Robson ====
    render_table_flextable(
      "table_chaprobson",
      reactive({
        analyse <- analyse_mortalite_res()
        
        if (isFALSE(analyse$has_converged_model) || is.null(analyse$chaprob)) {
          return(NULL)
        }
        
        analyse$chaprob$flextable
      })
    )
    
    render_download_table(
      "download_chaprob_df",
      data = reactive({
        analyse <- analyse_mortalite_res()
        
        if (isFALSE(analyse$has_converged_model) || is.null(analyse$chaprob)) {
          return(NULL)
        }
        
        analyse$chaprob$data
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