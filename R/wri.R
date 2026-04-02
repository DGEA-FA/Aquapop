#' Calculer l'indice de condition relatif (Wr) pour une espèce
#'
#' Cette fonction calcule l'indice de condition relatif (Wr) à partir des
#' longueurs et masses des spécimens d'une espèce donnée. Elle retourne un
#' tableau de synthèse, une version formatée avec `flextable`, ainsi que deux
#' graphiques illustrant les résultats : l'un selon la longueur individuelle,
#' l'autre par classe de taille.
#'
#' Si aucune donnée exploitable n'est disponible, la fonction retourne un objet
#' structuré avec `success = FALSE`, sans générer d'erreur.
#'
#' @param data Un `data.frame` contenant au minimum les colonnes suivantes :
#' `sp`, `ltm`, `masse` et `sexe`.
#'
#' @return Une liste contenant :
#' \describe{
#'   \item{success}{Indique si l'analyse a pu être produite}
#'   \item{data}{Un `data.frame` avec les Wr moyens et effectifs par groupe}
#'   \item{flextable}{Une version formatée (`flextable`) prête pour exportation}
#'   \item{plot_tous}{Un graphique `ggplot2` du Wr en fonction de la longueur}
#'   \item{plot_byclass}{Un graphique `ggplot2` du Wr moyen par classe de taille}
#'   \item{message}{Message explicatif si l'analyse n'est pas disponible}
#' }
#'
#' @examples
#' df <- data.frame(
#'   sp = rep("SANA", 10),
#'   ltm = seq(300, 480, by = 20),
#'   masse = seq(200, 380, by = 20),
#'   sexe = rep(c("F", "M"), 5)
#' )
#'
#' res <- wri(df)
#' res$data
#' if (requireNamespace("flextable", quietly = TRUE)) res$flextable
#'
#' @importFrom checkmate assert_data_frame assert_subset
#' @importFrom dplyr filter mutate select group_by summarise left_join count n_distinct bind_cols bind_rows arrange recode rename slice
#' @importFrom tidyr complete
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbar geom_hline annotate labs scale_color_manual scale_x_discrete xlab ylab scale_linetype_manual guide_legend guides
#' @importFrom flextable flextable set_caption set_header_labels
#' @importFrom labelled set_variable_labels
#' @importFrom tibble tibble
#' @importFrom FSA lencat
#' @importFrom rlang sym
#' @importFrom plyr mapvalues
#' @importFrom glue glue
#' @importFrom stats lm predict setNames
#'
#' @export
wri <- function(data) {
  
  # Validation des données ----
  assert_data_frame(data)
  
  colonnes_requises <- c("sp", "ltm", "masse", "sexe")
  assert_subset(colonnes_requises, colnames(data))
  
  # Cas sans ligne ----
  if (nrow(data) == 0) {
    return(list(
      success = FALSE,
      data = NULL,
      flextable = NULL,
      plot_tous = NULL,
      plot_byclass = NULL,
      message = "Aucun spécimen valide disponible pour produire l'indice de condition."
    ))
  }
  
  # Validation de l'espèce ----
  espece <- unique(as.character(data$sp))
  
  if (length(espece) != 1) {
    stop("Les données doivent contenir une seule espèce.")
  }
  
  info_pen <- get_info_pen(espece)
  constantes_wr <- get_wr_constants(espece)
  
  if (is.null(info_pen) || is.null(constantes_wr)) {
    stop("Espèce non supportée.")
  }
  
  # Préparation des données ----
  data_wr <- data |>
    filter(
      !is.na(ltm),
      !is.na(masse),
      ltm >= constantes_wr$min_TL
    ) |>
    mutate(
      prediction = 10 ^ (constantes_wr$int + constantes_wr$slope * log10(ltm)),
      wr = masse * 100 / prediction
    )
  
  # Cas sans donnée exploitable pour Wr ----
  if (nrow(data_wr) == 0) {
    return(list(
      success = FALSE,
      data = NULL,
      flextable = NULL,
      plot_tous = NULL,
      plot_byclass = NULL,
      message = paste(
        "Aucun spécimen ne possède les mesures nécessaires de longueur et de masse",
        "pour calculer l'indice de condition, ou n'atteint la taille minimale requise",
        "pour cette espèce."
      )
    ))
  }
  
  # Harmonisation du sexe ----
  niveaux_sexe <- c("F", "M", "IND")
  
  data_wr <- data_wr |>
    mutate(sexe = factor(sexe, levels = niveaux_sexe))
  
  # Graphique Wr selon la longueur et le sexe ----
  moyenne_par_sexe <- tibble(sexe = factor(niveaux_sexe, levels = niveaux_sexe)) |>
    left_join(
      data_wr |>
        group_by(sexe) |>
        summarise(moyenne = mean(wr, na.rm = TRUE), .groups = "drop"),
      by = "sexe"
    )
  
  moyenne_totale <- mean(data_wr$wr, na.rm = TRUE)
  
  plot_tous <- ggplot(data_wr, aes(x = ltm, y = wr, color = sexe)) +
    geom_point(alpha = 0.8) +
    scale_color_manual(
      values = group_colors$sexe,
      labels = group_labels$sexe,
      name = "",
      drop = FALSE
    ) +
    labs(
      x = "Longueur totale maximale (mm)",
      y = "Indice de condition (%)"
    ) +
    annotate(
      "segment",
      x = -Inf,
      xend = Inf,
      y = 100,
      yend = 100,
      color = "lightgrey",
      linewidth = 0.5,
      linetype = 2
    ) +
    geom_hline(
      data = moyenne_par_sexe,
      aes(yintercept = moyenne, color = sexe),
      linetype = 2,
      linewidth = 0.5
    ) +
    geom_hline(
      aes(yintercept = moyenne_totale, linetype = "Tous"),
      color = "red",
      linewidth = 0.5
    ) +
    scale_linetype_manual(
      name = "",
      values = c("Tous" = 2)
    ) +
    guides(
      color = guide_legend(order = 1),
      linetype = guide_legend(
        order = 2,
        override.aes = list(color = "red")
      )
    ) +
    theme_aquapop()
  
  # Préparation des classes PSD ----
  data_wr <- data_wr |>
    mutate(
      classe_brute = lencat(ltm, breaks = info_pen$breaks, as.fact = TRUE),
      classe = recode(
        as.character(classe_brute),
        !!!setNames(psd_classnames, as.character(info_pen$breaks))
      ),
      intervalle = recode(
        as.character(classe_brute),
        !!!setNames(info_pen$break_labels, as.character(info_pen$breaks))
      )
    )
  
  sommaire_classe <- data_wr |>
    count(classe_brute, classe, intervalle, name = "n")
  
  data_wr <- data_wr |>
    left_join(
      sommaire_classe,
      by = c("classe_brute", "classe", "intervalle")
    )
  
  # Graphique Wr moyen par classe ----
  if (n_distinct(data_wr$classe_brute) >= 2) {
    modele_classe <- lm(wr ~ classe_brute, data = data_wr)
    
    niveaux_classe <- levels(droplevels(data_wr$classe_brute))
    
    grille_classe <- tibble(
      classe_brute = factor(
        niveaux_classe,
        levels = levels(data_wr$classe_brute)
      )
    )
    
    prediction_classe <- predict(
      modele_classe,
      newdata = grille_classe,
      interval = "confidence"
    ) |>
      as.data.frame() |>
      bind_cols(grille_classe) |>
      left_join(sommaire_classe, by = "classe_brute")
    
    plot_byclass <- ggplot(prediction_classe, aes(x = classe, y = fit)) +
      geom_point() +
      geom_point(
        data = data_wr,
        aes(x = classe, y = wr),
        shape = 21,
        colour = "black",
        fill = "white",
        size = 1,
        alpha = 0.5
      ) +
      geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.1) +
      xlab("Classe de taille") +
      ylab("Indice de condition (%)") +
      scale_x_discrete(limits = psd_classnames, drop = FALSE) +
      annotate(
        "segment",
        x = -Inf,
        xend = Inf,
        y = 100,
        yend = 100,
        linewidth = 0.5,
        color = "black",
        linetype = 2
      ) +
      theme_aquapop()
    
  } else {
    plot_byclass <- NULL
  }
  
  # Tableau de synthèse : Tous ----
  table_tous <- predict(
    lm(wr ~ 1, data = data_wr),
    newdata = data.frame(groupe = "Tous"),
    interval = "confidence"
  ) |>
    as.data.frame() |>
    mutate(
      groupe = "Tous",
      ic95 = paste0("[", round(lwr), "-", round(upr), "]"),
      wr = round(fit),
      n = nrow(data_wr)
    ) |>
    select(groupe, wr, ic95, n)
  
  # Tableau de synthèse : sexe ----
  table_par_sexe <- if (n_distinct(data_wr$sexe) >= 2) {
    resumer_wr_par_groupe(
      mod = lm(wr ~ sexe, data = data_wr),
      var = "sexe"
    ) |>
      filter(groupe %in% c("F", "M")) |>
      mutate(
        groupe = mapvalues(
          groupe,
          from = c("F", "M"),
          to = c("Femelle", "Mâle")
        )
      )
  } else {
    tibble(
      groupe = character(),
      wr = numeric(),
      ic95 = character(),
      n = integer()
    )
  }
  
  # Tableau de synthèse : classes PSD ----
  table_par_classe <- if (n_distinct(data_wr$classe) >= 2) {
    resumer_wr_par_groupe(
      mod = lm(wr ~ classe, data = data_wr),
      var = "classe"
    ) |>
      complete(
        groupe = psd_classnames,
        fill = list(
          wr = 0,
          ic95 = "0",
          n = 0
        )
      ) |>
      mutate(groupe = factor(groupe, levels = psd_classnames)) |>
      arrange(groupe)
  } else {
    tibble(
      groupe = factor(psd_classnames, levels = psd_classnames),
      wr = 0,
      ic95 = "0",
      n = 0
    )
  }
  
  # Tableau final ----
  table_sommaire <- bind_rows(
    table_tous,
    table_par_sexe,
    table_par_classe
  ) |>
    labelled::set_variable_labels(
      groupe = "Groupe",
      wr = "Wr (%)",
      ic95 = "IC 95%",
      n = "N"
    )
  
  table_flextable <- table_sommaire |>
    flextable() |>
    set_caption("Indice de condition (Wr)") |>
    set_header_labels(
      groupe = "Groupe",
      wr = "Wr (%)",
      ic95 = "IC 95%",
      n = "N"
    ) |>
    style_flextable_aquapop()
  
  # Retour ----
  return(list(
    success = TRUE,
    data = table_sommaire,
    flextable = table_flextable,
    plot_tous = plot_tous,
    plot_byclass = plot_byclass,
    message = NULL
  ))
}

#' Résumer les résultats d'un modèle Wr par groupe
#'
#' Fonction interne. Génère un tableau de prédictions avec intervalles de
#' confiance et effectifs pour chaque modalité du groupe spécifié.
#'
#' @param mod Un objet `lm` ajusté sur `wr` en fonction du groupe.
#' @param var Nom de la variable de groupe à utiliser (`"sexe"` ou `"classe"`).
#'
#' @return Un `data.frame` contenant les colonnes `groupe`, `wr`, `ic95` et `n`.
#'
#' @keywords internal
resumer_wr_par_groupe <- function(mod, var) {
  
  valeurs <- unique(as.character(mod$model[[var]]))
  
  nouvelles_donnees <- tibble(!!sym(var) := valeurs)
  
  predictions <- predict(
    mod,
    newdata = nouvelles_donnees,
    interval = "confidence"
  ) |>
    as.data.frame() |>
    mutate(
      groupe = valeurs,
      ic95 = paste0("[", round(lwr), "-", round(upr), "]"),
      wr = round(fit)
    ) |>
    select(groupe, wr, ic95)
  
  effectifs <- mod$model |>
    count(!!sym(var)) |>
    rename(groupe = !!sym(var))
  
  predictions |>
    left_join(effectifs, by = "groupe")
}

#' Récupérer les constantes Wr pour une espèce donnée
#'
#' Cette fonction retourne les coefficients de référence pour le calcul de
#' l'indice de condition relatif (Wr) pour une espèce supportée.
#'
#' @param espece Code d'espèce, par exemple `"SANA"`, `"SAFO"` ou `"SAVI"`.
#'
#' @return Un `data.frame` d'une ligne contenant les constantes de calcul du Wr.
#'
#' @keywords internal
get_wr_constants <- function(espece) {
  wr_constants |>
    filter(sp == espece) |>
    slice(1)
}