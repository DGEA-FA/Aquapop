#' Calculer l’indice de condition relatif (Wr) pour une espèce
#'
#' Cette fonction calcule l’indice de condition relatif (Wr) à partir des longueurs et masses des spécimens d’une espèce donnée.
#' Elle retourne un tableau de synthèse, une version formatée (`flextable`), ainsi que deux graphiques illustrant les résultats
#' : l’un selon la longueur individuelle, l’autre par classe de taille.
#'
#' @param data Un `data.frame` contenant au minimum les colonnes suivantes :
#' `sp`, `ltm` (longueur totale en mm), `masse` (masse en g) et `sexe`.
#'
#' @return Une liste contenant :
#' \describe{
#'   \item{data}{Un `data.frame` avec les Wr moyens et effectifs par groupe}
#'   \item{flextable}{Une version formatée (`flextable`) prête pour exportation}
#'   \item{plot_tous}{Un graphique `ggplot2` du Wr en fonction de la longueur, coloré par sexe}
#'   \item{plot_byclass}{Un graphique `ggplot2` du Wr moyen par classe de taille}
#' }
#'
#' @examples
#' df <- data.frame(
#'   sp    = rep("SANA", 10),
#'   ltm   = seq(300, 480, by = 20),
#'   masse = seq(200, 380, by = 20),
#'   sexe  = rep(c("F", "M"), 5)
#' )
#' wri(df)
#'
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
#' @importFrom stats lm setNames
#'
#' @export
wri <- function(data) {
  
  # ---- Chargement et validation des données ----
  
  colonnes_essentielles <- c("sp", "ltm", "masse")
  colonnes_absentes <- setdiff(colonnes_essentielles, names(data))
  if (length(colonnes_absentes) > 0) {
    stop(glue("Colonnes manquantes : {paste(colonnes_absentes, collapse = ', ')}"))
  }
  
  espece <- as.character(unique(data$sp))
  if (length(espece) != 1) stop("Les données doivent contenir une seule espèce.")
  
  info_pen <- get_info_pen(espece)
  constantes_wr <- get_wr_constants(espece)
  if (is.null(info_pen) || is.null(constantes_wr)) stop("Espèce non supportée.")
  
  # ---- Calcul de l’indice Wr ----
  
  data <- data |>
    filter(!is.na(ltm), !is.na(masse), ltm >= constantes_wr$min_TL) |>
    mutate(
      prediction = 10 ^ (constantes_wr$int + constantes_wr$slope * log10(ltm)),
      wr = masse * 100 / prediction
    )
  
  if (nrow(data) == 0) {
    table_vide <- tibble(groupe = character(), wr = numeric(), ic95 = character(), n = integer())
    return(list(
      data = table_vide,
      flextable = flextable(table_vide),
      plot_tous = ggplot(),
      plot_byclass = ggplot()
    ))
  }
  
  # ---- Graphique Wr par sexe (plot_tous) ----
  
  niveaux_sexe <- c("F", "M", "IND")
  data <- mutate(data, sexe = factor(sexe, levels = niveaux_sexe))
  
  moyenne_par_sexe <- tibble(sexe = factor(niveaux_sexe, levels = niveaux_sexe)) |>
    left_join(
      data |> group_by(sexe) |> summarise(moyenne = mean(wr, na.rm = TRUE), .groups = "drop"),
      by = "sexe"
    )
  moyenne_totale <- mean(data$wr, na.rm = TRUE)
  
  plot_tous <- ggplot(data, aes(x = ltm, y = wr, color = sexe)) +
    geom_point(alpha = 0.8) +
    scale_color_manual(
      values = group_colors$sexe,
      labels = group_labels$sexe,
      name = "",
      drop = FALSE
    ) +
    labs(x = "Longueur totale maximale (mm)", y = "Indice de condition (%)") +
    annotate(
      "segment",
      x = -Inf, xend = Inf, y = 100, yend = 100,
      color = "lightgrey", linewidth = 0.5, linetype = 2
    ) +
    geom_hline(
      data = moyenne_par_sexe,
      aes(yintercept = moyenne, color = sexe),
      linetype = 2, linewidth = 0.5
    ) +
    # --- Ligne rouge (moyenne totale) + entrée de légende via linetype
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
      linetype = guide_legend(order = 2, override.aes = list(color = "red"))
    ) +
    theme_aquapop()
  
  # ---- Graphique Wr par classe de taille (plot_byclass) ----
  
  data <- data |>
    mutate(
      classe_brute = lencat(ltm, breaks = info_pen$breaks, as.fact = TRUE),
      classe = recode(as.character(classe_brute), !!!setNames(psd_classnames, as.character(info_pen$breaks))),
      intervalle = recode(as.character(classe_brute), !!!setNames(info_pen$break_labels, as.character(info_pen$breaks)))
    )
  
  sommaire_classe <- data |>
    count(classe_brute, classe, intervalle, name = "n")
  
  data <- left_join(data, sommaire_classe, by = c("classe_brute", "classe", "intervalle"))
  
  if (n_distinct(data$classe_brute) >= 2) {
    modele <- lm(wr ~ classe_brute, data = data)
    niveaux <- levels(droplevels(data$classe_brute))
    grille <- tibble(classe_brute = factor(niveaux, levels = levels(data$classe_brute)))
    
    prediction <- predict(modele, newdata = grille, interval = "confidence") |>
      as.data.frame() |>
      bind_cols(grille) |>
      left_join(sommaire_classe, by = "classe_brute")
    
    plot_byclass <- ggplot(prediction, aes(x = classe, y = fit)) +
      geom_point() +
      geom_point(data = data, aes(x = classe, y = wr),
                          shape = 21, colour = "black", fill = "white", size = 1, alpha = 0.5) +
      geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.1) +
      xlab("Classe de taille") +
      ylab("Indice de condition (%)") +
      scale_x_discrete(limits = psd_classnames, drop = FALSE) +
      annotate("segment", x = -Inf, xend = Inf, y = 100, yend = 100,
                        linewidth = 0.5, color = "black", linetype = 2) +
      theme_aquapop()
  } else {
    plot_byclass <- ggplot()
  }
  
  # ---- Construction des tableaux de synthèse ----
  
  table_tous <- predict(lm(wr ~ 1, data = data), newdata = data.frame(groupe = "Tous"), interval = "confidence") |>
    as.data.frame() |>
    mutate(
      groupe = "Tous",
      ic95 = paste0("[", round(lwr), "-", round(upr), "]"),
      wr = round(fit),
      n = nrow(data)
    ) |>
    select(groupe, wr, ic95, n)
  
  table_par_sexe <- if (n_distinct(data$sexe) >= 2) {
    resumer_wr_par_groupe(lm(wr ~ sexe, data = data), "sexe") |>
      filter(groupe %in% c("F", "M")) |>
      mutate(groupe = mapvalues(groupe, c("F", "M"), c("Femelle", "Mâle")))
  } else {
    tibble(groupe = character(), wr = numeric(), ic95 = character(), n = integer())
  }
  
  table_par_classe <- if (n_distinct(data$classe) >= 2) {
    resumer_wr_par_groupe(lm(wr ~ classe, data = data), "classe") |>
      complete(groupe = psd_classnames, fill = list(wr = 0, ic95 = "0", n = 0)) |>
      mutate(groupe = factor(groupe, levels = psd_classnames)) |>
      arrange(groupe)
  } else {
    tibble(groupe = factor(psd_classnames, levels = psd_classnames),
                   wr = 0, ic95 = "0", n = 0)
  }
  
  table_sommaire <- bind_rows(table_tous, table_par_sexe, table_par_classe)
  
  table_flextable <- flextable(table_sommaire) |>
    set_caption("Indice de condition (Wr)") |>
    set_header_labels(values = list(
                        groupe = "Groupe",
                        wr = "Wr (%)",
                        ic95 = "IC 95%",
                        n = "N"
                      )
    ) |>
    style_flextable_aquapop()
  
  # ---- Retour de la liste finale ----
  
  return(list(
    data = table_sommaire,
    flextable = table_flextable,
    plot_tous = plot_tous,
    plot_byclass = plot_byclass
  ))
}



#' Résumer les résultats d’un modèle Wr par groupe (sexe ou classe)
#'
#' Cette fonction applique un modèle linéaire et génère un tableau de prédictions
#' avec intervalles de confiance et effectifs pour chaque modalité du groupe spécifié.
#'
#' @param mod Un objet `lm` ajusté sur `Wri` en fonction du groupe (`sexe` ou `Classe`)
#' @param var Nom du groupe à utiliser (`"sexe"` ou `"Classe"`)
#'
#' @return Un data.frame contenant les colonnes `groupe`, `wr`, `ic95`, `n`
#' @keywords internal
resumer_wr_par_groupe <- function(mod, var) {
  valeurs <- unique(as.character(mod$model[[var]]))
  nd <- tibble(!!sym(var) := valeurs)
  
  pred <- predict(mod, newdata = nd, interval = "confidence") |>
    as.data.frame() |>
    mutate(groupe = valeurs) |>
    mutate(
      ic95 = paste0("[", round(lwr), "-", round(upr), "]"),
      wr = round(fit)
    ) |>
    select(groupe, wr, ic95)
  
  counts <- mod$model |>
    count(!!sym(var)) |>
    rename(groupe = !!rlang::sym(var))
  
  left_join(pred, counts, by = "groupe")
}

#' Récupérer les constantes Wr pour une espèce donnée
#'
#' Cette fonction retourne les coefficients de référence pour le calcul de l’indice
#' de condition (Wr) pour une espèce supportée, à partir de la table `wr_constants`.
#'
#' @param espece Code d’espèce (ex: "SANA", "SAFO", "SAVI")
#'
#' @return Un `data.frame` avec les colonnes `min_TL`, `int`, `slope`, etc.
#' @keywords internal
get_wr_constants <- function(espece) {
  wr_constants |>
    filter(sp == espece) |>
    slice(1)
}
