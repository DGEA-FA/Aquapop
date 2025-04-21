#' Calculer l’indice de condition (Wr) à partir des spécimens d’une espèce
#'
#' Cette fonction calcule l’indice de condition relatif (Wr) pour une espèce donnée, à partir des longueurs et masses
#' des spécimens mesurés. Elle retourne une liste incluant un tableau brut, une version formatée,
#' ainsi que deux graphiques : l’un pour l’ensemble des données, l’autre par classe de taille.
#'
#' @param data Un `data.frame` contenant les colonnes `sp`, `ltm`, `masse` et `sexe`
#'
#' @return Une liste avec les éléments :
#' \describe{
#'   \item{data}{Tableau synthèse (`data.frame`) des Wr moyens par groupe}
#'   \item{flextable}{Version formatée du tableau (objet `flextable`)}
#'   \item{plot_tous}{Graphique Wr selon la longueur, coloré par sexe}
#'   \item{plot_byclass}{Graphique Wr par classe de taille}
#' }
#'
#' @examples
#' df <- data.frame(
#'   sp = rep("SANA", 10),
#'   ltm = seq(300, 500, by = 20),
#'   masse = seq(200, 400, by = 20),
#'   sexe = rep(c("F", "M"), 5)
#' )
#' wri(df)
#'
#' @export
wri <- function(data) {
  
  # ---- Chargement et validation des données ----
  
  colonnes_essentielles <- c("sp", "ltm", "masse")
  colonnes_absentes <- setdiff(colonnes_essentielles, names(data))
  if (length(colonnes_absentes) > 0) {
    stop(glue::glue("Colonnes manquantes : {paste(colonnes_absentes, collapse = ', ')}"))
  }
  
  espece <- unique(data$sp)
  if (length(espece) != 1) stop("Les données doivent contenir une seule espèce.")
  
  info_pen <- get_info_pen(espece)
  constantes_wr <- get_wr_constants(espece)
  if (is.null(info_pen) || is.null(constantes_wr)) stop("Espèce non supportée.")
  
  # ---- Calcul de l’indice Wr ----
  
  data <- data |>
    dplyr::filter(!is.na(ltm), !is.na(masse), ltm >= constantes_wr$min_TL) |>
    dplyr::mutate(
      prediction = 10 ^ (constantes_wr$int + constantes_wr$slope * log10(ltm)),
      wr = masse * 100 / prediction
    )
  
  if (nrow(data) == 0) {
    table_vide <- tibble::tibble(groupe = character(), wr = numeric(), ic95 = character(), n = integer())
    return(list(
      data = table_vide,
      flextable = flextable::flextable(table_vide),
      plot_tous = ggplot2::ggplot(),
      plot_byclass = ggplot2::ggplot()
    ))
  }
  
  # ---- Graphique Wr par sexe (plot_tous) ----
  
  niveaux_sexe <- c("F", "M", "IND")
  data <- dplyr::mutate(data, sexe = factor(sexe, levels = niveaux_sexe))
  
  moyenne_par_sexe <- tibble::tibble(sexe = factor(niveaux_sexe, levels = niveaux_sexe)) |>
    dplyr::left_join(
      data |> dplyr::group_by(sexe) |> dplyr::summarise(moyenne = mean(wr, na.rm = TRUE), .groups = "drop"),
      by = "sexe"
    )
  moyenne_totale <- mean(data$wr, na.rm = TRUE)
  
  plot_tous <- ggplot2::ggplot(data, ggplot2::aes(x = ltm, y = wr, color = sexe)) +
    ggplot2::geom_point(alpha = 0.8) +
    ggplot2::scale_color_manual(values = group_colors$sexe, labels = group_labels$sexe, name = "", drop = FALSE) +
    ggplot2::labs(x = "Longueur totale maximale (mm)", y = "Indice de condition (%)") +
    ggplot2::annotate("segment", x = -Inf, xend = Inf, y = 100, yend = 100,
                      color = "lightgrey", linewidth = 0.5, linetype = 2) +
    ggplot2::geom_hline(data = moyenne_par_sexe, ggplot2::aes(yintercept = moyenne, color = sexe),
                        linetype = 2, linewidth = 0.5) +
    ggplot2::geom_hline(yintercept = moyenne_totale, color = "red", linetype = 2, linewidth = 0.5) +
    theme_aquapop()
  
  # ---- Graphique Wr par classe de taille (plot_byclass) ----
  
  data <- data |>
    dplyr::mutate(
      classe_brute = FSA::lencat(ltm, breaks = info_pen$breaks, as.fact = TRUE),
      classe = dplyr::recode(as.character(classe_brute), !!!setNames(psd_classnames, as.character(info_pen$breaks))),
      intervalle = dplyr::recode(as.character(classe_brute), !!!setNames(info_pen$break_labels, as.character(info_pen$breaks)))
    )
  
  sommaire_classe <- data |>
    dplyr::count(classe_brute, classe, intervalle, name = "n")
  
  data <- dplyr::left_join(data, sommaire_classe, by = c("classe_brute", "classe", "intervalle"))
  
  if (dplyr::n_distinct(data$classe_brute) >= 2) {
    modele <- lm(wr ~ classe_brute, data = data)
    niveaux <- levels(droplevels(data$classe_brute))
    grille <- tibble::tibble(classe_brute = factor(niveaux, levels = levels(data$classe_brute)))
    
    prediction <- predict(modele, newdata = grille, interval = "confidence") |>
      as.data.frame() |>
      dplyr::bind_cols(grille) |>
      dplyr::left_join(sommaire_classe, by = "classe_brute")
    
    plot_byclass <- ggplot2::ggplot(prediction, ggplot2::aes(x = classe, y = fit)) +
      ggplot2::geom_point() +
      ggplot2::geom_point(data = data, ggplot2::aes(x = classe, y = wr),
                          shape = 21, colour = "black", fill = "white", size = 1, alpha = 0.5) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = lwr, ymax = upr), width = 0.1) +
      ggplot2::xlab("Classe de taille") +
      ggplot2::ylab("Indice de condition (%)") +
      ggplot2::scale_x_discrete(limits = psd_classnames, drop = FALSE) +
      ggplot2::annotate("segment", x = -Inf, xend = Inf, y = 100, yend = 100,
                        linewidth = 0.5, color = "black", linetype = 2) +
      theme_aquapop()
  } else {
    plot_byclass <- ggplot2::ggplot()
  }
  
  # ---- Construction des tableaux de synthèse ----
  
  table_tous <- predict(lm(wr ~ 1, data = data), newdata = data.frame(groupe = "Tous"), interval = "confidence") |>
    as.data.frame() |>
    dplyr::mutate(
      groupe = "Tous",
      ic95 = paste0("[", round(lwr), "-", round(upr), "]"),
      wr = round(fit),
      n = nrow(data)
    ) |>
    dplyr::select(groupe, wr, ic95, n)
  
  table_par_sexe <- if (dplyr::n_distinct(data$sexe) >= 2) {
    resumer_wr_par_groupe(lm(wr ~ sexe, data = data), "sexe") |>
      dplyr::filter(groupe %in% c("F", "M")) |>
      dplyr::mutate(groupe = plyr::mapvalues(groupe, c("F", "M"), c("Femelle", "Mâle")))
  } else {
    tibble::tibble(groupe = character(), wr = numeric(), ic95 = character(), n = integer())
  }
  
  table_par_classe <- if (dplyr::n_distinct(data$classe) >= 2) {
    resumer_wr_par_groupe(lm(wr ~ classe, data = data), "classe") |>
      tidyr::complete(groupe = psd_classnames, fill = list(wr = 0, ic95 = "0", n = 0)) |>
      dplyr::mutate(groupe = factor(groupe, levels = psd_classnames)) |>
      dplyr::arrange(groupe)
  } else {
    tibble::tibble(groupe = factor(psd_classnames, levels = psd_classnames),
                   wr = 0, ic95 = "0", n = 0)
  }
  
  table_sommaire <- dplyr::bind_rows(table_tous, table_par_sexe, table_par_classe)
  
  table_flextable <- flextable::flextable(table_sommaire) |>
    flextable::set_caption("Indice de condition (Wr)") |>
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
  nd <- tibble::tibble(!!rlang::sym(var) := valeurs)
  
  pred <- predict(mod, newdata = nd, interval = "confidence") |>
    as.data.frame() |>
    dplyr::mutate(groupe = valeurs) |>
    dplyr::mutate(
      ic95 = paste0("[", round(lwr), "-", round(upr), "]"),
      wr = round(fit)
    ) |>
    dplyr::select(groupe, wr, ic95)
  
  counts <- mod$model |>
    dplyr::count(!!rlang::sym(var)) |>
    dplyr::rename(groupe = !!rlang::sym(var))
  
  dplyr::left_join(pred, counts, by = "groupe")
}

#' Récupérer les constantes Wr pour une espèce donnée
#'
#' Cette fonction retourne les coefficients de référence pour le calcul de l’indice
#' de condition (Wr) pour une espèce supportée, à partir de la table `wr_constants`.
#'
#' @param sp Code d’espèce (ex: "SANA", "SAFO", "SAVI")
#'
#' @return Un `data.frame` avec les colonnes `min_TL`, `int`, `slope`, etc.
#' @keywords internal
get_wr_constants <- function(sp) {
  wr_constants |>
    dplyr::filter(sp == sp) |>
    dplyr::slice(1)
}
