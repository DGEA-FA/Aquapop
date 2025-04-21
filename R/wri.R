#' Calculer l'indice de condition (Wr) des spécimens d'une espèce
#'
#' Cette fonction calcule l’indice de condition Wr pour une espèce donnée.
#' Elle retourne une liste contenant :
#' - un tableau synthèse (`data`)
#' - une version formatée (`flextable`)
#' - un graphique de dispersion (`plot_tous`)
#' - un graphique par classe de taille (`plot_byclass`)
#'
#' @param data Données brutes de spécimens (`data.frame` avec colonnes `sp`, `ltm`, `masse`, etc.)
#'
#' @return Une liste avec les éléments `data`, `flextable`, `plot_tous`, `plot_byclass`
#' @export
wri <- function(data) {
  
  # Chargement et validation des données ----
  
  espece <- unique(data$sp)
  if (length(espece) != 1) stop("Les données doivent contenir une seule espèce.")
  
  info <- get_info_pen(espece)
  ref <- get_wr_constants(espece)
  
  if (is.null(info) || is.null(ref)) stop("Espèce non supportée.")
  
  data <- data |>
    dplyr::filter(!is.na(ltm), !is.na(masse), ltm >= ref$min_TL) |>
    dplyr::mutate(
      prediction = 10 ^ (ref$int + ref$slope * log10(ltm)),
      Wri = masse * 100 / prediction
    )
  
  if (nrow(data) == 0) {
    vide <- tibble::tibble()
    return(list(
      data = vide,
      flextable = flextable::flextable(vide),
      plot_tous = ggplot2::ggplot(),
      plot_byclass = ggplot2::ggplot()
    ))
  }
  
  # Graphique Wr par sexe ----
  sexe_niveaux <- c("F", "M", "IND")
  data <- data |>
    dplyr::mutate(sexe = factor(sexe, levels = sexe_niveaux))
  
  moyennes <- tibble::tibble(
    sexe = factor(sexe_niveaux, levels = sexe_niveaux)
  ) |>
    dplyr::left_join(
      data |>
        dplyr::group_by(sexe) |>
        dplyr::summarise(moy = mean(Wri, na.rm = TRUE), .groups = "drop"),
      by = "sexe"
    )
  
  moy_tous <- mean(data$Wri, na.rm = TRUE)
  
  fig_tous <- ggplot2::ggplot(data, ggplot2::aes(x = ltm, y = Wri, color = sexe)) +
    ggplot2::geom_point(alpha = 0.8) +
    ggplot2::scale_color_manual(
      values = group_colors$sexe,
      labels = group_labels$sexe,
      name = "", drop = FALSE
    ) +
    theme_aquapop() + 
    
    ggplot2::labs(x = "Longueur totale maximale (mm)", y = "Indice de condition (%)") +
 
    ggplot2::annotate("segment", x = -Inf, xend = Inf, y = 100, yend = 100,
                      color = "lightgrey", linewidth = 0.5, linetype = 2
    ) +
    ggplot2::geom_hline(data = moyennes, ggplot2::aes(yintercept = moy, color = sexe),
                        linetype = 2, linewidth = 0.5) +
    ggplot2::geom_hline(yintercept = moy_tous, color = "red", linetype = 2, linewidth = 0.5)
  
  # Graphique Wr par classe de taille ----
  breaks <- info$breaks
  labels <- info$break_labels
  
  data <- data |>
    dplyr::mutate(
      gcat = FSA::lencat(ltm, breaks = breaks, as.fact = TRUE),
      Classe = dplyr::recode(as.character(gcat), !!!setNames(psd_classnames, as.character(breaks))),
      Intervalle = dplyr::recode(as.character(gcat), !!!setNames(labels, as.character(breaks)))
    )
  
  sommaire <- data |>
    dplyr::group_by(gcat, Classe, Intervalle) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop")
  
  data <- dplyr::left_join(data, sommaire, by = c("gcat", "Classe", "Intervalle"))
  
  aov1 <- lm(Wri ~ gcat, data = data)
  gcat_obs <- levels(droplevels(data$gcat))
  nd <- tibble::tibble(gcat = factor(gcat_obs, levels = levels(data$gcat)))
  
  pred <- predict(aov1, newdata = nd, interval = "confidence") |>
    as.data.frame() |>
    dplyr::bind_cols(nd) |>
    dplyr::left_join(sommaire, by = "gcat")
  
  fig_byclass <- ggplot2::ggplot(pred, ggplot2::aes(x = Classe, y = fit)) +
    ggplot2::geom_point() +
    ggplot2::geom_point(data = data,
                        ggplot2::aes(x = Classe, y = Wri),
                        shape = 21, colour = "black", fill = "white", size = 1, alpha = 0.5) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lwr, ymax = upr), width = 0.1) +
    ggplot2::xlab("Classe de taille") +
    ggplot2::ylab("Indice de condition (%)") +
    theme_aquapop() +
    ggplot2::scale_x_discrete(limits = psd_classnames, drop = FALSE) +
    ggplot2::annotate("segment", x = -Inf, xend = Inf, y = 100, yend = 100,
                      linewidth = 0.5, color = "black", linetype = 2)
  
  # Construction des tableaux synthèse ----
 
  
  tab_all <- predict(lm(Wri ~ 1, data = data), newdata = data.frame(Groupe = "Tous"), interval = "confidence") |>
    as.data.frame() |>
    dplyr::mutate(
      Groupe = "Tous",
      IC95 = paste0("[", round(lwr), "-", round(upr), "]"),
      Wr = round(fit),
      n = nrow(data)
    ) |>
    dplyr::select(Groupe, Wr, IC95, n)
  
  tab_sexe <- resumer_wr_par_groupe(lm(Wri ~ sexe, data = data), "sexe") |>
    dplyr::filter(Groupe %in% c("F", "M")) |>
    dplyr::mutate(Groupe = plyr::mapvalues(Groupe, c("F", "M"), c("Femelle", "Mâle")))
  
  tab_class <- resumer_wr_par_groupe(lm(Wri ~ Classe, data = data), "Classe") |>
    tidyr::complete(Groupe = psd_classnames, fill = list(Wr = 0, IC95 = "0", n = 0)) |>
    dplyr::mutate(Groupe = factor(Groupe, levels = psd_classnames)) |>
    dplyr::arrange(Groupe)
  
  table_finale <- dplyr::bind_rows(tab_all, tab_sexe, tab_class)
  
  table_wr_flextable <- flextable::flextable(table_finale) |>
    flextable::set_caption("Indice de condition (Wr)") |>
    style_flextable_aquapop()
  
  # Retour de la liste finale ----
  
  return(list(
    data = table_finale,
    flextable = table_wr_flextable,
    plot_tous = fig_tous,
    plot_byclass = fig_byclass
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
#' @return Un data.frame contenant les colonnes `Groupe`, `Wr`, `IC95`, `n`
#' @keywords internal
resumer_wr_par_groupe <- function(mod, var) {
  valeurs <- unique(as.character(mod$model[[var]]))
  nd <- tibble::tibble(!!rlang::sym(var) := valeurs)
  
  pred <- predict(mod, newdata = nd, interval = "confidence") |>
    as.data.frame() |>
    dplyr::mutate(Groupe = valeurs) |>
    dplyr::mutate(
      IC95 = paste0("[", round(lwr), "-", round(upr), "]"),
      Wr = round(fit)
    ) |>
    dplyr::select(Groupe, Wr, IC95)
  
  counts <- mod$model |>
    dplyr::count(!!rlang::sym(var)) |>
    dplyr::rename(Groupe = !!rlang::sym(var))
  
  dplyr::left_join(pred, counts, by = "Groupe")
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
