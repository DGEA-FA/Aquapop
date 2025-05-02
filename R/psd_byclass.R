#' Calculer la fréquence par classe PSD pour une espèce donnée
#'
#' Cette fonction retourne le nombre de poissons et leur fréquence relative (%) pour chaque
#' classe de taille du PSD. Les données doivent être filtrées pour une seule espèce.
#'
#' @param data Un `data.frame` contenant au moins les colonnes `ltm` et `sp`.
#'
#' @return Une liste contenant :
#' \describe{
#'   \item{`data`}{Un `data.frame` résumant les fréquences par classe PSD.}
#'   \item{`flextable`}{Une version formatée du tableau (pour Word, Shiny, etc.).}
#'   \item{`plot`}{Un graphique en barres montrant la fréquence relative par classe.}
#' }
#' @export
psd_byclass <- function(data) {
  espece <- unique(data$sp)
  if (length(espece) != 1) stop("Les données doivent être filtrées pour une seule espèce.")
  
  info <- get_info_pen(espece)
  if (is.null(info)) stop("Espèce non supportée.")
  
  seuils_psd <- info$breaks
  etiquettes <- info$break_labels
  seuil_min_stock <- seuils_psd[2]
  noms_classes <- psd_classnames
  
  donnees_classes <- data |>
    filter(ltm >= seuil_min_stock) |>
    mutate(
      gcat = FSA::lencat(ltm, breaks = seuils_psd, droplevels = TRUE),
      Classe = plyr::mapvalues(gcat, from = seuils_psd, to = noms_classes, warn_missing = FALSE),
      intervalle = plyr::mapvalues(gcat, from = seuils_psd, to = etiquettes, warn_missing = FALSE)
    )
  
  n_par_classe <- donnees_classes |>
    group_by(gcat, Classe, intervalle) %>%
    summarise(n = n(), .groups = "keep") %>%
    droplevels()
  
  donnees_classes <- merge(donnees_classes, n_par_classe, by = c("gcat", "Classe", "intervalle"))
  
  table_frequence <- (prop.table(xtabs(~ gcat, data = donnees_classes)) * 100) %>%
    as.data.frame()
  
  donnees_classes <- merge(donnees_classes, table_frequence, by = "gcat")
  
  table_resumee <- donnees_classes %>%
    dplyr::select(Classe, intervalle, n, Freq) %>%
    group_by(Classe, intervalle, n, Freq) %>%
    summarise(.groups = "keep")
  
  structure_complete <- tibble::tibble(
    Classe = noms_classes,
    intervalle = etiquettes
  )
  
  table_finale <- merge(
    structure_complete,
    table_resumee,
    by = c("Classe", "intervalle"),
    all.x = TRUE
  ) %>%
    mutate(
      Classe = factor(Classe, levels = noms_classes),
      Freq   = round(as.numeric(Freq), 0),
      Freq   = ifelse(is.na(Freq), "0", Freq),
      n      = tidyr::replace_na(n, 0)
    ) %>%
    arrange(Classe)
  
  table_finale[1, "n"] <- data %>%
    filter(sp == espece, ltm < seuil_min_stock) %>%
    summarise(n = n()) %>%
    pull(n)
  
  colnames(table_finale)[2:4] <- c("Intervalle (mm)", "n", "%")
  
  # -- Graphique
  table_finale$`%` <- as.numeric(table_finale$`%`)
  fig <- ggplot2::ggplot(table_finale, ggplot2::aes(x = Classe, y = `%`)) +
    ggplot2::geom_bar(stat = "identity") +
    geom_text_aquapop(aes(label = paste0("n = ", n)), nudge_y = 3) +
    ggplot2::xlab("Classe de taille") +
    ggplot2::ylab("Fréquence relative (%)") +
    theme_aquapop() +
    ggplot2::scale_y_continuous(expand = c(0, 0.1), limits = c(0, 110)) +
    ggplot2::scale_x_discrete(limits = noms_classes)
  

  # -- Flextable
  ft <- flextable::flextable(table_finale) %>%
    style_flextable_aquapop()
  
  return(list(
    data = table_finale,
    flextable = ft,
    plot = fig
  ))
}
