#' Calcule la fréquence par classe PSD pour une espèce donnée
#'
#' Cette fonction retourne le nombre de poissons et leur fréquence relative (%) pour chaque
#' classe de taille du PSD. Les données doivent être filtrées pour une seule espèce.
#' Le résultat peut être retourné sous forme brute (`data.frame`), en tableau formaté (`flextable`), ou en graphique (`plot`).
#'
#' @param data Un `data.frame` contenant au moins les colonnes `ltm` et `sp`.
#' @param format Format de sortie : `"data.frame"` (par défaut), `"flextable"`, ou `"plot"`.
#'
#' @return Un tableau ou un graphique selon l'option choisie.
#' @export
psd_byclass <- function(data, format = c("data.frame", "flextable", "plot")) {
  format <- match.arg(format)
  
  espece <- unique(data$sp)
  if (length(espece) != 1) stop("Les données doivent être filtrées pour une seule espèce.")
  
  info <- get_info_pen(espece)
  if (is.null(info)) stop("Espèce non supportée.")
  
  seuils_psd <- info$breaks
  etiquettes <- info$break_labels
  seuil_min_stock <- seuils_psd[2]
  noms_classes <- psd_classnames
  
  donnees_classes <- data %>%
    filter(ltm >= seuil_min_stock) %>%
    mutate(
      gcat = FSA::lencat(ltm, breaks = seuils_psd, droplevels = TRUE),
      Classe = plyr::mapvalues(gcat, from = seuils_psd, to = noms_classes, warn_missing = FALSE),
      intervalle = plyr::mapvalues(gcat, from = seuils_psd, to = etiquettes, warn_missing = FALSE)
    )
  
  n_par_classe <- donnees_classes %>%
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
  
  structure_complete <- tibble(
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
  
  # ----- Sortie : plot -----
  if (format == "plot") {
    table_finale$`%` <- as.numeric(table_finale$`%`)
    
    return(
      ggplot(table_finale, aes(x = Classe, y = `%`)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = paste0("n = ", n)), nudge_y = 3) +
        xlab("Classe de taille") +
        ylab("Fréquence relative (%)") +
        theme_minimal(base_size = 11) +
        theme(
          panel.background = element_rect(
            fill = "white",
            colour = "white",
            linewidth = 0.5
          ),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.text.y.left = element_text(color = "black"),
          axis.text.x = element_text(color = "black"),
          axis.title.y.left = element_text(color = "black", hjust = 0.5),
          axis.title.x = element_text(color = "black", hjust = 0.5),
          plot.margin = unit(c(0.5, 0.1, 0.2, 0.1), "cm"),
          axis.line = element_line(colour = "black")
        ) +
        scale_y_continuous(expand = c(0, 0.1),
                           limits = c(0, 100)) +
        scale_x_discrete(limits = noms_classes)
    )
  }
  
  # ----- Sortie : flextable -----
  if (format == "flextable") {
    return(
      flextable::flextable(table_finale) %>%
        flextable::autofit() %>%
        flextable::align(align = "center", part = "all")
    )
  }
  
  # ----- Sortie : data.frame -----
  return(table_finale)
}
