#' Calculer la fréquence par classe PSD pour une espèce donnée
#'
#' Cette fonction retourne le nombre de poissons et leur fréquence relative (%) pour chaque
#' classe de taille du PSD. Elle s'appuie sur les seuils définis dans `get_info_pen()` pour
#' l'espèce analysée. Les données doivent être filtrées pour contenir une seule espèce.
#'
#' La fonction retourne un tableau de synthèse prêt à l'usage, un graphique et un `flextable`
#' formaté pour des rapports Word ou des interfaces Shiny.
#'
#' @importFrom flextable flextable
#' @importFrom ggplot2 scale_x_discrete ylab scale_y_continuous xlab geom_bar aes ggplot
#' @importFrom tidyr replace_na
#' @importFrom tibble tibble
#' @importFrom dplyr select rename
#' @importFrom plyr mapvalues
#' @importFrom FSA lencat
#' @importFrom labelled set_variable_labels
#'
#' @param data Un `data.frame` filtré pour une seule espèce, contenant les colonnes `ltm` (longueur en mm)
#'   et `sp` (code espèce). La variable `ltm` est utilisée pour attribuer chaque poisson à une classe PSD.
#'
#' @return Une liste contenant :
#' \describe{
#'   \item{`data`}{Un `data.frame` résumant les fréquences par classe PSD, avec colonnes `classe`,
#'     `Intervalle (mm)`, `n` et `%`.}
#'   \item{`flextable`}{Une version formatée du tableau (pour Word, Shiny, etc.).}
#'   \item{`plot`}{Un graphique en barres montrant la fréquence relative par classe.}
#' }
#'
#' @seealso [get_info_pen()], [psd_classnames]
#'
#' @examples
#' # Exemple avec données simulées
#' data_ex <- tibble::tibble(
#'   sp = rep("SANA", 100),
#'   ltm = sample(100:1000, 100, replace = TRUE)
#' )
#' psd_byclass(data_ex)
#'
#' @export
psd_byclass <- function(data) {
  # --- Validation ---
  espece <- unique(data$sp)
  if (length(espece) != 1) stop("Les données doivent être filtrées pour une seule espèce.")
  
  info <- get_info_pen(espece)
  if (is.null(info)) stop("Espèce non supportée.")
  
  # --- Catégorisation ---
  seuils_psd <- info$breaks               # Seuils de classes PSD 
  etiquettes <- info$break_labels         # Libellés d'intervalles 
  seuil_min_stock <- seuils_psd[2]        # Seuil minimum de la première classe PSD réelle 
  noms_classes <- psd_classnames          # Libellés standards 
  
  # Découpe des tailles en classes et attribution des étiquettes
  donnees_classes <- data |>
    filter(ltm >= seuil_min_stock) |>
    mutate(
      gcat = lencat(ltm, breaks = seuils_psd, droplevels = TRUE),
      classe = mapvalues(gcat, from = seuils_psd, to = noms_classes, warn_missing = FALSE),
      intervalle = mapvalues(gcat, from = seuils_psd, to = etiquettes, warn_missing = FALSE)
    )
  
  # --- Calculs ---
  n_par_classe <- donnees_classes |>
    group_by(gcat, classe, intervalle) |>
    summarise(n = n(), .groups = "keep") |>
    droplevels()
  
  donnees_classes <- merge(donnees_classes, n_par_classe, by = c("gcat", "classe", "intervalle"))
  
  table_frequence <- (prop.table(xtabs(~ gcat, data = donnees_classes)) * 100) |>
    as.data.frame() |>
    rename(freq = Freq)
  
  donnees_classes <- merge(donnees_classes, table_frequence, by = "gcat")
  
  table_resumee <- donnees_classes |>
    select(classe, intervalle, n, freq) |>
    group_by(classe, intervalle, n, freq) |>
    summarise(.groups = "keep")
  
  structure_complete <- tibble(
    classe = noms_classes,
    intervalle = etiquettes
  )
  
  table_finale <- merge(
    structure_complete,
    table_resumee,
    by = c("classe", "intervalle"),
    all.x = TRUE
  ) %>%
    mutate(
      classe = factor(classe, levels = noms_classes),
      freq   = round(as.numeric(freq), 0),
      freq   = ifelse(is.na(freq), "0", freq),
      n      = replace_na(n, 0)
    ) %>%
    arrange(classe)
  
  # Ajout de la classe <P (non incluse dans gcat)
  table_finale[1, "n"] <- data %>%
    filter(sp == espece, ltm < seuil_min_stock) |>
    summarise(n = n()) |>
    pull(n)
  
  
  
  # Labellisation des colonnes pour export
  table_finale <- set_variable_labels(
    table_finale,
    classe = "Classe",
    intervalle = "Intervalle (mm)",
    n = "n",
    freq = "%"
  )
  # --- Flextable ---
  ft <- flextable(table_finale)
  ft <- labelled_data(ft)
  ft <- style_flextable_aquapop(ft)
  
  # --- Graphique ---
  table_finale$freq <- as.numeric(table_finale$freq)
  fig <- ggplot(table_finale, aes(x = classe, y = freq)) +
    geom_bar(stat = "identity") +
    geom_text_aquapop(aes(label = paste0("n = ", n)), nudge_y = 3) +
    xlab("Classe de taille") +
    ylab("Fréquence relative (%)") +
    theme_aquapop() +
    scale_y_continuous(expand = c(0, 0.1), limits = c(0, 110)) +
    scale_x_discrete(limits = noms_classes)

  
  return(list(
    data = table_finale,
    flextable = ft,
    plot = fig
  ))
}
