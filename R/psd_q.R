#' Calculer l'indice PSD-Q global (Proportional Size Distribution – Quality)
#'
#' Cette fonction calcule l'indice PSD-Q pour une espèce cible, à partir des longueurs des spécimens capturés.
#' L'indice PSD-Q correspond à la proportion d'individus situés dans les classes de qualité (Q), définies
#' par des seuils spécifiques à chaque espèce.
#'
#' @importFrom flextable flextable set_header_labels
#' @importFrom dplyr select rename mutate filter
#' @importFrom glue glue
#' @importFrom FSA psdCI lencat
#'
#' @param data Un `data.frame` contenant les données pour une seule espèce.
#'
#' @return Une liste nommée contenant :
#' \describe{
#'   \item{success}{Indique si l'analyse a pu être produite}
#'   \item{data}{Un `data.frame` avec la valeur de l'indice PSD-Q et son intervalle de confiance à 95 %}
#'   \item{flextable}{Une version formatée du tableau}
#'   \item{message}{Message explicatif si l'analyse n'est pas disponible}
#' }
#'
#' @examples
#' set.seed(123)
#' data_ex <- data.frame(
#'   ltm = stats::rnorm(100, mean = 250, sd = 50),
#'   sp = "SAFO"
#' )
#' data_ex <- dplyr::filter(data_ex, ltm > 0)
#' psd_q(data_ex)
#'
#' @export
psd_q <- function(data) {
  
  # Validation ----
  if (!is.data.frame(data)) {
    stop("`data` doit être un data.frame.")
  }
  
  if (!all(c("ltm", "sp") %in% colnames(data))) {
    stop("Le jeu de données doit contenir les colonnes `ltm` et `sp`.")
  }
  
  if (nrow(data) == 0) {
    return(list(
      success = FALSE,
      data = NULL,
      flextable = NULL,
      message = "Aucun spécimen valide disponible pour calculer l'indice PSD-Q."
    ))
  }
  
  sp <- as.character(unique(stats::na.omit(data$sp)))
  
  if (length(sp) != 1) {
    stop("Les données doivent être filtrées pour une seule espèce.")
  }
  
  info <- get_info_pen(sp)
  
  if (is.null(info)) {
    return(list(
      success = FALSE,
      data = NULL,
      flextable = NULL,
      message = "L'espèce sélectionnée n'est pas supportée pour le calcul de l'indice PSD-Q."
    ))
  }
  
  if (all(is.na(data$ltm))) {
    return(list(
      success = FALSE,
      data = NULL,
      flextable = NULL,
      message = "Aucune longueur exploitable n'est disponible pour calculer l'indice PSD-Q."
    ))
  }
  
  # Préparation ----
  break_class <- info$breaks
  seuil_qualite <- break_class[2]
  
  donnees_qualite <- data |>
    filter(!is.na(ltm), ltm >= seuil_qualite) |>
    mutate(gcat = lencat(ltm, breaks = break_class, droplevels = TRUE))
  
  freq_classes <- xtabs(~ gcat, data = donnees_qualite)
  
  if (sum(freq_classes) == 0) {
    return(list(
      success = FALSE,
      data = NULL,
      flextable = NULL,
      message = paste(
        "Aucun spécimen n'atteint la longueur minimale requise pour",
        "calculer l'indice PSD-Q."
      )
    ))
  }
  
  freq_relatives <- prop.table(freq_classes) * 100
  freq_vecteur <- apply(freq_relatives, 1, sum)
  
  poids_classes <- rep(1, length(freq_vecteur))
  poids_classes[1] <- 0
  
  if (all((freq_vecteur / 100)[poids_classes == 1] == 0)) {
    return(list(
      success = FALSE,
      data = NULL,
      flextable = NULL,
      message = paste(
        "Aucune donnée n'est disponible dans les classes pondérées",
        "pour calculer l'indice PSD-Q."
      )
    ))
  }
  
  # Calcul ----
  table_resultats <- psdCI(
    poids_classes,
    ptbl = freq_vecteur / 100,
    n = sum(freq_classes),
    method = "binomial",
    label = "PSD Q"
  ) |>
    as.data.frame() |>
    rename(
      Q = Estimate,
      LCI = `95% LCI`,
      UCI = `95% UCI`
    ) |>
    mutate(
      ic95 = glue("[{round(LCI, 1)}-{round(UCI, 1)}]")
    ) |>
    select(Q, ic95)
  
  table_flextable <- flextable(table_resultats) |>
    set_header_labels(values = list(
      Q = "Q",
      ic95 = "IC 95%"
    )) |>
    style_flextable_aquapop()
  
  return(list(
    success = TRUE,
    data = table_resultats,
    flextable = table_flextable,
    message = NULL
  ))
}