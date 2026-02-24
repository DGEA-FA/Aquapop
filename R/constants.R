#' Constantes associees aux especes suivies par PEN
#'
#' Contient le nom commun, le binwidth recommande pour les histogrammes,
#' les seuils de classes PSD (`breaks`) et leurs libelles (`break_labels`) par espece (`sp`).
#'
#' @importFrom FSA wsVal
#' @importFrom tibble tibble
#' @format Un `tibble` avec une ligne par espece.
#' @export
pen_constants <- tibble::tibble(
  sp       = c("SANA", "SAFO", "SAVI"),
  nom_sp   = c("touladis", "ombles de fontaine", "dorés jaunes"),
  binwidth = c(50, 20, 50),
  breaks   = list(
    c(0, 300, 500, 650, 800, 1000),
    c(0, 150, 250, 325, 400, 500),
    c(0, 250, 380, 510, 630, 760)
  ),
  break_labels = list(
    c("<300", "300-499", "500-649", "650-799", "800-999", ">=1000"),
    c("<150", "150-249", "250-324", "325-399", "400-499", ">=500"),
    c("<250", "250-379", "380-509", "510-629", "630-759", ">=760")
  )
)

#' Noms standardises des classes PSD
#'
#' Utilises dans les fonctions PSD (`psd_q`, `psd_byclass`, etc.)
#'
#' @export
psd_classnames <- c("Sous-stock", "Stock", "Qualité", "Préférée", "Mémorable", "Trophée")

#' Constantes pour le calcul de l'indice de condition (Wr)
#'
#' Source : `wsVal()` pour touladi, omble de fontaine, dore jaune
#'
#' @format Un `tibble` avec les colonnes :
#' - `sp` : Code d'espece (SANA, SAFO, SAVI)
#' - `species` : Nom anglais de l'espece
#' - `min_TL` : Longueur minimale (mm)
#' - `int` : Intercept de la regression log-log
#' - `slope` : Pente de la regression log-log
#' - `source` : Reference source
#' @importFrom tibble tibble
#' @export
wr_constants <-  tibble::tibble(
  sp      = c("SANA", "SAFO", "SAVI"),
  species = c("Lake Trout", "Brook Trout", "Walleye"),
  min_TL  = c(280, 120, 150),
  int     = c(-5.681, -5.186, -5.453),
  slope   = c(3.246, 3.103, 3.180),
  source  = c(
    "Piccolo et al. (1993)",
    "Hyatt and Hubert (2001a)",
    "Murphy et al. (1990)"
  )
)

#' @keywords internal
couleur_default <- "#084594"

#' @keywords internal
group_labels <- list(
  "sexe"     = c("F" = "Femelle", "M" = "Mâle", "IND" = "Indéterminé"),
  "maturite" = c("O" = "Mature", "N" = "Immature", "IND" = "Indéterminé"),
  "marquage" = c("MA" = "Marqué", "NMA" = "Non marqué"),
  "tous"     = c("TOUS" = "Tous")
)

#' @keywords internal
group_colors <- list(
  "sexe"     = c("F" = couleur_default, "M" = "#99CCFF", "IND" = "#4d4d4d"),
  "maturite" = c("O" = couleur_default, "N" = "#99CCFF", "IND" = "#4d4d4d"),
  "marquage" = c("MA" = couleur_default, "NMA" = "#99CCFF"),
  "tous"     = c("TOUS" = couleur_default)
)
