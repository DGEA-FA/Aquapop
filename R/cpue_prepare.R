#' Préparer les données agrégées de CPUE par station
#'
#' Cette fonction prépare un tableau de CPUE (captures par unité d'effort) à l’échelle de la station, 
#' à partir des données de captures et des spécimens observés. Elle permet de calculer soit la CPUE totale,
#' soit la CPUE restreinte aux femelles, selon l’argument `group`.
#'
#' Elle suppose que les objets `capture` et `specimen` sont déjà filtrés pour ne contenir que les stations valides et aléatoires 
#' (par exemple via `get_analysis_data()`) ainsi que les individus d’une seule espèce.
#'
#' @param capture Un `data.frame` de captures, contenant au minimum les colonnes `no_station`, `nb_capture`, `nb_pese`.
#' @param specimen Un `data.frame` de spécimens, déjà filtré pour une seule espèce, les stations valides et aléatoires.
#' @param group Une chaîne de caractères, "tous" (valeur par défaut) ou "femelles", indiquant le groupe à analyser.
#'
#' @return Un `data.frame` contenant les colonnes suivantes :
#' \describe{
#'   \item{no_station}{Identifiant de la station}
#'   \item{CPUE}{Nombre de spécimens observés à la station, divisé par l’effort (station)}
#'   \item{Group}{Libellé du groupe analysé : "Tous" ou "Femelles"}
#' }
#'
#' @importFrom dplyr filter group_by summarise right_join mutate
#'
#' @examples
#' capture <- tibble::tibble(
#'   no_station = c(1, 2),
#'   nb_capture = c(3, 2),
#'   nb_pese = c(3, 2)
#' )
#' specimen <- tibble::tibble(
#'   no_station = c(1, 1, 2, 2, 2),
#'   sexe = c("F", "M", "F", "F", "M")
#' )
#' cpue_prepare(capture, specimen, group = "femelles")
#'
#' @export
cpue_prepare <- function(capture, specimen, group = c("tous", "femelles")) {
  group <- match.arg(group)
  if (!group %in% c("tous", "femelles")) {
    stop("L’argument `group` doit être 'tous' ou 'femelles'.")
  }
  
  # --- Filtrage selon le groupe ---
  if (group == "femelles") {
    specimen <- filter(specimen, sexe == "F")
    group_label <- "Femelles"
  } else {
    group_label <- "Tous"
  }
  
  # --- Jointure spécimens / captures ---
  specimens_avec_stations <- right_join(specimen, capture, by = "no_station") |>
    mutate(count = ifelse(is.na(sexe), 0L, 1L))
  
  # --- Calcul de la CPUE par station ---
  cpue_par_station <- specimens_avec_stations |>
    group_by(no_station) |>
    summarise(
      CPUE = sum(count),
      Group = group_label,
      .groups = "drop"
    )
  
  return(cpue_par_station)
}
