#' Préparer les données agrégées de CPUE par station
#'
#' Cette fonction prépare un tableau de CPUE (captures par unité d'effort) à l'échelle de la station, 
#' à partir des données de captures et des spécimens observés. Elle permet de calculer soit la CPUE totale,
#' soit la CPUE restreinte aux femelles matures, selon l’argument `group`.
#'
#' Elle suppose que les objets `capture` et `specimen` sont déjà filtrés pour ne contenir que les stations valides et aléatoires 
#' (par exemple via `get_analysis_data()`) ainsi que les individus d’une seule espèce.
#'
#' @param capture Un `data.frame` de captures, contenant au minimum les colonnes `no_station`, `nb_capture`, `nb_pese`.
#' @param specimen Un `data.frame` de spécimens, déjà filtré pour une seule espèce, les stations valides et aléatoires.
#' @param group Une chaîne de caractères, `"tous"` (par défaut) ou `"femelles"`, indiquant le groupe à analyser.
#'
#' @return Un `data.frame` contenant les colonnes suivantes :
#' \describe{
#'   \item{no_station}{Identifiant de la station}
#'   \item{cpue}{Nombre d’individus du groupe, divisé par le nombre de filets (1 par station)}
#'   \item{group}{Libellé du groupe analysé : "Tous" ou "Femelles"}
#' }
#'
#' @details
#' Lorsque `group = "femelles"`, la fonction filtre les spécimens pour `sexe == "F"` et `maturite == "O"`. 
#' Dans ce cas, la colonne `maturite` doit obligatoirement être présente dans le tableau `specimen`.
#'
#' @importFrom dplyr filter group_by summarise left_join mutate select n
#' @importFrom checkmate assert_data_frame assert_choice
#' @importFrom rlang abort
#' @importFrom tidyr replace_na
#' @export
cpue_prepare <- function(capture, specimen, group = c("tous", "femelles")) {
  group <- match.arg(group)
  
  # --- Validation d'entrée ---
  assert_data_frame(capture, min.rows = 1)
  assert_data_frame(specimen, min.cols = 1)
  assert_choice(group, choices = c("tous", "femelles"))
  
  # --- Filtrage des spécimens selon le groupe ---
  if (group == "femelles") {
    if (!"maturite" %in% names(specimen)) {
      abort("La colonne `maturite` est requise dans `specimen` lorsque group = 'femelles'.")
    }
    specimen <- filter(specimen, sexe == "F", maturite == "O")
    group_label <- "Femelles"
  } else {
    group_label <- "Tous"
  }
  
  # --- Compter les spécimens par station ---
  nb_specimens <- specimen |>
    group_by(no_station) |>
    summarise(nb_specimens = n(), .groups = "drop")
  
  # --- Joindre avec la table des captures (pour conserver toutes les stations) ---
  cpue_par_station <- capture |>
    select(no_station) |>
    left_join(nb_specimens, by = "no_station") |>
    mutate(
      nb_specimens = replace_na(nb_specimens, 0L),
      cpue = nb_specimens,  # ici, effort = 1 filet/station
      group = group_label
    ) |>
    select(no_station, cpue, group)
  
  return(cpue_par_station)
}
