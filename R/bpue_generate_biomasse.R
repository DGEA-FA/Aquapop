#' Créer une table de biomasse et BPUE par groupe biologique
#'
#' Cette fonction calcule la biomasse totale (kg), la proportion relative (%) et
#' la biomasse par unité d'effort (BPUE, en kg/station) pour différents groupes
#' biologiques d'une espèce cible.
#'
#' Elle est conçue pour être utilisée à la fois dans un script reproductible,
#' par exemple avec `specimen_hasard_valide` et `station_hasard_valide`, et dans
#' le module Shiny de biomasse-BPUE.
#'
#' Le tableau brut (`data`) conserve les valeurs numériques non arrondies pour
#' les colonnes `biomasse`, `percent` et `bpue`, afin de permettre leur
#' réutilisation dans d'autres analyses ou exports. Le formatage des décimales
#' est appliqué uniquement dans la version `flextable`.
#'
#' Les intervalles de confiance (`ic95`) sont calculés lorsque le modèle de
#' biomasse par station est ajusté, notamment pour le groupe complet et les
#' femelles reproductrices actives. Les groupes sans intervalle de confiance ont
#' une valeur `NA` dans la colonne `ic95`.
#'
#' @param specimen Un `data.frame` de spécimens filtrés pour les stations hasard et 
#' valides, contenant minimalement les colonnes `no_station`, `masse`, `sexe` et `maturite`.
#' @param station Un `data.frame` des stations hasard et valides, contenant
#'  minimalement la colonne `no_station`. Ce tableau est utilisé pour calculer
#'  l'effort d'échantillonnage.
#'
#' @return Une liste contenant :
#' \describe{
#'   \item{`data`}{Un `data.frame` résumant la biomasse totale, la proportion
#'   relative, la BPUE et les intervalles de confiance par groupe biologique.
#'   Les colonnes `biomasse`, `percent` et `bpue` sont numériques. La colonne
#'   `ic95` est de type caractère et contient `NA` lorsque l'intervalle n'est
#'   pas calculé.}
#'   \item{`flextable`}{Une version formatée du tableau (`flextable`) pour
#'   l'affichage dans Shiny ou l'exportation vers Word.}
#' }
#'
#' @examples
#' \dontrun{
#' table_biomasse <- bpue_generate_biomasse(
#'   specimen_hasard_valide,
#'   station_hasard_valide
#' )
#' table_biomasse$data
#' table_biomasse$flextable
#' }
#'
#' @importFrom dplyr bind_rows filter recode mutate select right_join summarise group_by
#' @importFrom tidyr expand_grid replace_na
#' @importFrom tibble tibble
#' @importFrom flextable flextable set_caption set_header_labels hline colformat_double
#' @importFrom MASS glm.nb
#' @importFrom officer fp_border
#'
#' @export
bpue_generate_biomasse <- function(specimen, station) {
  n_stations <- nrow(station)
  
  # ---- Fonction interne : ajustement NB2 securise ----
  safe_nb_fit <- function(y) {
   if (length(unique(y)) <= 1 || all(y == 0, na.rm = TRUE)) {
      return(list(bpue = 0, ic95 = "[0,00 – 0,00]"))
    }
    
    suppressWarnings({
      model <- try(glm.nb(biomasse_g ~ 1, data = data.frame(biomasse_g = y)), silent = TRUE)
    })
    
    if (inherits(model, "try-error")) {
      return(list(bpue = NA_real_, ic95 = NA_character_))
    }
    
    pred <- predict(model, se.fit = TRUE, type = "link", newdata = data.frame(biomasse_g = 0))
    fit_val <- as.numeric(pred$fit[1])
    se_val <- as.numeric(pred$se.fit[1])
    
    bpue <- exp(fit_val) / 1000
    
    lower_num <- exp(fit_val - 1.96 * se_val) / 1000
    upper_num <- exp(fit_val + 1.96 * se_val) / 1000
    
    ic95 <- paste0(
      "[",
      format_num_fr(lower_num, digits = 2),
      " – ",
      format_num_fr(upper_num, digits = 2),
      "]"
    )
    
    list(bpue = bpue, ic95 = ic95)
  }
  
  
  # ---- Groupe "Tous" ----
  biomasse_totale_par_station <- specimen |>
    group_by(.data$no_station) |>
    summarise(biomasse_g = sum(.data$masse, na.rm = TRUE), .groups = "drop") |>
    right_join(station |> select(.data$no_station), by = "no_station") |>
    mutate(biomasse_g = replace_na(.data$biomasse_g, 0))
  
  biomasse_totale_kg <- sum(biomasse_totale_par_station$biomasse_g) / 1000
  fit_tous <- safe_nb_fit(biomasse_totale_par_station$biomasse_g)
  
  ligne_tous <- tibble(
    groupe = "Tous",
    biomasse = biomasse_totale_kg,
    percent = 100,
    bpue = fit_tous$bpue,
    ic95 = fit_tous$ic95
  )
  
  # ---- Groupe par sexe ----
  biomasse_par_sexe <- specimen |>
    group_by(.data$no_station, .data$sexe) |>
    summarise(biomasse = sum(.data$masse, na.rm = TRUE), .groups = "drop") |>
    right_join(
      expand_grid(no_station = station$no_station, sexe = unique(specimen$sexe)),
      by = c("no_station", "sexe")
    ) |>
    mutate(biomasse = replace_na(.data$biomasse, 0)) |>
    group_by(.data$sexe) |>
    summarise(
      biomasse = sum(.data$biomasse) / 1000,
      bpue = .data$biomasse / n_stations,
      percent = .data$biomasse * 100 / biomasse_totale_kg,
      ic95 = NA_character_
    ) |>
    mutate(groupe = recode(.data$sexe,
                           "F" = "Femelle",
                           "M" = "Mâle",
                           "IND" = "Sexe inconnu")) |>
    select("groupe", "biomasse", "percent", "bpue", "ic95")
  
  # ---- Repro. actifs males ----
  data_males_matures <- specimen |>
    filter(.data$sexe == "M", .data$maturite == "O") |>
    group_by(.data$no_station) |>
    summarise(biomasse = sum(.data$masse), .groups = "drop") |>
    right_join(station |> select(.data$no_station), by = "no_station") |>
    mutate(biomasse = replace_na(.data$biomasse, 0))
  
  ligne_males_matures <- tibble(
    groupe = "Repro. actifs mâles",
    biomasse = sum(data_males_matures$biomasse) / 1000,
    percent = biomasse * 100 / biomasse_totale_kg,
    bpue = biomasse / n_stations,
    ic95 = NA_character_
  )
  
  # ---- Repro. actifs femelles ----
  data_femelles_matures <- specimen |>
    filter(.data$sexe == "F", .data$maturite == "O") |>
    group_by(.data$no_station) |>
    summarise(biomasse_g = sum(.data$masse), .groups = "drop") |>
    right_join(station |> select(.data$no_station), by = "no_station") |>
    mutate(biomasse_g = replace_na(.data$biomasse_g, 0))
  
  biomasse_femelles_matures <- sum(data_femelles_matures$biomasse_g)
  fit_femelles <- safe_nb_fit(data_femelles_matures$biomasse_g)
  
  ligne_femelles_matures <- tibble(
    groupe = "Repro. actifs femelles",
    biomasse = biomasse_femelles_matures / 1000,
    percent = biomasse * 100 / biomasse_totale_kg,
    bpue = fit_femelles$bpue,
    ic95 = fit_femelles$ic95
  )
  
  # ---- Immatures ----
  data_immatures <- specimen |>
    filter(.data$maturite == "N") |>
    group_by(.data$no_station) |>
    summarise(biomasse = sum(.data$masse), .groups = "drop") |>
    right_join(station |> select(.data$no_station), by = "no_station") |>
    mutate(biomasse = replace_na(.data$biomasse, 0))
  
  ligne_immatures <- tibble(
    groupe = "Imm. ou reprod. inactifs",
    biomasse = sum(data_immatures$biomasse) / 1000,
    percent = biomasse * 100 / biomasse_totale_kg,
    bpue = biomasse / n_stations,
    ic95 = NA_character_
  )
  
  # ---- Inconnu ----
  data_inconnu <- specimen |>
    filter(.data$maturite == "IND") |>
    group_by(.data$no_station) |>
    summarise(biomasse = sum(.data$masse), .groups = "drop") |>
    right_join(station |> select(.data$no_station), by = "no_station") |>
    mutate(biomasse = replace_na(.data$biomasse, 0))
  
  ligne_inconnu <- tibble(
    groupe = "Statut reprod. inconnu",
    biomasse = sum(data_inconnu$biomasse) / 1000,
    percent = biomasse * 100 / biomasse_totale_kg,
    bpue = biomasse / n_stations,
    ic95 = NA_character_
  )
  
  # ---- Table finale ----
  table_biomasse <- bind_rows(
    ligne_tous,
    biomasse_par_sexe,
    ligne_femelles_matures,
    ligne_males_matures,
    ligne_immatures,
    ligne_inconnu
  )
  
  table_flex <- table_biomasse |>
    flextable() |>
    set_caption("Tableau de biomasse") |>
    set_header_labels(
      groupe   = "Groupe",
      biomasse = "Biomasse totale (kg)",
      percent  = "Proportion (%)",
      bpue     = "BPUE (kg/station)",
      ic95     = "IC 95%"
    ) |>
    style_flextable_aquapop() |>
    colformat_double(j = c("biomasse", "bpue"), digits = 2, decimal.mark = ",", big.mark = " ", na_str = "-") |>
    colformat_double(j = "percent", digits = 1, decimal.mark = ",", big.mark = " ", na_str = "-") |>
    hline(i = 3, border = fp_border(color = "black", width = 0.5))
  
  return(list(
    data = table_biomasse,
    flextable = table_flex
  ))
}