#' Créer une table de biomasse et BPUE par groupe biologique
#'
#' Cette fonction calcule la biomasse totale (kg), la proportion (%) et la biomasse
#' par unité d'effort (BPUE, en kg/station) pour différents groupes biologiques d'une espèce cible.
#'
#' @param data_specimen Un `data.frame` de spécimens filtrés (issu de `load_specimen()`).
#' @param data_station Un `data.frame` des stations valides (issu de `load_station()`).
#'
#' @return Une liste contenant :
#' \describe{
#'   \item{`data`}{Un `data.frame` résumant la biomasse totale, la proportion, la BPUE et les intervalles de confiance pour chaque groupe biologique.}
#'   \item{`flextable`}{Une version formatée du tableau pour l’exportation (Word, Shiny, etc.) à l’aide du package `flextable`.}
#' }
#' @export
bpue_generate_biomasse <- function(data_specimen, data_station) {
  n_stations <- nrow(data_station)
  
  # ---- Fonction interne : ajustement NB2 sécurisé ----
  safe_nb_fit <- function(y) {
    if (length(unique(y)) <= 1 || all(y == 0, na.rm = TRUE)) {
      return(list(bpue = 0, ic95 = "(0.0-0.0)"))
    }
    
    suppressWarnings({
      model <- try(MASS::glm.nb(biomasse_g ~ 1, data = data.frame(biomasse_g = y)), silent = TRUE)
    })
    
    if (inherits(model, "try-error")) {
      return(list(bpue = NA_real_, ic95 = "(NA-NA)"))
    }
    
    pred <- predict(model, se.fit = TRUE, type = "link", newdata = data.frame(biomasse_g = 0))
    fit_val <- as.numeric(pred$fit[1])
    se_val <- as.numeric(pred$se.fit[1])
    bpue <- exp(fit_val) / 1000
    ic <- exp(fit_val + c(-1.96, 1.96) * se_val) / 1000
    
    list(bpue = bpue, ic95 = sprintf("(%.1f-%.1f)", ic[1], ic[2]))
  }
  
  
  # ---- Groupe "Tous" ----
  biomasse_totale_par_station <- data_specimen |>
    dplyr::group_by(no_station) |>
    dplyr::summarise(biomasse_g = sum(masse, na.rm = TRUE), .groups = "drop") |>
    dplyr::right_join(data_station |> dplyr::select(no_station), by = "no_station") |>
    dplyr::mutate(biomasse_g = tidyr::replace_na(biomasse_g, 0))
  
  biomasse_totale_kg <- sum(biomasse_totale_par_station$biomasse_g) / 1000
  fit_tous <- safe_nb_fit(biomasse_totale_par_station$biomasse_g)
  
  ligne_tous <- tibble::tibble(
    groupe = "Tous",
    biomasse = biomasse_totale_kg,
    percent = 100,
    bpue = fit_tous$bpue,
    ic95 = fit_tous$ic95
  )
  
  # ---- Groupe par sexe ----
  biomasse_par_sexe <- data_specimen |>
    dplyr::group_by(no_station, sexe) |>
    dplyr::summarise(biomasse = sum(masse, na.rm = TRUE), .groups = "drop") |>
    dplyr::right_join(
      tidyr::expand_grid(no_station = data_station$no_station, sexe = unique(data_specimen$sexe)),
      by = c("no_station", "sexe")
    ) |>
    dplyr::mutate(biomasse = tidyr::replace_na(biomasse, 0)) |>
    dplyr::group_by(sexe) |>
    dplyr::summarise(
      biomasse = sum(biomasse) / 1000,
      bpue = biomasse / n_stations,
      percent = biomasse * 100 / biomasse_totale_kg,
      ic95 = ""
    ) |>
    dplyr::mutate(groupe = dplyr::recode(sexe,
                                         "F" = "Femelle",
                                         "M" = "Mâle",
                                         "IND" = "Sexe inconnu")) |>
    dplyr::select(groupe, biomasse, percent, bpue, ic95)
  
  # ---- Repro. actifs mâles ----
  data_males_matures <- data_specimen |>
    dplyr::filter(sexe == "M", maturite == "O") |>
    dplyr::group_by(no_station) |>
    dplyr::summarise(biomasse = sum(masse), .groups = "drop") |>
    dplyr::right_join(data_station |> dplyr::select(no_station), by = "no_station") |>
    dplyr::mutate(biomasse = tidyr::replace_na(biomasse, 0))
  
  ligne_males_matures <- tibble::tibble(
    groupe = "Repro. actifs mâles",
    biomasse = sum(data_males_matures$biomasse) / 1000,
    percent = biomasse * 100 / biomasse_totale_kg,
    bpue = biomasse / n_stations,
    ic95 = ""
  )
  
  # ---- Repro. actifs femelles ----
  data_femelles_matures <- data_specimen |>
    dplyr::filter(sexe == "F", maturite == "O") |>
    dplyr::group_by(no_station) |>
    dplyr::summarise(biomasse_g = sum(masse), .groups = "drop") |>
    dplyr::right_join(data_station |> dplyr::select(no_station), by = "no_station") |>
    dplyr::mutate(biomasse_g = tidyr::replace_na(biomasse_g, 0))
  
  biomasse_femelles_matures <- sum(data_femelles_matures$biomasse_g)
  fit_femelles <- safe_nb_fit(data_femelles_matures$biomasse_g)
  
  ligne_femelles_matures <- tibble::tibble(
    groupe = "Repro. actifs femelles",
    biomasse = biomasse_femelles_matures / 1000,
    percent = biomasse * 100 / biomasse_totale_kg,
    bpue = fit_femelles$bpue,
    ic95 = fit_femelles$ic95
  )
  
  # ---- Immatures ----
  data_immatures <- data_specimen |>
    dplyr::filter(maturite == "N") |>
    dplyr::group_by(no_station) |>
    dplyr::summarise(biomasse = sum(masse), .groups = "drop") |>
    dplyr::right_join(data_station |> dplyr::select(no_station), by = "no_station") |>
    dplyr::mutate(biomasse = tidyr::replace_na(biomasse, 0))
  
  ligne_immatures <- tibble::tibble(
    groupe = "Imm. ou reprod. inactifs",
    biomasse = sum(data_immatures$biomasse) / 1000,
    percent = biomasse * 100 / biomasse_totale_kg,
    bpue = biomasse / n_stations,
    ic95 = ""
  )
  
  # ---- Inconnu ----
  data_inconnu <- data_specimen |>
    dplyr::filter(maturite == "IND") |>
    dplyr::group_by(no_station) |>
    dplyr::summarise(biomasse = sum(masse), .groups = "drop") |>
    dplyr::right_join(data_station |> dplyr::select(no_station), by = "no_station") |>
    dplyr::mutate(biomasse = tidyr::replace_na(biomasse, 0))
  
  ligne_inconnu <- tibble::tibble(
    groupe = "Statut reprod. inconnu",
    biomasse = sum(data_inconnu$biomasse) / 1000,
    percent = biomasse * 100 / biomasse_totale_kg,
    bpue = biomasse / n_stations,
    ic95 = ""
  )
  
  # ---- Table finale ----
  table_biomasse <- dplyr::bind_rows(
    ligne_tous,
    biomasse_par_sexe,
    ligne_femelles_matures,
    ligne_males_matures,
    ligne_immatures,
    ligne_inconnu
  ) |>
    dplyr::mutate(
      biomasse = round(biomasse, 1),
      percent  = round(percent, 0),
      bpue     = round(bpue, 1)
    )
  
  table_flex <- flextable::flextable(table_biomasse) |>
    flextable::set_caption("Tableau de biomasse") |>
    flextable::set_header_labels(
      groupe   = "Groupe",
      biomasse = "Biomasse totale (kg)",
      percent  = "Proportion (%)",
      bpue     = "BPUE (kg/station)",
      ic95     = "IC 95%"
    ) |>
    flextable::hline(i = 3, border = officer::fp_border(color = "black", width = 0.5)) |>
    style_flextable_aquapop()
  
  return(list(
    data = table_biomasse,
    flextable = table_flex
  ))
}
