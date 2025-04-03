#' Créer une table de biomasse et BPUE par groupe biologique
#'
#' Cette fonction calcule la biomasse totale (kg), la proportion (%) et la biomasse
#' par unité d'effort (BPUE, en kg/station) pour différents groupes biologiques d'une espèce cible.
#'
#' @param data_specimen Un `data.frame` de spécimens filtrés (issu de `load_specimen()`).
#' @param data_station Un `data.frame` des stations valides (issu de `load_station()`).
#' @param format Format de sortie : `"data.frame"` (défaut) ou `"flextable"`.
#'
#' @return Un tableau résumant la biomasse totale, la proportion, la BPUE et les IC pour chaque groupe biologique.
#' @export
biomasse_table <- function(data_specimen, data_station,
                           format = c("data.frame", "flextable")) {
  format <- match.arg(format)
  n_stations <- nrow(data_station)
  
  # ---- Groupe "Tous" ----
  biomasse_totale_par_station <- data_specimen |>
    dplyr::group_by(no_station) |>
    dplyr::summarise(biomasse_g = sum(masse, na.rm = TRUE), .groups = "drop") |>
    dplyr::right_join(data_station |> dplyr::select(no_station), by = "no_station") |>
    dplyr::mutate(biomasse_g = tidyr::replace_na(biomasse_g, 0))
  
  biomasse_totale_kg <- sum(biomasse_totale_par_station$biomasse_g) / 1000
  
  modele_nb2_tous <- MASS::glm.nb(biomasse_g ~ 1, data = biomasse_totale_par_station)
  prediction_tous <- predict(modele_nb2_tous, newdata = data.frame(moyenne = "moyenne"),
                             se.fit = TRUE, type = "link")
  
  bpue_tous <- exp(prediction_tous$fit) / 1000
  ic_tous <- exp(prediction_tous$fit + c(-1.96, 1.96) * prediction_tous$se.fit) / 1000
  
  ligne_tous <- tibble::tibble(
    groupe = "Tous",
    biomasse = biomasse_totale_kg,
    percent = biomasse_totale_kg * 100 / biomasse_totale_kg,
    bpue = bpue_tous,
    ic95 = sprintf("(%.1f-%.1f)", ic_tous[1], ic_tous[2])
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
  modele_nb2_femelles <- MASS::glm.nb(biomasse_g ~ 1, data = data_femelles_matures)
  prediction_femelles <- predict(modele_nb2_femelles, newdata = data.frame(moyenne = "moyenne"),
                                 se.fit = TRUE, type = "link")
  
  bpue_femelles <- exp(prediction_femelles$fit) / 1000
  ic_femelles <- exp(prediction_femelles$fit + c(-1.96, 1.96) * prediction_femelles$se.fit) / 1000
  
  ligne_femelles_matures <- tibble::tibble(
    groupe = "Repro. actifs femelles",
    biomasse = biomasse_femelles_matures / 1000,
    percent = biomasse * 100 / biomasse_totale_kg,
    bpue = bpue_femelles,
    ic95 = sprintf("(%.1f-%.1f)", ic_femelles[1], ic_femelles[2])
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
  
  if (format == "data.frame") {
    return(table_biomasse)
  } else {
    return(
      flextable::flextable(table_biomasse) |>
        flextable::set_caption("Tableau de biomasse") |>
        flextable::set_header_labels(
          groupe   = "Groupe",
          biomasse = "Biomasse totale (kg)",
          percent  = "Proportion (%)",
          bpue     = "BPUE (kg/station)",
          ic95     = "IC 95%"
        ) |>
        flextable::fontsize(size = 12, part = "all") |>
        flextable::font(fontname = "Arial", part = "all") |>
        flextable::align(align = "center", part = "all") |>
        flextable::autofit() |>
        flextable::hline(i = 3, border = officer::fp_border(color = "black", width = 0.5))
    )
  }
}
