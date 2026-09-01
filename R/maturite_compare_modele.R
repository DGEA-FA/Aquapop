#' Évaluer et comparer les modèles L50 (ou A50) de maturité
#'
#' Cette fonction ajuste, évalue et sélectionne les modèles de maturité sexuelle
#' selon l'approche séparée (par sexe) et combinée (sexes confondus), à partir
#' des données de spécimens. Elle retourne les tableaux d'évaluation au format brut
#' et flextable, un message d'interprétation, ainsi que les meilleurs modèles
#' disponibles pour les mâles, les femelles et l'approche combinée.
#'
#' En cas de jeu de données vide ou insuffisant, la fonction retourne un objet
#' structuré avec `success = FALSE`, sans générer d'erreur.
#'
#' @param specimen_data Un `data.frame` contenant les données brutes de spécimens,
#'   incluant les colonnes `maturite`, `sexe`, et la variable quantitative choisie
#'   (`ltm` ou `age`).
#' @param prefer_combined Logique indiquant si l'approche combinée doit être forcée
#'   pour le tableau principal (défaut : `FALSE`).
#' @param variable Variable quantitative utilisée dans les modèles : `"ltm"`
#'   (par défaut) ou `"age"`.
#'
#' @return Une liste contenant :
#' \describe{
#'   \item{success}{Indique si la comparaison des modèles a pu être effectuée}
#'   \item{table}{Liste avec `df` et `flextable` pour le tableau principal}
#'   \item{best_model}{Liste contenant `best_model_M`, `best_model_F` et
#'   `best_model_combined`}
#'   \item{message}{Texte interprétatif décrivant la sélection ou les cas d'échec}
#'   \item{table_sep}{Liste avec `df` et `flextable` pour les modèles séparés}
#'   \item{table_comb}{Liste avec `df` et `flextable` pour les modèles combinés}
#' }
#'
#' @export
#'
#' @importFrom dplyr mutate select if_else
#' @importFrom flextable flextable set_header_labels
#' @importFrom stringr str_extract
#' @importFrom tibble tibble

maturite_compare_modele <- function(specimen_data,
                                    variable = c("ltm", "age")) {
  variable <- match.arg(variable)
  
  validation_res <- maturite_validate_data(
  specimen_data = specimen_data,
  variable = variable
)

if (!isTRUE(validation_res$success)) {
  
  return(
    list(
      success = FALSE,
      message = validation_res$message,
      table_sep_M = NULL,
      table_sep_F = NULL,
      table_comb = NULL,
      table = NULL,
      best_model = NULL
    )
  )
}

df <- validation_res$data


#----------------------------------------------------
# MODÈLES SÉPARÉS
#----------------------------------------------------

models_sep <- maturite_fit_separated_modele(df,variable = variable)
eval_sep <- maturite_eval_modele(models_sep)

eval_sep <- eval_sep |>
  dplyr::mutate(
    sexe = dplyr::case_when(
      grepl("^M_", .data$modele_id) ~ "M",
      grepl("^F_", .data$modele_id) ~ "F",
      TRUE ~ NA_character_
    )
  )

# Delta AICc PAR SEXE

eval_sep <- eval_sep |>
  group_by(.data$sexe) |>
  
  mutate(
    min_aicc = if (any(is.finite(.data$aicc) & .data$convergence))
      {
      
      min(.data$aicc[is.finite(.data$aicc) & .data$convergence],na.rm = TRUE)
      
    } else {
      NA_real_
    },
    
    delta_aicc = ifelse(is.finite(.data$aicc), .data$aicc - min_aicc, NA_real_)
    
  ) |>
  ungroup() |>
  select(-min_aicc)


# Recommandation par sexe séparés
best_sep <- maturite_select_best_separated_modele(eval_sep)

best_M <- best_sep$best_model_M
best_F <- best_sep$best_model_F

eval_sep <- eval_sep |>
  dplyr::mutate(
    recommande = .data$modele_id %in% c(best_M, best_F),
    
    commentaire = dplyr::case_when(
      
      is.na(.data$type) ~
        "Données insuffisantes",
      
      .data$convergence == FALSE ~
        "Ce modèle ne converge pas.",
      
      .data$ajust == FALSE ~
        "Ce modèle ne s'ajuste pas bien aux données.",
      
      .data$modele_id %in% c(best_M, best_F) ~
        "Ce modèle est recommandé car son AICc est le plus faible.",
      
      is.finite(.data$delta_aicc) &
        .data$delta_aicc > 0 &
        .data$delta_aicc < 2 ~
        "Modèle alternatif ayant un support statistique similaire au modèle recommandé.",
      
      TRUE ~ "Modèle valide."
    )
  )


#----------------------------------------------------
# MODÈLES COMBINÉS
#----------------------------------------------------

models_comb <- maturite_fit_combined_modele(df,variable = variable)
eval_comb <- maturite_eval_modele(models_comb)


# Delta AICc pour les modèles combinés
aicc_valides <- eval_comb |>
  filter(
    .data$convergence,
    is.finite(.data$aicc)
  )

if (nrow(aicc_valides) > 0) {
  
  min_comb_aicc <- min(
    aicc_valides$aicc,
    na.rm = TRUE
  )
  
  eval_comb <- eval_comb |>
    mutate(
      delta_aicc = ifelse(
        is.finite(.data$aicc),
        .data$aicc - min_comb_aicc,
        NA_real_
      )
    )
  
} else {
  
  eval_comb$delta_aicc <- NA_real_
}


# Meilleur modèle combiné

best_comb <- maturite_select_best_combined_modele(eval_comb)

best_comb_ids <- best_comb$best_model

eval_comb <- eval_comb |>
  mutate(
    recommande = .data$modele_id %in% best_comb_ids,
    
    commentaire = case_when(
      
      is.na(.data$type) ~
        "Données insuffisantes",
      
      .data$convergence == FALSE ~
        "Ce modèle ne converge pas.",
      
      .data$ajust == FALSE ~
        "Ce modèle ne s'ajuste pas bien aux données.",
      
      .data$modele_id %in% best_comb_ids ~
        "Ce modèle est recommandé car son AICc est le plus faible.",
      
      is.finite(.data$delta_aicc) &
        .data$delta_aicc > 0 &
        .data$delta_aicc < 2 ~
        "Modèle alternatif ayant un support statistique similaire au modèle recommandé.",
      
      TRUE ~ "Modèle valide."
    )
  )


# ===========================================================================
# TABLEAU SÉPARÉ
# ===========================================================================

make_sep_table <- function(data_sex) {

  # Formatage des valeurs
  
  digits_ic <- if (variable == "age") 1 else 0
  
  tab <- data_sex |>
    dplyr::mutate(
      
      point50 = if (variable == "age") {
        format(round(.data$point50, 1), decimal.mark = ",", nsmall = 1)
        
        } else {
          
          format(round(.data$point50, 0), decimal.mark = ",", nsmall = 0)
          },
      
      IC95_inf = .data$point50_IC95_inf,
      IC95_sup = .data$point50_IC95_sup,
      
      b0 = ifelse(is.na(.data$b0), "-",
                  formatC(.data$b0, format = "f", digits = 3, decimal.mark = ",")),
      
      b1 = ifelse(is.na(.data$b1),  "-",
                  formatC(.data$b1, format = "f", digits = 3, decimal.mark = ",")),
      
      AICc = ifelse(is.na(.data$aicc), "-",
                    formatC(.data$aicc, format = "f", digits = 2, decimal.mark = ",")),
      
      delta_AICc = ifelse(is.na(.data$delta_aicc), "-",
                          formatC(.data$delta_aicc, format = "f", digits = 2, decimal.mark = ",")),
      
      IC95 = ifelse(is.na(IC95_inf) | is.na(IC95_sup), "-",
                    paste0("[", format(round(IC95_inf, digits_ic), decimal.mark = "," ),
                           " - ", format(round(IC95_sup, digits_ic), decimal.mark = ","),
                           "]")
                    ),
      
      across(everything(), ~ ifelse(is.na(.x), "-", .x)
             )
      )
    
    # Correction du label point50 selon la variable
    point50_label <- if (variable == "ltm") {
      "L50"
    } else {
      "A50"
    }
    
    names(tab)[names(tab) == "point50"] <- point50_label
  

# Sélection des colonnes et modification des titres pour affichage 
    tab <- tab |>
      select(-IC95_inf, -IC95_sup) |>
      select(
        modele_id,
        all_of(point50_label),
        IC95,
        b0,
        b1,
        AICc,
        delta_AICc,
        convergence,
        ajust,
        commentaire
      )
    
    names(tab) <- c(
      "modele_id",
      point50_label,
      "IC 95 %",
      "b0",
      "b1",
      "AICc",
      "Δ AICc",
      "Convergence",
      "Ajustement",
      "Commentaire"
    )
    
  
  ft <- flextable::flextable(tab) |>
      style_flextable_aquapop()
  
  list(
    df = tab,
    flextable = ft
  )
}

table_sep_M <- make_sep_table(
  eval_sep |>
    dplyr::filter(.data$sexe == "M")
)

table_sep_F <- make_sep_table(
  eval_sep |>
    dplyr::filter(.data$sexe == "F")
)



# ===========================================================================
# TABLEAU SEXES COMBINÉS
# ===========================================================================

# Formatage du point50 selon variable et selon type de modèle

eval_comb <- eval_comb |>
  mutate(type_modele = sub("_.*$", "", modele_id))

digits_point50 <- if (variable == "age") 1 else 0

eval_comb <- eval_comb |>
  mutate(point50_F = ifelse(type_modele == "TLO", point50, point50_F),
         point50_M = ifelse(type_modele == "TLO", point50, point50_M))

eval_comb <- eval_comb |>
  mutate(point50_M = case_when(
    
    type_modele == "TLO" & is.finite(.data$point50_M) ~
      
      paste0(format(round(.data$point50_M, digits_point50), decimal.mark = ",", nsmall = digits_point50),
             " [",
             format(round(.data$point50_IC95_inf, digits_point50),decimal.mark = ",", nsmall = digits_point50),
             " - ",
             format(round(.data$point50_IC95_sup, digits_point50), decimal.mark = ",", nsmall = digits_point50),
             "]"),
    
    type_modele %in% c("ADD", "INT", "COM") ~
      
      format(round(.data$point50_M, digits_point50),decimal.mark = ",",nsmall = digits_point50),
    
    TRUE ~ "-"
  ),
  
  point50_F = case_when(
    
    type_modele == "TLO" & is.finite(.data$point50_F) ~
      
      paste0(format(round(.data$point50_F, digits_point50), decimal.mark = ",", nsmall = digits_point50),
             " [",
             format(round(.data$point50_IC95_inf, digits_point50),decimal.mark = ",",nsmall = digits_point50),
             " - ",
             format(round(.data$point50_IC95_sup, digits_point50), decimal.mark = ",", nsmall = digits_point50),
             "]"),
    
    type_modele %in% c("ADD", "INT", "COM") ~
      
      format(round(.data$point50_F, digits_point50), decimal.mark = ",", nsmall = digits_point50),
    
    TRUE ~ "-"
    
  )
  )


# Formatage des valeurs

tab_comb <- eval_comb |>
  mutate(
    
    b0 = ifelse(is.na(.data$b0), "-",
                formatC(.data$b0,format = "f", digits = 3, decimal.mark = ",")
                ),
    
    b1 = ifelse(is.na(.data$b1),  "-",
                formatC(.data$b1, format = "f", digits = 3, decimal.mark = ",")
                ),
    
    b2 = ifelse(is.na(.data$b2), "-",
                formatC(.data$b2, format = "f", digits = 3, decimal.mark = ",")
                ),
    
    b3 = ifelse(is.na(.data$b3), "-",
                formatC(.data$b3, format = "f", digits = 3, decimal.mark = ",")
                ),
    
    AICc = ifelse(is.na(.data$aicc), "-",
                  formatC(.data$aicc, format = "f", digits = 2, decimal.mark = ",")
                  ),
    
    delta_AICc = ifelse(is.na(.data$delta_aicc), "-",
                        formatC(.data$delta_aicc, format = "f", digits = 2, decimal.mark = ",")
                        ),
    
    across(everything(), ~ ifelse(is.na(.x), "-", .x))
    )


# Sélection des colonnes et modification des titres pour affichage 

tab_comb <- tab_comb |>
  dplyr::select(
    modele_id,
    modele,
    lien,
    point50_F,
    point50_M,
    b0,
    b1,
    b2,
    b3,
    AICc,
    delta_AICc,
    convergence,
    ajust,
    commentaire
  )

point50_label <- if (variable == "ltm") {
  "L50"
} else {
  "A50"
}

names(tab_comb) <- c(
  "modele_id",
  "Type",
  "Lien",
  paste0(point50_label, "_F"),
  paste0(point50_label, "_M"),
  "b0",
  "b1",
  "Coeff_sexe",
  "Coeff_interaction",
  "AICc",
  "Δ AICc",
  "Convergence",
  "Ajustement",
  "Commentaire"
  )
  
ft_comb <- flextable::flextable(tab_comb) |>
  style_flextable_aquapop()


table_comb <- list(
  df = tab_comb,
  flextable = ft_comb
)


# ===========================================================================
# MEILLEURS MODÈLES
# ===========================================================================

best_model <- list(
  
  best_model_M = if (!is.null(best_M) && length(best_M) > 0)
    {
    
    list(modele = sub( "_.*$", "", best_M),
      lien = sub("^.*_", "", best_M),
      variable = variable)
    
    } else {
      NULL
      },
  
  best_model_F = if (!is.null(best_F) && length(best_F) > 0)
    {
    list(
      modele = sub("_.*$", "", best_F),
      lien = sub("^.*_", "", best_F),
      variable = variable)
    
    } else {
      NULL
      },
  
  best_model_combined = if (!is.null(best_comb_ids) && length(best_comb_ids) > 0)
    {
    list(modele = sub("_.*$", "", best_comb_ids),
      lien = sub("^.*_",  "", best_comb_ids),
      variable = variable)
    
    } else {
      NULL
      }
  )


# ===========================================================================
# MESSAGE
# ===========================================================================

message <- paste0(
  best_sep$message,
  "\n",
  best_comb$message
)


# ===========================================================================
# RETOUR
# ===========================================================================

list(
  
  success = TRUE,
  message = message,
  table_sep_M = table_sep_M,
  table_sep_F = table_sep_F,
  table_comb = table_comb,
  best_model = best_model,
  eval_sep = eval_sep,
  eval_comb = eval_comb,
  models_sep = models_sep,
  models_comb = models_comb
)
}