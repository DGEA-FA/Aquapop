#' Ajuster la relation masse-longueur pour une espèce
#'
#' Cette fonction ajuste une régression linéaire sur les données log-transformées
#' de masse et de longueur. Elle retourne les coefficients estimés (avec IC 95 %),
#' un graphique de la courbe ajustée, et une version formatée du tableau.
#'
#' Si aucune donnée exploitable n'est disponible, la fonction retourne un objet
#' structuré avec `success = FALSE`, sans générer d'erreur.
#'
#' @param data Un `data.frame` contenant les colonnes `ltm`, `masse`, `sp` et
#'   `no_specimen`.
#'
#' @return Une liste nommée contenant :
#' \describe{
#'   \item{success}{Indique si l'analyse a pu être produite}
#'   \item{data}{Tableau des coefficients estimés (`data.frame`)}
#'   \item{flextable}{Tableau formaté (`flextable`)}
#'   \item{plot}{Graphique ggplot de la relation masse-longueur}
#'   \item{message}{Message explicatif si l'analyse n'est pas disponible}
#' }
#'
#' @importFrom checkmate assert_data_frame assert_subset
#' @importFrom dplyr filter mutate recode
#' @importFrom FSA logbtcf
#' @importFrom flextable flextable set_header_labels colformat_double
#' @importFrom ggplot2 aes geom_line geom_point ggplot labs
#' @importFrom stats confint lm predict
#' @importFrom tibble tibble
#'
#' @examples
#' data_exemple <- data.frame(
#'   no_specimen = 1:5,
#'   sp = rep("SANA", 5),
#'   ltm = c(120, 140, 160, 180, 200),
#'   masse = c(20, 30, 45, 60, 80)
#' )
#'
#' res <- masse_longueur_fit(data_exemple)
#' res$data
#' if (requireNamespace("flextable", quietly = TRUE)) res$flextable
#'
#' @export
masse_longueur_fit <- function(data) {
  
  # Validation des données ----
  assert_data_frame(data)
  
  colonnes_requises <- c("ltm", "masse", "sp", "no_specimen")
  assert_subset(colonnes_requises, colnames(data))
  
  # Cas sans ligne ----
  if (nrow(data) == 0) {
    return(list(
      success = FALSE,
      data = NULL,
      flextable = NULL,
      plot = NULL,
      message = "Aucun spécimen valide disponible pour produire la relation masse-longueur."
    ))
  }
  
  # Validation espèce unique ----
  espece <- unique(stats::na.omit(data$sp))
  
  if (length(espece) != 1) {
    stop("Les données doivent contenir une seule espèce (sp).")
  }
  
  # Prétraitement ----
  donnees_filtrees <- data |>
    filter(!is.na(.data$ltm), !is.na(.data$masse)) |>
    mutate(
      log_masse = log10(.data$masse),
      log_longueur = log10(.data$ltm)
    )
  
  # Cas sans donnée exploitable ----
  if (nrow(donnees_filtrees) == 0) {
    return(list(
      success = FALSE,
      data = NULL,
      flextable = NULL,
      plot = NULL,
      message = paste(
        "Aucune donnée exploitable n'est disponible pour ajuster la relation",
        "masse-longueur. Les variables ltm et masse sont absentes ou manquantes."
      )
    ))
  }
  
  # Cas insuffisant pour ajuster un modèle ----
  if (nrow(donnees_filtrees) < 2) {
    return(list(
      success = FALSE,
      data = NULL,
      flextable = NULL,
      plot = NULL,
      message = paste(
        "La relation masse-longueur requiert au moins deux spécimens avec une",
        "longueur et une masse valides."
      )
    ))
  }
  
  if (length(unique(donnees_filtrees$ltm)) < 2) {
    return(list(
      success = FALSE,
      data = NULL,
      flextable = NULL,
      plot = NULL,
      message = paste(
        "La relation masse-longueur ne peut pas être ajustée, car toutes les",
        "longueurs valides sont identiques."
      )
    ))
  }
  
  # Ajustement du modèle ----
  modele_masse_longueur <- lm(log_masse ~ log_longueur, data = donnees_filtrees)
  resume_modele <- summary(modele_masse_longueur)$coefficients
  intervalle_confiance <- confint(modele_masse_longueur)
  
  # Tableau brut ----
  table_resultats <- tibble(
    coefficient = c("log10_a", "b"),
    estimation = resume_modele[, 1],
    erreur_standard = resume_modele[, 2],
    ic95 = paste0(
      "[",
      format_num_fr(intervalle_confiance[, 1], digits = 3),
      " – ",
      format_num_fr(intervalle_confiance[, 2], digits = 3),
      "]"
    )
  )
  
  # Tableau formaté ----
  table_flextable <- table_resultats |> 
    mutate(
      coefficient = dplyr::recode(
        .data$coefficient,
        "log10_a" = "log\u2081\u2080(a)",
        "b" = "b"
      )) |>
    flextable() |>
    set_header_labels(
      coefficient = "Coefficient",
      estimation = "Estimation",
      erreur_standard = "Erreur standard",
      ic95 = "IC 95%"
    ) |>
    style_flextable_aquapop() |>
    colformat_double(
      j = c("estimation", "erreur_standard"),
      digits = 3,
      decimal.mark = ",",
      big.mark = " ",
      na_str = "-"
    )
  
  # Données de prédiction ----
  
  
  
  
  sequence_log_longueur <- seq(
    min(donnees_filtrees$log_longueur),
    max(donnees_filtrees$log_longueur),
    length.out = 100
  )
  
  facteur_correction <- logbtcf(modele_masse_longueur, base = 10)
  
  predictions <- facteur_correction * 10 ^ predict(
    modele_masse_longueur,
    newdata = data.frame(log_longueur = sequence_log_longueur),
    interval = "prediction"
  )
  
  donnees_prediction <- tibble(
    ltm = 10 ^ sequence_log_longueur,
    fit = predictions[, "fit"],
    lwr = predictions[, "lwr"],
    upr = predictions[, "upr"]
  )
  
  # Graphique ----
  log10_a <- table_resultats$estimation[table_resultats$coefficient == "log10_a"]
  b <- table_resultats$estimation[table_resultats$coefficient == "b"]
  
  equation_label <- paste0(
    "Équation : W = 10^(",
    format_num_fr(log10_a, digits = 3),
    " + ",
    format_num_fr(b, digits = 3),
    " × log10(L))"
  )
  
  
  graphique_relation <- ggplot() +
    geom_point(data = donnees_filtrees, aes(x = .data$ltm, y = .data$masse)) +
    geom_line(data = donnees_prediction, aes(x = .data$ltm, y = .data$fit), color = "blue") +
    geom_line(data = donnees_prediction, aes(x = .data$ltm, y = .data$lwr), color = "red", linetype = 2) +
    geom_line(data = donnees_prediction, aes(x = .data$ltm, y = .data$upr), color = "red", linetype = 2) +
    theme_aquapop() +
    labs(
      x = "Longueur totale maximale (mm)",
      y = "Masse (g)",
      caption = equation_label
    )
  
  # Retour ----
  return(list(
    success = TRUE,
    data = table_resultats,
    flextable = table_flextable,
    plot = graphique_relation,
    message = NULL
  ))
}