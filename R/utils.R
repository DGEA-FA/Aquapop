myspinner <- 6

#' Constantes associées aux espèces suivies par PEN
#'
#' Contient le nom commun, le binwidth recommandé pour les histogrammes,
#' les seuils de classes PSD (`breaks`) et leurs libellés (`break_labels`) par espèce (`sp`).
#'
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

#' Noms standardisés des classes PSD
#'
#' Utilisés dans les fonctions PSD (psd_q, psd_byclass, psd_plot)
#'
#' @export
psd_classnames <- c("Sous-stock", "Stock", "Qualité", "Préférée", "Mémorable", "Trophée")

# Couleur par défaut pour les graphiques (ex. lorsque groupement = "tous")
couleur_default <- "#084594"

# Constantes de libellés pour les groupements
group_labels <- list(
  "sexe"     = c("F" = "Femelle", "M" = "Mâle", "IND" = "Indéterminé"),
  "maturite" = c("O" = "Mature", "N" = "Immature", "IND" = "Indéterminé"),
  "marquage" = c("MA" = "Marqué", "NMA" = "Non marqué"),
  "tous"     = c("TOUS" = "Tous")  # Catégorie unique
)

# Constantes de couleurs associées aux groupements
group_colors <- list(
  "sexe"     = c("F" = couleur_default, "M" = "#99CCFF", "IND" = "#4d4d4d"),
  "maturite" = c("O" = couleur_default, "N" = "#99CCFF", "IND" = "#4d4d4d"),
  "marquage" = c("MA" = couleur_default, "NMA" = "#99CCFF"),
  "tous"     = c("TOUS" = couleur_default)
)


#' Constantes pour le calcul de l'indice de condition (Wr)
#'
#' Source : FSA::wsVal() pour Lake Trout, Brook Trout, Walleye
#'
#' @format Un `tibble` avec les colonnes :
#' - `sp` : Code d’espèce (SANA, SAFO, SAVI)
#' - `species` : Nom anglais de l'espèce
#' - `min_TL` : Longueur minimale (mm)
#' - `int` : Intercept de la régression log-log
#' - `slope` : Pente de la régression log-log
#' - `source` : Référence source
#' @export
wr_constants <- tibble::tibble(
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

get_wr_constants <- function(sp) {
  wr_constants |>
    dplyr::filter(sp == sp) |>
    dplyr::slice(1)
}


# Fonctions internes pour les modèles de croissance
vb_function <- function(age, linf, k, t0) linf * (1 - exp(-k * (age - t0)))
gompertz_function <- function(age, linf, k, t0) linf * exp(-exp(-k * (age - t0)))
logistic_function <- function(age, linf, k, t0) linf / (1 + exp(-k * (age - t0)))


# 
# # Copy report to temporary directory. This is mostly important when
# # deploying the app, since often the working directory won't be writable
# report_path <- tempfile(fileext = ".Rmd")
# file.copy("report.Rmd", report_path, overwrite = TRUE)

labelled_data <- function(data) {
  # Obtenir les labels des colonnes
  labels <- labelled::var_label(data)
  
  # Remplacer les noms des colonnes par leurs labels
  colnames(data) <- unlist(labels)
  
  return(data)
}

#' Exécute une expression glm() en filtrant le warning "probabilités ajustées à 0 ou 1"
#'
#' @param expr Une expression glm() passée sans guillemets
#'
#' @return Le résultat de glm()
sans_warning_proba <- function(expr) {
  withCallingHandlers(
    expr = force(expr),
    warning = function(w) {
      if (grepl("probabilités ont été ajustées numériquement à 0 ou 1", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

coef_par_nom <- function(modele_glm, sexe = c("sexeF", "sexeM"), interaction = FALSE) {
  sexe <- match.arg(sexe)
  pattern <- if (interaction) paste0(":", sexe) else sexe
  coef_nom <- names(coef(modele_glm))
  nom_cible <- coef_nom[grepl(pattern, coef_nom)]
  
  if (length(nom_cible) == 0) {
    stop(glue::glue("❌ Aucun coefficient ne correspond au motif '{pattern}' dans le modèle."))
  }
  
  coef(modele_glm)[[nom_cible]]
}

#' Calculer le ratio mâles:femelles sous forme simplifiée
#'
#' Cette fonction prend en entrée un nombre de mâles et de femelles, puis retourne un
#' ratio \eqn{M:F} (mâles pour femelles) sous forme réduite à ses plus simples expressions,
#' comme `"3:2"` ou `"1:1"`. Si les deux valeurs sont nulles, la fonction retourne `NA`.
#'
#' @param male_count Nombre d’individus de sexe masculin (entier)
#' @param female_count Nombre d’individus de sexe féminin (entier)
#'
#' @return Une chaîne de caractères représentant le ratio simplifié (ex: `"3:2"`), ou `NA_character_` si les deux valeurs sont nulles.
#'
#' @examples
#' calculate_mf_ratio(6, 4)   # Retourne "3:2"
#' calculate_mf_ratio(5, 5)   # Retourne "1:1"
#' calculate_mf_ratio(0, 0)   # Retourne NA
#' calculate_mf_ratio(0, 7)   # Retourne "0:1"
#'
#' @export
calculate_mf_ratio <- function(male_count, female_count) {
  if (male_count == 0 && female_count == 0) {
    return(NA_character_)  # Retourne NA explicite de type character
  }
  # Simplifier le ratio avec fractions()
  ratio <- MASS::fractions(c(male_count, female_count))
  return(paste0(ratio[1], ":", ratio[2]))
}



#' Gérer les erreurs silencieusement
#'
#' Fonction utilitaire pour extraire le message d’erreur dans un `tryCatch`.
#'
#' @param e Objet d’erreur (`condition`) attrapé par `tryCatch`.
#' @return Message de l’erreur (chaîne de caractères).
#' @export
handle_error <- function(e) {
  conditionMessage(e)
}
