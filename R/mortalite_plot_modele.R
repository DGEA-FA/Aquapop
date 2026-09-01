#' Tracer la structure d'âge observée et la courbe du modèle de mortalité sélectionné
#'
#' Cette fonction affiche l'histogramme des âges issus du tableau `specimen` et
#' y superpose la courbe prédite à partir d'un modèle de mortalité ajusté
#' (`modele`). Le style graphique est cohérent avec les autres figures de
#' structure d'âge.
#'
#' La fonction retourne `NULL` si les données ou le modèle ne permettent pas de
#' produire un graphique interprétable.
#'
#' @param specimen Un `data.frame` contenant au moins les colonnes `sp` et `age`.
#' @param modele Un objet de modèle ajusté (`glm`, `glm.nb`, `glmmTMB`, etc.)
#'   pour prédire la fréquence selon l'âge.
#' @param info_modele Un `data.frame` issu de `mortalite_compare_modele()$data`
#'   contenant les estimations de `A` et `ic95`.
#'
#' @return Un objet `ggplot2`, ou `NULL` si le graphique ne peut pas être produit.
#'
#' @importFrom dplyr case_when filter mutate
#' @importFrom ggplot2 aes geom_histogram geom_line ggplot labs scale_x_continuous scale_y_continuous
#' @importFrom glue glue
#' @importFrom stats predict
#' @importFrom tibble tibble
#'
#' @export
#'
#' @examples
#' data_exemple <- tibble::tibble(
#'   sp = "SAFO",
#'   age = sample(0:10, size = 200, replace = TRUE)
#' )
#' modele_exemple <- stats::glm(age ~ 1, data = data_exemple, family = stats::poisson())
#' info_modele_exemple <- tibble::tibble(
#'   methode = "poisson", A = 38, ic95 = "[32-45]"
#' )
#' mortalite_plot_modele(data_exemple, modele_exemple, info_modele_exemple)
mortalite_plot_modele <- function(
    specimen,
    modele,
    info_modele,
    peak_plus
    ) {
  
  # Validation de base ====
  if (is.null(peak_plus) ||
      length(peak_plus) != 1 ||
      is.na(peak_plus) ||
      !is.numeric(peak_plus)) {
    return(NULL)
    
  }
  if (is.null(specimen) || !is.data.frame(specimen) || nrow(specimen) == 0) {
    return(NULL)
  }
  
  if (!all(c("sp", "age") %in% names(specimen))) {
    return(NULL)
  }
  
  if (is.null(modele)) {
    return(NULL)
  }
  
  # Préparation des données ====
  donnees_age <- specimen |>
    filter(!is.na(.data$age)) |>
    mutate(age = as.integer(.data$age))
  
  if (nrow(donnees_age) == 0) {
    return(NULL)
  }
  
  if (length(unique(donnees_age$sp)) != 1) {
    return(NULL)
  }
  
  max_age <- max(donnees_age$age, na.rm = TRUE)
  
  info_pen <- tryCatch(
    get_info_pen(as.character(unique(donnees_age$sp))),
    error = function(e) NULL
  )
  
  nom_espece <- if (!is.null(info_pen) && "nom_sp" %in% names(info_pen)) {
    info_pen$nom_sp
  } else {
    "poissons"
  }
  
  # Prédiction du modèle ====
  donnees_prediction <- tibble(
    age = peak_plus:(max_age + 2)
  )
  
  pred_link <- tryCatch(
    predict(modele, newdata = donnees_prediction, type = "link"),
    error = function(e) NULL
  )
  
  if (is.null(pred_link)) {
    return(NULL)
  }
  
  donnees_prediction$pred <- as.numeric(exp(pred_link))
  
  # Limite Y du graphique ===============================================
  
  # Maximum de la structure d'âge
  max_y_hist <- max( table(donnees_age$age), na.rm = TRUE )
  
  # Maximum de la courbe du modèle
  max_y_modele <- max( donnees_prediction$pred, na.rm = TRUE )
  
  # On prend le plus grand des deux
  max_y <- ceiling( max(max_y_hist, max_y_modele) * 1.1 )
  
  
  # Détermination de la méthode du modèle ====
  methode_modele <- attr(modele, "methode")
  
  if (is.null(methode_modele)) {
    modele_class <- class(modele)[1]
    
    methode_modele <- case_when(
      modele_class == "glm" ~ "poisson",
      modele_class == "glmmTMB" && grepl("nbinom1", paste(deparse(modele$call$family), collapse = " ")) ~ "nb1",
      modele_class == "negbin" ~ "nb2",
      modele_class == "glmmTMB" && grepl("compois", paste(deparse(modele$call$family), collapse = " ")) ~ "cmp",
      modele_class == "glmmTMB" && grepl("genpois", paste(deparse(modele$call$family), collapse = " ")) ~ "gp",
      TRUE ~ NA_character_
    )
  }
  
  # Extraction du sous-titre ====
  sous_titre <- NULL
  
  if (!is.null(info_modele) && is.data.frame(info_modele) && nrow(info_modele) > 0 &&
      all(c("methode", "A", "ic95") %in% names(info_modele)) &&
      !is.na(methode_modele)) {
    
    ligne_info_modele <- info_modele |>
      filter(tolower(.data$methode) == tolower(methode_modele))
    
    if (nrow(ligne_info_modele) >= 1) {
      
      a_formate <- format(
        ligne_info_modele$A[1],
        nsmall = 1,
        decimal.mark = ","
      )
      
      sous_titre <- glue(
        "Modèle {methode_modele}\n",
        "A = {a_formate} %, IC 95 % = {ligne_info_modele$ic95[1]}"
      ) |>
        as.character()
    }
  }
  
  # Tracé final ====

  ggplot(donnees_age, aes(x = .data$age)) +
    geom_histogram(
      binwidth = 1,
      closed = "right",
      fill = couleur_default,
      color = "white",
      na.rm = TRUE
    ) +
    geom_line(
      data = donnees_prediction,
      aes(x = .data$age, y = .data$pred),
      color = "red",
      linewidth = 1.2,
      inherit.aes = FALSE
    ) +
    labs(
      title = "Distribution d'âge et modèle de mortalité",
      subtitle = sous_titre,
      x = "Âge",
      y = paste0("Nb. ", nom_espece, " échantillonnés")
    ) +
    theme_aquapop() +
    theme(
      plot.subtitle = element_text(
        lineheight = 1.5
      )
    ) +
    scale_x_continuous(
      expand = c(0, 0),
      limits = c(0, max_age + 2),
      breaks = 0:(max_age + 2)
    ) +
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, max_y)
    )
}