#' Calculer l’indice PSD-Q global (Proportional Size Distribution – Quality)
#'
#' Cette fonction calcule l’indice PSD-Q pour une espèce cible, à partir des longueurs des spécimens capturés.
#' L’indice PSD-Q correspond à la proportion d’individus situés dans les classes de qualité (Q), définies
#' par des seuils spécifiques à chaque espèce.
#' La fonction utilise les seuils extraits par `get_info_pen()` et retourne l’estimation ponctuelle
#' ainsi qu’un intervalle de confiance à 95 %, calculé selon une méthode binomiale.
#'
#' @importFrom flextable flextable
#' @importFrom dplyr select rename mutate filter
#' @importFrom stats rnorm xtabs
#' @importFrom glue glue
#' @importFrom FSA psdCI lencat
#' @param data Un `data.frame` contenant les données pour une seule espèce.
#'             Doit inclure les colonnes :
#'             - `ltm` : Longueur totale (en mm)
#'             - `sp`  : Code de l’espèce (ex : `"SAFO"`)
#'
#' @return Une liste nommée contenant :
#' \describe{
#'   \item{`data`}{Un `data.frame` avec la valeur de l’indice PSD-Q et son intervalle de confiance à 95 %.}
#'   \item{`flextable`}{Une version formatée (`flextable`) du tableau pour affichage ou export.}
#' }
#'
#' @examples
#' # Exemple avec données simulées
#' set.seed(123)
#' data_ex <- data.frame(
#'   ltm = stats::rnorm(100, mean = 250, sd = 50),
#'   sp = "SAFO"
#' )
#' data_ex <- dplyr::filter(data_ex, ltm > 0)
#' psd_q_res <- psd_q(data_ex)
#' psd_q_res$data
#' psd_q_res$flextable
#'
#' @export
psd_q <- function(data) {
  
  # --- Validation des données ---
  
  if (!is.data.frame(data))
    stop("`data` doit être un data.frame.")
  if (!all(c("ltm", "sp") %in% colnames(data))) {
    stop("Le jeu de données doit contenir les colonnes `ltm` et `sp`.")
  }
  
  sp <- as.character(unique(data$sp))
  if (length(sp) != 1)
    stop("Les données doivent être filtrées pour une seule espèce.")
  
  info <- get_info_pen(sp)
  if (is.null(info))
    stop("Espèce non supportée par `get_info_pen()`.")
  
  # --- Préparation des classes de taille ---
  
  break_class <- info$breaks
  seuil_qualite <- break_class[2]
  
  # --- Filtrage et classification des longueurs ---
  
  donnees_qualite <- data |>
    filter(ltm >= seuil_qualite) |>
    mutate(gcat = lencat(ltm, breaks = break_class, droplevels = TRUE))
  
  # --- Calcul des fréquences par classe ---
  
  freq_classes <- xtabs(~ gcat, data = donnees_qualite)
  freq_relatives <- prop.table(freq_classes) * 100
  freq_vecteur <- apply(freq_relatives, 1, sum)
  
  # --- Pondération : on exclut la première classe (stock) du calcul PSD-Q ---
  
  poids_classes <- rep(1, length(freq_vecteur))
  poids_classes[1] <- 0
  
  # --- Validation des fréquences pondérées ---
  
  if (all((freq_vecteur / 100)[poids_classes == 1] == 0)) {
    stop("Aucune donnée dans les classes pondérées. Impossible de calculer l’indice PSD-Q.")
  }
  
  # --- Calcul de l’indice PSD-Q avec IC 95 % ---
  
  table_resultats <- psdCI(
    poids_classes,
    ptbl = freq_vecteur / 100,
    n = sum(freq_classes),
    method = "binomial",
    label = "PSD Q"
  ) |>
    as.data.frame() |>
    rename(
      Q   = Estimate,
      LCI = `95% LCI`,
      UCI = `95% UCI`
    ) |>
    mutate(ic95 = glue("[{round(LCI, 1)}-{round(UCI, 1)}]")) |>
    select(Q, ic95)
  
  # --- Construction du tableau flextable ---
  
  table_flextable <- flextable(table_resultats) |>
    style_flextable_aquapop()
    
  
  # --- Retour ---
  
  return(list(
    data = table_resultats,
    flextable = table_flextable
  ))
}
