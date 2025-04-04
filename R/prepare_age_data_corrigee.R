#' Préparer les données corrigées de fréquence d'âge pour la mortalité
#'
#' Cette fonction utilise `fishmethods::agesurv()` avec `type = 1` pour générer
#' les données individuelles de la courbe de capture (descending limb),
#' puis complète les classes d'âge manquantes avec des zéros.
#'
#' @param data_specimen Un `data.frame` contenant au moins une colonne `age`.
#'                      Doit être déjà filtré pour une seule espèce.
#' @param age_peak_plus Âge à partir duquel la mortalité est estimée.
#' @param age_max Âge maximal observé dans les spécimens.
#'
#' @return Un `data.frame` avec les colonnes `age` et `number`, incluant les âges manquants.
#' @export
#'
#' @examples
#' df_corr <- prepare_age_data_corrigee(data_specimen = specimen_sana, age_peak_plus = 5, age_max = 10)
#' print(df_corr)
prepare_age_data_corrigee <- function(data_specimen, age_peak_plus, age_max) {
  stopifnot("age" %in% names(data_specimen))
  
  # Étape 1 : estimation initiale (type = 1) pour obtenir la descending limb
  resultat_agesurv <- fishmethods::agesurv(
    type = 1,
    age = data_specimen$age,
    full = age_peak_plus,
    last = age_max,
    estimate = "z",
    method = "cr"
  )
  
  # Étape 2 : récupérer les fréquences d'âge observées
  df_initial <- resultat_agesurv$data
  
  # Étape 3 : insérer les classes manquantes avec des zéros
  df_complet <- tibble::tibble(age = seq(min(df_initial$age), max(df_initial$age), by = 1)) %>%
    dplyr::left_join(df_initial, by = "age") %>%
    dplyr::mutate(number = tidyr::replace_na(number, 0)) %>%
    dplyr::arrange(age)
  
  return(df_complet)
}
