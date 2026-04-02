#' Préparer les données corrigées de fréquence d'âge pour l'estimation de la mortalité
#'
#' Cette fonction applique `agesurv()` avec `type = 1` pour générer
#' les données individuelles de fréquence d'âge sur la *descending limb* (partie descendante de la courbe de capture).
#' Elle filtre d'abord les spécimens ayant un âge valide (`data_valid`), applique l'estimation (`data_agesurv`),
#' puis complète les classes d'âge manquantes avec des zéros pour produire une table uniforme (`data_final`).
#'
#' @importFrom dplyr arrange
#' @importFrom tidyr replace_na
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom tibble tibble
#' @importFrom dplyr filter
#' @importFrom checkmate assert_int assert_numeric assert_names assert_data_frame assert
#' @importFrom fishmethods agesurv
#' @param data Un `data.frame` contenant les spécimens d'une seule espèce, avec une colonne nommée `age`
#'             (valeurs numériques entières ≥ 0). Les valeurs manquantes (`NA`) seront automatiquement exclues.
#' @param age_peak_plus Un entier indiquant l'âge à partir duquel commence l'analyse de mortalité
#'                      (souvent le pic d'abondance + 1).
#' @param age_max Un entier indiquant l'âge maximum à considérer pour l'estimation de la mortalité.
#'
#' @return Un `data.frame` nommé `data_final` contenant :
#' \describe{
#'   \item{`age`}{Âge (entier)}
#'   \item{`number`}{Nombre d'individus observés à cet âge, incluant les zéros pour les classes absentes}
#' }
#' Toutes les classes entre les âges extrêmes sont incluses, même celles absentes dans les données sources.
#'
#' @export
#'
#' @examples
#' data_exemple <- data.frame(age = c(2, 3, 3, 4, 5, 5, 5, 6, 7, NA))
#' mortalite_prepare_corr(data = data_exemple, age_peak_plus = 5, age_max = 7)
mortalite_prepare_corr <- function(data, age_peak_plus, age_max) {
  # Validations ----
  assert_data_frame(data)
  assert_names(names(data), must.include = "age")
  assert_numeric(data$age, any.missing = TRUE)
  assert_int(age_peak_plus, lower = 0)
  assert_int(age_max, lower = 0)
  
  data_valid <- filter(data, !is.na(age))
  
  assert(
    nrow(data_valid) > 0,
    "Aucune valeur d'âge valide après suppression des NA."
  )
  
  assert(
    age_peak_plus <= max(data_valid$age),
    "`age_peak_plus` est supérieur à l'âge maximum observé dans les données."
  )
  
  assert(
    age_max >= age_peak_plus,
    "`age_max` doit être supérieur ou égal à `age_peak_plus`."
  )
  
  # Estimation avec agesurv ----
  resultat <- agesurv(
    type = 1,
    age = data_valid$age,
    full = age_peak_plus,
    last = age_max,
    estimate = "z",
    method = "cr"
  )
  
  data_agesurv <- resultat$data
  
  assert(
    nrow(data_agesurv) >= 2,
    "L'ajustement agesurv() a retourné un jeu de données trop incomplet (moins de 2 âges)."
  )
  
  # Complétion des classes d'âge ----
  data_final <- tibble(age = seq(min(data_agesurv$age), max(data_agesurv$age))) |>
    left_join(data_agesurv, by = "age") |>
    mutate(number = replace_na(number, 0L)) |>
    arrange(age)
  
  return(data_final)
}
