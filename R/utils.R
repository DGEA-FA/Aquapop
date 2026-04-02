#' Calculer le ratio mâles:femelles sous forme simplifiée
#'
#' Cette fonction prend en entrée un nombre de mâles et de femelles, puis retourne un
#' ratio \eqn{M:F} (mâles pour femelles) sous forme réduite à ses plus simples expressions,
#' comme `"3:2"` ou `"1:1"`. Si les deux valeurs sont nulles, la fonction retourne `NA`.
#'
#' @importFrom labelled var_label
#' @importFrom stringi stri_trans_general
#' @importFrom writexl write_xlsx
#' @param male_count Nombre d'individus de sexe masculin (entier)
#' @param female_count Nombre d'individus de sexe féminin (entier)
#'
#' @return Une chaîne de caractères représentant le ratio simplifié (ex: `"3:2"`), ou `NA_character_` si les deux valeurs sont nulles.
#'
#' @export
calculate_mf_ratio <- function(male_count, female_count) {
  if (male_count == 0 && female_count == 0) {
    return(NA_character_)
  }
  
  pgcd <- function(a, b) if (b == 0) a else Recall(b, a %% b)
  divisor <- pgcd(male_count, female_count)
  divisor <- ifelse(divisor == 0, 1, divisor)
  
  paste0(male_count %/% divisor, ":", female_count %/% divisor)
}

#' Arrondir et formater les valeurs p
#'
#' Cette fonction prend une ou plusieurs valeurs de p en entrée et retourne une version
#' arrondie et formatée sous forme de chaîne de caractères, selon les conventions usuelles :
#' - `< 0.001` pour les valeurs très faibles
#' - valeurs numériques à 3 décimales sinon
#'
#' @param p Un vecteur numérique contenant des valeurs p.
#'
#' @return Un vecteur de chaînes de caractères (`character`) contenant les valeurs p formatées.
#' 
#' @examples
#' format_pval(c(0.0005, 0.02, 0.3456, NA))
#' # [1] "< 0.001" "0.020" "0.346" NA
#'
#' @export
format_pval <- function(p) {
  ifelse(is.na(p), NA_character_,
         ifelse(p < 0.001, "< 0.001", formatC(round(p, 3), format = "f", digits = 3)))
}



#' Génère un suffixe de nom de fichier à partir des métadonnées du lac
#'
#' Exemple : `"PENT_2022_LacArchambault_no01565"`
#'
#' @param typ_pech Code du type de pêche (ex: `"PENT"`)
#' @param annee Année de l'inventaire (ex: `2022`)
#' @param no_lac Numéro du lac (ex: `"01565"`)
#' @param nom_lac Nom du lac (ex: `"Lac Archambault"`). Optionnel.
#'
#' @return Une chaîne de caractères.
#'
#' @export
generate_filename_suffix <- function(typ_pech, annee, no_lac, nom_lac = NULL) {
  stopifnot(!missing(typ_pech), !missing(annee), !missing(no_lac))
  
  lac_name_clean <- if (!is.null(nom_lac) && nzchar(nom_lac)) {
    lac <- stri_trans_general(nom_lac, "Latin-ASCII")
    lac <- gsub("[^A-Za-z0-9]+", "", lac)
    paste0(lac, "_")
  } else {
    ""
  }
  
  paste0(typ_pech, "_", annee, "_", lac_name_clean, "no", no_lac)
}

#' Construit un nom de fichier standardisé pour les exports
#'
#' Exemple : `"masselongueur_PENT_2022_LacArchambault_no01565.xlsx"`
#'
#' @param objet Nom du contenu exporté (ex: `"masselongueur"`, `"cpue_tous"`)
#' @param suffixe Résultat de `generate_filename_suffix()`
#' @param ext Extension souhaitée (ex: `"xlsx"`, `"png"`)
#'
#' @return Une chaîne de caractères représentant le nom de fichier.
#'
#' @export
build_export_filename <- function(objet, suffixe, ext = "xlsx") {
  paste0(objet, "_", suffixe, ".", ext)
}

#' Extraire les labels comme noms de colonnes (pour usage dans export)
#'
#' @param data Un `data.frame` avec des labels (`labelled`).
#'
#' @return Un `data.frame` avec les noms de colonnes remplacés par leurs labels s'ils sont valides.
#' @export
#' @importFrom labelled var_label
labelled_data <- function(data) {
  labels <- var_label(data)
  
  # Vérifier si tous les labels sont valides
  if (all(!is.na(labels) & nzchar(labels))) {
    colnames(data) <- unlist(labels)
  }
  
  return(data)
}


#' Gérer les erreurs silencieusement
#'
#' Fonction utilitaire pour extraire le message d'erreur dans un `tryCatch`.
#'
#' @param e Objet d'erreur (`condition`) attrapé par `tryCatch`.
#' @return Message de l'erreur (chaîne de caractères).
#'
#' @export
handle_error <- function(e) {
  conditionMessage(e)
}
