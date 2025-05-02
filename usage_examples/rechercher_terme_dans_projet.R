#' rechercher_terme_dans_projet
#'
#' Recherche un terme donné dans tous les fichiers R, Rmd et qmd du projet (incluant les tests),
#' et affiche les correspondances avec fichier, ligne et extrait de code.
#'
#' @param terme (chaîne) Terme à rechercher (ex. "ma_fonction").
#' @param dossier (chaîne) Chemin du dossier de départ (défaut = ".").
#' @return Un data.frame des correspondances (fichier, ligne, texte).
#' @examples
#' rechercher_terme_dans_projet("plot")

# Vérifier et charger les packages nécessaires
required_packages <- c("fs", "stringr", "purrr", "dplyr")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

rechercher_terme_dans_projet <- function(terme, dossier = ".") {
  script_actuel <- sys.frames()[[1]]$ofile
  script_nom <- if (!is.null(script_actuel)) basename(script_actuel) else ""
  
  message("\n🔍 Recherche du terme : \033[1m", terme, "\033[0m dans les fichiers du projet...\n")
  
  fichiers <- fs::dir_ls(
    path = dossier,
    recurse = TRUE,
    regexp = "\\.(R|Rmd|qmd)$"
  )
  
  # Exclure ce script uniquement (mais PAS les fichiers dans test/)
  fichiers <- fichiers[!stringr::str_detect(fichiers, script_nom)]
  
  rechercher_terme <- function(fichier) {
    lignes <- readLines(fichier, warn = FALSE)
    lignes_idx <- grep(terme, lignes)
    lignes_txt <- lignes[lignes_idx]
    
    if (length(lignes_idx) > 0) {
      return(data.frame(
        fichier = rep(fichier, length(lignes_idx)),
        ligne = lignes_idx,
        texte = lignes_txt,
        stringsAsFactors = FALSE
      ))
    }
    return(NULL)
  }
  
  
  resultats <- purrr::map_dfr(fichiers, rechercher_terme)
  
  if (nrow(resultats) > 0) {
    message("\n✅ \033[1mCorrespondances trouvées :\033[0m\n")
    for (i in seq_len(nrow(resultats))) {
      cat(
        "📄 Fichier : \033[34m", resultats$fichier[i], "\033[0m\n",
        "   Ligne   : \033[33m", resultats$ligne[i], "\033[0m\n",
        "   Code    : ", resultats$texte[i], "\n",
        strrep("-", 80), "\n", sep = ""
      )
    }
  } else {
    message("❌ Aucune correspondance trouvée pour '", terme, "'.")
  }
  
  invisible(resultats)
}

# Exemple d'exécution
rechercher_terme_dans_projet("mortalite_plot_modele")
