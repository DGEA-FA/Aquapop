# dev/run_tests.R

# Charger les fonctions du package en développement
devtools::load_all()

# Exécuter tous les tests unitaires
devtools::test()

fs::dir_tree(path = ".", depth = 2)

devtools::document()




library(stringr)
library(fs)
library(readr)
library(glue)

# Liste tous les fichiers R
r_files <- dir_ls("R", regexp = "\\.R$", recurse = FALSE)

# Fonction principale
interact_clean_imports <- function(file) {
  lines <- read_lines(file)
  
  modified <- FALSE
  
  for (i in seq_along(lines)) {
    matches <- str_extract_all(lines[i], "\\b([a-zA-Z0-9\\.]+)::([a-zA-Z0-9_\\.]+)\\b")[[1]]
    
    if (length(matches) > 0) {
      for (full_call in matches) {
        pkg <- str_extract(full_call, "^[^:]+")
        fun <- str_extract(full_call, "(?<=::)[^:]+")
        
        import_line <- glue("#' @importFrom {pkg} {fun}")
        doc_has_import <- any(str_detect(lines[1:i], fixed(import_line)))
        
        cat(cli::rule(), "\n")
        cat("Fichier :", file, "\n")
        cat("Ligne ", i, ": ", lines[i], "\n")
        cat("→ Appel détecté : ", full_call, "\n")
        if (doc_has_import) {
          cat("✅ Import déjà présent :", import_line, "\n")
        } else {
          cat("⚠️  Import manquant :", import_line, "\n")
        }
        
        answer <- readline(prompt = glue("Remplacer `{full_call}` par `{fun}` ? (o/n) [défaut: n] "))
        
        if (tolower(answer) %in% c("o", "oui")) {
          lines[i] <- str_replace_all(lines[i], fixed(full_call), fun)
          modified <- TRUE
          cat("✅ Remplacement effectué.\n")
          
          # Suggère l'ajout de @importFrom si manquant
          if (!doc_has_import) {
            where_to_insert <- which(str_detect(lines, "^#' @")) |> min(na.rm = TRUE)
            if (is.finite(where_to_insert)) {
              lines <- append(lines, values = import_line, after = where_to_insert - 1)
              cat("➕ Ligne ajoutée :", import_line, "\n")
              modified <- TRUE
            } else {
              cat("❗ Aucun bloc roxygen2 détecté, ligne non ajoutée.\n")
            }
          }
        } else {
          cat("⏭️  Conservé tel quel.\n")
        }
      }
    }
  }
  
  if (modified) {
    write_lines(lines, file)
    cat("💾 Fichier modifié :", file, "\n")
  } else {
    cat("✔ Aucun changement requis dans :", file, "\n")
  }
}

# Appliquer à tous les fichiers
walk(r_files, interact_clean_imports)
