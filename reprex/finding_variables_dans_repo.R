library(fs)
library(stringr)

termeachercher<- "verifier_doublons_data_recolte"

# Liste tous les fichiers R dans le repo
files <- dir_ls(path = ".", recurse = TRUE, glob = "*.R")

# Cherche "profil_df" dans chaque fichier
matches <- lapply(files, function(f) {
  lines <- readLines(f, warn = FALSE)
  matched_lines <- grep(termeachercher, lines, value = TRUE)
  if (length(matched_lines) > 0) {
    return(data.frame(file = f, line = which(lines %in% matched_lines), text = matched_lines, stringsAsFactors = FALSE))
  }
  return(NULL)
})

# Concaténer les résultats
results <- do.call(rbind, matches)

# Afficher les résultats
print(results)

