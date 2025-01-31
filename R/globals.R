# Vérifier si user_guide.html doit être mis à jour
if (!file.exists("www/user_guide.html") || file.mtime("texte/user_guide.rmd") > file.mtime("www/user_guide.html")) {
  rmarkdown::render(
    input = "texte/user_guide.rmd",
    output_format = "html_document",
    output_file = "../www/user_guide.html" # Sauvegarde directement dans www/
  )
}