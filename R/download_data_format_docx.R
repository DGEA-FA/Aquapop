# Fonction pour télécharger le tableau formaté en tant que document Word
download_data_format_docx <- function(givenname, ft_table) {
  downloadHandler(
    filename = function() {
      paste(givenname, ".docx", sep = "")
    },
    content = function(file) {
      # Créer un document Word et ajouter le tableau flextable
      doc <- read_docx() %>%
        body_add_flextable(ft_table) %>%
        body_add_par(" ", style = "Normal")  # Ajouter un espace après le tableau
      
      # Sauvegarder le document Word
      print(doc, target = file)
    }
  )
}