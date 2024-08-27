download_data_format_docx <- function(givenname, datadown) {
  downloadHandler(
    filename = function() {
      paste(givenname, ".docx", sep = "")
    },
    content = function(file) {
      # Crée un document Word vide
      doc <- officer::read_docx()
      
      # Convertit le dataframe en une flextable
      ft <- flextable::qflextable(datadown)
      
      # Ajoute le tableau flextable au document
      doc <- flextable::body_add_flextable(doc, value = ft)
      
      # Sauvegarde le document Word au fichier spécifié
      print(doc, target = file)
    }
  )
}