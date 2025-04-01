download_data_format_xlsx <- function(nom_output, data) {
  downloadHandler(
    filename = function() {
      paste(nom_output, ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(data,
                 file,
                 col_names = TRUE,
                 format_headers = TRUE)
    }
  )
}
