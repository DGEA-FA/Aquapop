download_data_format_xlsx <- function(givenname, datadown) {
  downloadHandler(
    filename = function() {
      paste(givenname, ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(datadown,
                 file,
                 col_names = TRUE,
                 format_headers = TRUE)
    }
  )
}
