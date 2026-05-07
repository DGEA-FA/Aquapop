# dev/run_tests.R

  # Charger les fonctions du package en développement
devtools::document()

devtools::load_all()

# Exécuter tous les tests unitaires
devtools::test()

fs::dir_tree(path = ".", depth = 2)


# usethis::use_package("tidyselect", type = "Imports")



tools::showNonASCIIfile("R/taille_masse_age.R")
stringi::stri_escape_unicode("Écart-type, Âge, Mâle, à, prêt, résumé")
?taille_masse_age

stringi::stri_escape_unicode("pen_constants")
pen_constants

Extract_IFA_AquaPop_2026_02_27 <- read_excel("inst/extdata/Extract_IFA_AquaPop_2026-02-27.xlsx", 
                                                  sheet = "Lac")


all_combinaisons <- Extract_IFA_AquaPop_2026_02_27 |>
  dplyr::mutate(
    annee = lubridate::year(`Année`)
  ) |>
  dplyr::distinct(
    typ_pech = `Type de pêche`,
    no_lac   = `No plan d'eau`,
    annee
  ) |>
  dplyr::arrange(typ_pech, no_lac, annee)


combinaisons <- all_combinaisons |>
  dplyr::filter(!(no_lac == "39016" & annee == 2014 & type_pech == "PENOF"))


# ==== Tester exemple_utilisation.R sur les combinaisons observées ----

for (i in seq_len(nrow(combinaisons))) {
  
  path     <- "inst/extdata/Extract_IFA_AquaPop_2026-02-27.xlsx"
  typ_pech <- combinaisons$typ_pech[i]
  no_lac   <- combinaisons$no_lac[i]
  annee    <- combinaisons$annee[i]
  
  message(glue::glue(
    "\n==============================",
    "\nItération {i}/{nrow(combinaisons)}",
    "\n{typ_pech} | {no_lac} | {annee}",
    "\n=============================="
  ))
  
  tryCatch(
    {
      source("inst/usage_examples/exemple_utilisation.R")
      message("OK")
    },
    error = function(e) {
      message(glue::glue(
        "\nPLANTAGE DÉTECTÉ",
        "\nItération : {i}",
        "\nType      : {typ_pech}",
        "\nLac       : {no_lac}",
        "\nAnnée     : {annee}",
        "\nErreur    : {e$message}"
      ))
      stop(e)
    }
  )
}
