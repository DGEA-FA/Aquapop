# tests/testthat/helper_create_xlsx_recolte.R

library(writexl)
dir.create(testthat::test_path("testdata"), showWarnings = FALSE, recursive = TRUE)

# Base commune
recolte_base <- data.frame(
  "Numéro LCE"     = c("12345", "12345"),
  "Nom du lac"     = c("Lac Test", "Lac Test"),
  "Type pêche"     = c("PE", "PE"),
  "Année"          = c("2020", "2020"),
  "No station"     = c("ST01", "ST01"),
  "Espèce"         = c("EP", "EP"),
  "Nbre capturé"   = c("5", "5"),
  "Nbre pesé"      = c("3", "3"),
  "Commentaires"   = c("Aucun", "Autre ligne")
)

# 1. Fichier complet
write_xlsx(list("Recolte" = recolte_base[1, ]), testthat::test_path("testdata", "recolte_complete.xlsx"))

# 2. Année en format Excel (n° série Excel pour 2020-01-01 = 43831)
recolte_excel_annee <- recolte_base[1, ]
recolte_excel_annee$Année <- "43831"
write_xlsx(list("Recolte" = recolte_excel_annee), testthat::test_path("testdata", "recolte_annee_excel.xlsx"))

# 3. Année texte
recolte_texte_annee <- recolte_base[1, ]
recolte_texte_annee$Année <- "2021"
write_xlsx(list("Recolte" = recolte_texte_annee), testthat::test_path("testdata", "recolte_annee_texte.xlsx"))

# 4. Doublons stricts
recolte_doublons <- recolte_base[rep(1, 2), ]
write_xlsx(list("Recolte" = recolte_doublons), testthat::test_path("testdata", "recolte_doublons.xlsx"))

# 5. Deux lignes identiques sauf commentaires
write_xlsx(list("Recolte" = recolte_base[1:2, ]), testthat::test_path("testdata", "recolte_doublons_commentaires.xlsx"))

# 6. Avec colonne nom_lac
write_xlsx(list("Recolte" = recolte_base[1, ]), testthat::test_path("testdata", "recolte_avec_nom_lac.xlsx"))

# 7. Sans commentaires
recolte_sans_comments <- recolte_base[1, 1:8]  # Retirer la 9e colonne
write_xlsx(list("Recolte" = recolte_sans_comments), testthat::test_path("testdata", "recolte_sans_comments.xlsx"))

# 8. Colonnes désordonnées
set.seed(42)
recolte_colonnes_desordonnees <- recolte_base[1, sample(names(recolte_base))]
write_xlsx(list("Recolte" = recolte_colonnes_desordonnees), testthat::test_path("testdata", "recolte_colonnes_desordonnees.xlsx"))

# 9. Colonnes supplémentaires
recolte_col_sup <- recolte_base[1, ]
recolte_col_sup$extra1 <- "X"
recolte_col_sup$extra2 <- "Y"
write_xlsx(list("Recolte" = recolte_col_sup), testthat::test_path("testdata", "recolte_colonnes_sup.xlsx"))
