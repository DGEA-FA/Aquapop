test_that("Cas normal : wri() retourne une liste structurée", {
  specimen <- data.frame(
    sp = rep("SANA", 12),
    ltm = c(300, 320, 340, 360, 380, 400, 420, 440, 460, 480, 500, 520),
    masse = c(200, 230, 250, 270, 300, 340, 370, 400, 430, 460, 480, 510),
    sexe = rep(c("F", "M", "IND"), 4)
  )
  
  res <- wri(specimen)
  
  expect_type(res, "list")
  expect_named(res, c("data", "flextable", "plot_tous", "plot_byclass"))
  expect_s3_class(res$flextable, "flextable")
  expect_s3_class(res$plot_tous, "gg")
  expect_s3_class(res$plot_byclass, "gg")
  expect_s3_class(res$data, "data.frame")
})

test_that("Filtrage : ltm manquant est ignoré sans planter", {
  specimen <- data.frame(
    sp = rep("SANA", 5),
    ltm = c(320, NA, 350, NA, 400),
    masse = c(200, 220, 240, 260, 280),
    sexe = rep("F", 5)
  )
  
  expect_no_error(wri(specimen))
})

test_that("Filtrage : masse partiellement manquante", {
  specimen <- data.frame(
    sp = rep("SANA", 5),
    ltm = c(300, 310, 320, 330, 340),
    masse = c(200, NA, 220, NA, 240),
    sexe = rep("M", 5)
  )
  
  res <- wri(specimen)
  expect_s3_class(res$data, "data.frame")
})

test_that("Toutes les classes de taille ne sont pas représentées", {
  specimen <- data.frame(
    sp = rep("SAFO", 4),
    ltm = c(130, 135, 140, 145),  # classes basses uniquement
    masse = c(30, 35, 40, 45),
    sexe = rep("F", 4)
  )
  
  res <- wri(specimen)
  expect_s3_class(res$data, "data.frame")
})

test_that("Tous les spécimens sont IND : pas d’erreur", {
  specimen <- data.frame(
    sp = rep("SAVI", 6),
    ltm = c(160, 165, 170, 175, 180, 185),
    masse = c(80, 85, 90, 95, 100, 105),
    sexe = rep("IND", 6)
  )
  
  res <- wri(specimen)
  expect_s3_class(res$data, "data.frame")
})

test_that("Seulement des femelles ou seulement des mâles : pas d’erreur", {
  femelles <- data.frame(
    sp = rep("SANA", 4),
    ltm = c(300, 310, 320, 330),
    masse = c(200, 210, 220, 230),
    sexe = rep("F", 4)
  )
  males <- data.frame(
    sp = rep("SANA", 4),
    ltm = c(350, 360, 370, 380),
    masse = c(250, 260, 270, 280),
    sexe = rep("M", 4)
  )
  
  expect_no_error(wri(femelles))
  expect_no_error(wri(males))
})

test_that("Plus d'une espèce dans les données : erreur explicite", {
  specimen <- data.frame(
    sp = c("SANA", "SAFO"),
    ltm = c(300, 320),
    masse = c(250, 270),
    sexe = c("F", "M")
  )
  
  expect_error(wri(specimen), "une seule espèce")
})

test_that("Colonnes essentielles manquantes : erreur explicite", {
  # Manque sp
  df1 <- data.frame(ltm = 300:304, masse = 200:204, sexe = "F")
  expect_error(wri(df1), "Colonnes manquantes : sp")
  
  # Manque ltm
  df2 <- data.frame(sp = "SANA", masse = 200:204, sexe = "F")
  expect_error(wri(df2), "Colonnes manquantes : ltm")
  
  # Manque masse
  df3 <- data.frame(sp = "SANA", ltm = 300:304, sexe = "F")
  expect_error(wri(df3), "Colonnes manquantes : masse")
})

test_that("ltm trop faible → retourne une structure vide", {
  specimen <- data.frame(
    sp = rep("SANA", 3),
    ltm = c(100, 110, 120),  # en dessous du seuil min_TL
    masse = c(80, 85, 90),
    sexe = c("F", "M", "F")
  )
  
  res <- wri(specimen)
  
  expect_s3_class(res$data, "data.frame")
  expect_equal(nrow(res$data), 0)
  expect_s3_class(res$flextable, "flextable")
})

test_that("ltm trop faible → retourne une structure vide valide", {
  specimen <- data.frame(
    sp = rep("SANA", 3),
    ltm = c(100, 110, 120),  # en dessous du seuil min_TL
    masse = c(80, 85, 90),
    sexe = c("F", "M", "F")
  )
  
  res <- wri(specimen)
  
  expect_s3_class(res$data, "data.frame")
  expect_equal(nrow(res$data), 0)
  expect_named(res$data, c("groupe", "wr", "ic95", "n"))
  expect_s3_class(res$flextable, "flextable")
  expect_s3_class(res$plot_tous, "gg")
  expect_s3_class(res$plot_byclass, "gg")
})

test_that("wri() retourne une structure vide proprement si aucune donnée ne passe les filtres", {
  specimen <- data.frame(
    sp = rep("SAFO", 5),
    ltm = c(50, 60, 70, 80, 90),  # sous le seuil min_TL
    masse = c(10, 12, 14, 16, 18),
    sexe = rep("IND", 5)
  )
  
  res <- wri(specimen)
  
  expect_type(res, "list")
  expect_s3_class(res$data, "data.frame")
  expect_equal(nrow(res$data), 0)
  expect_named(res$data, c("groupe", "wr", "ic95", "n"))
  expect_s3_class(res$flextable, "flextable")
})
