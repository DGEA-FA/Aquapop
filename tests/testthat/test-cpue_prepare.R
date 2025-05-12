test_that("cpue_prepare() retourne un data.frame structuré correctement", {
  capture <- tibble::tibble(
    no_station = c("A", "B", "C", "D"),
    nb_capture = c(2, 2, 1, 0),
    nb_pese = c(2, 2, 1, 0)
  )
  
  specimen <- tibble::tibble(
    no_station = c("A", "A", "B", "B", "C"),
    sexe = c("F", "M", "F", "F", "M")
  )
  
  # Cas 1 : group = "tous"
  res_all <- cpue_prepare(capture, specimen, group = "tous")
  
  expect_s3_class(res_all, "data.frame")
  expect_named(res_all, c("no_station", "cpue", "group"))
  expect_true(all(res_all$group == "Tous"))
  expect_equal(res_all$cpue[res_all$no_station == "A"], 2)
  expect_equal(res_all$cpue[res_all$no_station == "B"], 2)
  expect_equal(res_all$cpue[res_all$no_station == "C"], 1)
  expect_true("D" %in% res_all$no_station) # Station sans spécimens
  
  # Cas 2 : group = "femelles"
  res_f <- cpue_prepare(capture, specimen, group = "femelles")
  
  expect_s3_class(res_f, "data.frame")
  expect_named(res_f, c("no_station", "cpue", "group"))
  expect_true(all(res_f$group == "Femelles"))
  expect_equal(res_f$cpue[res_f$no_station == "A"], 1)
  expect_equal(res_f$cpue[res_f$no_station == "B"], 2)
  expect_equal(res_f$cpue[res_f$no_station == "C"], 0)
  expect_equal(res_f$cpue[res_f$no_station == "D"], 0)
  
  # Cas 3 : que des femelles
  only_f <- specimen |> filter(sexe == "F")
  res_only_f <- cpue_prepare(capture, only_f, group = "femelles")
  expect_true(all(res_only_f$group == "Femelles"))
  
  # Cas 4 : doublons dans capture
  capture_dup <- bind_rows(capture, capture[1, ])
  res_dup <- cpue_prepare(capture_dup, specimen, group = "tous")
  expect_true("A" %in% res_dup$no_station) # A doit apparaître une fois malgré doublon
  
  # Cas 5 : vérifie que total cpue = nb de spécimens filtrés
  expect_equal(sum(res_all$cpue), nrow(specimen))
  expect_equal(sum(res_f$cpue), sum(specimen$sexe == "F"))
})

test_that("cpue_prepare() retourne cpue = 0 pour une station sans spécimens capturés", {
  capture <- tibble::tibble(
    no_station = c("X"),
    nb_capture = 1,
    nb_pese = 1
  )
  specimen <- tibble::tibble( # station non correspondante
    no_station = character(0),
    sexe = character(0)
  )
  
  res <- cpue_prepare(capture, specimen, group = "tous")
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$no_station, "X")
  expect_equal(res$cpue, 0)
  expect_equal(res$group, "Tous")
})

test_that("cpue_prepare() retourne cpue = 0 pour une station sans femelles capturées", {
  capture <- tibble::tibble(
    no_station = c("Z"),
    nb_capture = 1,
    nb_pese = 1
  )
  specimen <- tibble::tibble(
    no_station = c("Z"),
    sexe = c("M")  # aucun spécimen femelle
  )
  
  res <- cpue_prepare(capture, specimen, group = "femelles")
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$no_station, "Z")
  expect_equal(res$cpue, 0)
  expect_equal(res$group, "Femelles")
})

