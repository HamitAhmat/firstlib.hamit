test_that("filtre_anomalie supprime les anomalies", {
  resultat <- filtre_anomalie(df_velo)

  expect_true(all(is.na(resultat[["Probabilité de présence d'anomalies"]])))
  expect_true(all(resultat$Total > 0))
  expect_true(all(resultat$Total < 10000))
  expect_lt(nrow(resultat), nrow(df_velo))
})

test_that("compter_nombre_trajets retourne un nombre positif", {
  resultat <- compter_nombre_trajets(df_velo)

  expect_type(resultat, "double")
  expect_gt(resultat, 0)
})

test_that("compter_nombre_boucle retourne le bon nombre", {
  resultat <- compter_nombre_boucle(df_velo)

  expect_type(resultat, "integer")
  expect_gt(resultat, 1)
})

test_that("trouver_trajet_max retourne les bonnes colonnes", {
  resultat <- trouver_trajet_max(df_velo)

  expect_s3_class(resultat, "data.frame")
  expect_true("Boucle de comptage" %in% colnames(resultat))
  expect_true("Jour" %in% colnames(resultat))
  expect_true("Total" %in% colnames(resultat))
  expect_true("moyenne_jour_identique" %in% colnames(resultat))
  expect_true("moyenne_boucle_identique" %in% colnames(resultat))
  expect_equal(nrow(resultat), 1)
})

test_that("calcul_distribution_semaine retourne 7 jours", {
  resultat <- calcul_distribution_semaine(df_velo)

  expect_s3_class(resultat, "data.frame")
  expect_true("trajets" %in% colnames(resultat))
  expect_lte(nrow(resultat), 7)

})

test_that("plot_distribution_semaine retourne un ggplot", {
   resultat <- plot_distribution_semaine(df_velo)

   expect_s3_class(resultat, "ggplot")
})

test_that("filtrer_trajet filtre les bonnes boucles", {
  resultat <- filtrer_trajet(df_velo, boucle = c(880, 881))

  expect_lt(nrow(resultat), nrow(df_velo))
  expect_true(all(resultat[["Numéro de boucle"]] %in% c(880, 881)))
})

test_that("filtrer_trajet avec une seule boucle", {
  resultat <- filtrer_trajet(df_velo, boucle = 949)

  expect_true(all(resultat[["Numéro de boucle"]] == 949))
})




test_that("filtrer_trajet retourne tout si boucle est NULL", {
  resultat <- filtrer_trajet(df_velo, boucle = NULL)

  expect_equal(nrow(resultat), nrow(df_velo))
})


test_that("calcul_distribution_semaine avec filtre TRUE", {
  resultat <- calcul_distribution_semaine(df_velo, filtre = TRUE)

  expect_s3_class(resultat, "data.frame")
  expect_true("trajets" %in% colnames(resultat))
  expect_lte(nrow(resultat), 7)
})

test_that("calcul_distribution_semaine avec filtre FALSE", {
  resultat_filtre <- calcul_distribution_semaine(df_velo, filtre = TRUE)
  resultat_sans <- calcul_distribution_semaine(df_velo, filtre = FALSE)

  expect_s3_class(resultat_sans, "data.frame")
  expect_gte(sum(resultat_sans$trajets), sum(resultat_filtre$trajets))
})

test_that("calcul_distribution_semaine filtre TRUE par défaut", {
  resultat_defaut <- calcul_distribution_semaine(df_velo)
  resultat_true <- calcul_distribution_semaine(df_velo, filtre = TRUE)

  expect_equal(resultat_defaut, resultat_true)
})

test_that("plot_distribution_semaine avec filtre FALSE", {
  resultat <- plot_distribution_semaine(df_velo, filtre = FALSE)

  expect_s3_class(resultat, "ggplot")
})
