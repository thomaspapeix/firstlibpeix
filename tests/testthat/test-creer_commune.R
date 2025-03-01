test_that("creer_commune fonctionne correctement", {
  df <- data.frame(Code.de.la.commune = rep(12345, 5))

  commune <- creer_commune(df)
  expect_s3_class(commune, "commune")
  expect_error(creer_commune(data.frame()), "Le dataframe doit contenir la colonne 'Code.de.la.commune'")
})
