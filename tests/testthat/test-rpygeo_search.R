context("rpygeo_search")

test_that("Search for all functions", {
  skip_on_cran()

  arcpy <- rpygeo_build_env(workspace = tempdir(),
                            overwrite = TRUE)

  rpygeo_search() %>% expect_type("list")
})

test_that("Search for functions in main module", {
  skip_on_cran()

  arcpy <- rpygeo_build_env(workspace = tempdir(),
                            overwrite = TRUE)

  res <- rpygeo_search()
  expect_true(length(res$main) > 0)


})

test_that("Search for functions in Spatial Analyst extension", {
  skip_on_cran()

  arcpy <- rpygeo_build_env(workspace = tempdir(),
                            overwrite = TRUE)

  res <- rpygeo_search()
  expect_true(length(res$sa) > 0)
})
