context("rpygeo_geoprocessor")
library(RPyGeo)
library(reticulate)

test_that("rypgeo_build_env's python versions contains arcpy", {
  skip_on_cran()

  rpygeo_build_env()

  testthat::expect_true(py_module_available("arcpy"))
})

test_that("rypgeo_build_env's python versions contains arcpy with ArcGIS Pro", {
  skip_on_cran()

  rpygeo_build_env(pro = TRUE)

  testthat::expect_true(py_module_available("arcpy"))
})
