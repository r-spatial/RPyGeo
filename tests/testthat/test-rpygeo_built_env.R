context("rpygeo_geoprocessor")

test_that("rypgeo_build_env's python versions contains arcpy", {

  skip_on_cran()

  rpygeo_build_env()

  testthat::expect_true(py_module_available("arcpy"))

})





test_that("", {

  skip_on_cran()



})

