context("rpygeo_save")
library(spData)
library(raster)
library(sf)
library(reticulate)

test_that("Workspace and scratch workspace are file geodatabase", {
  skip_on_cran()

  temp_dir <- tempdir()
  dir.create(paste0(temp_dir, "/test2_1"))
  temp_dir <- paste0(temp_dir, "/test2_1")

  writeRaster(elev, paste0(temp_dir, "/elev.tif"), overwrite=TRUE)

  arcpy <- rpygeo_build_env(workspace = temp_dir,
                            overwrite = TRUE)

  arcpy$CreateFileGDB_management(temp_dir, "test_db")
  arcpy$CreateFileGDB_management(temp_dir, "scratch_db")

  arcpy <- rpygeo_build_env(workspace = paste0(temp_dir, "/test_db.gdb"),
                          scratch_workspace = paste0(temp_dir, "/scratch_db.gdb"),
                          overwrite = TRUE,
                          extensions = "Spatial")

  py_run_string(
    paste0("arcpy.RasterToGeodatabase_conversion('",
           temp_dir,
           "/elev.tif",
           "', '",
           temp_dir,
           "/test_db.gdb')"))

  ras_elev <- arcpy$sa$Raster("elev")

  arcpy$sa$Aspect(in_raster = ras_elev) %>%
    rpygeo_save("aspect")

  res <- py_run_string("res = arcpy.Exists('aspect')")

  expect_true(res$res)
})

test_that("Workspace is file geodatbase and scratch workspace is directory", {
  skip_on_cran()

  temp_dir <- tempdir()
  dir.create(paste0(temp_dir, "/test2_2"))
  temp_dir <- paste0(temp_dir, "/test2_2")

  writeRaster(elev, paste0(temp_dir, "/elev.tif"), overwrite=TRUE)

  arcpy <- rpygeo_build_env(workspace = temp_dir,
                            overwrite = TRUE)

  arcpy$CreateFileGDB_management(temp_dir, "test_db")

  arcpy <- rpygeo_build_env(workspace = paste0(temp_dir, "/test_db.gdb"),
                            overwrite = TRUE,
                            extensions = "Spatial")

  py_run_string(
    paste0("arcpy.RasterToGeodatabase_conversion('",
           temp_dir,
           "/elev.tif",
           "', '",
           temp_dir,
           "/test_db.gdb')"))

  ras_elev <- arcpy$sa$Raster("elev")

  arcpy$sa$Aspect(in_raster = ras_elev) %>%
    rpygeo_save("aspect")

  res <- py_run_string("res = arcpy.Exists('aspect')")

  expect_true(res$res)
})

test_that("Workspace and scratch workspace are directories", {
  skip_on_cran()

  temp_dir <- tempdir()
  dir.create(paste0(temp_dir, "/test2_3"))
  temp_dir <- paste0(temp_dir, "/test2_3")

  writeRaster(elev, paste0(temp_dir, "/elev.tif"), overwrite=TRUE)

  arcpy <- rpygeo_build_env(workspace = temp_dir,
                            overwrite = TRUE,
                            extensions = "Spatial")

  ras_elev <- arcpy$sa$Raster("elev.tif")

  arcpy$sa$Aspect(in_raster = ras_elev) %>%
    rpygeo_save("aspect.tif")

  res <- py_run_string("res = arcpy.Exists('aspect.tif')")

  expect_true(res$res)
})
