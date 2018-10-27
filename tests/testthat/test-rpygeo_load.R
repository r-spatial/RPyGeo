context("rpygeo_load")
library(spData)
library(raster)
library(sf)
library(reticulate)

test_that("Load tif raster file", {
  skip_on_cran()

  temp_dir <- tempdir()
  dir.create(paste0(temp_dir, "/test1_1"))
  temp_dir <- paste0(temp_dir, "/test1_1")
  writeRaster(elev, paste0(temp_dir, "/elev.tif"), overwrite=TRUE)

  arcpy <- rpygeo_build_env(workspace = temp_dir,
                            overwrite = TRUE)

  arcpy$sa$Raster(paste0("elev.tif")) %>%
    rpygeo_load() %>%
    expect_s4_class("RasterLayer")
})

test_that("Load img raster file", {
  skip_on_cran()

  temp_dir <- tempdir()
  dir.create(paste0(temp_dir, "/test1_2"))
  temp_dir <- paste0(temp_dir, "/test1_2")
  writeRaster(elev, paste0(temp_dir, "/elev.img"), overwrite=TRUE)

  arcpy <- rpygeo_build_env(workspace = temp_dir,
                            overwrite = TRUE)

  arcpy$sa$Raster(paste0("elev.img")) %>%
    rpygeo_load() %>%
    expect_s4_class("RasterLayer")
})

test_that("Load asc raster file", {
  skip_on_cran()

  temp_dir <- tempdir()
  dir.create(paste0(temp_dir, "/test1_4"))
  temp_dir <- paste0(temp_dir, "/test1_4")
  writeRaster(elev, paste0(temp_dir, "/elev.asc"), overwrite=TRUE)

  arcpy <- rpygeo_build_env(workspace = temp_dir,
                            overwrite = TRUE)

  arcpy$sa$Raster(paste0("elev.asc")) %>%
    rpygeo_load() %>%
    expect_s4_class("RasterLayer")
})

test_that("Loads shp vector file", {
  skip_on_cran()

  temp_dir <- tempdir()
  dir.create(paste0(temp_dir, "/test1_5"))
  temp_dir <- paste0(temp_dir, "/test1_5")
  expect_warning(st_write(nz, paste0(temp_dir, "/nz.shp"), quiet=TRUE))

  arcpy <- rpygeo_build_env(workspace = temp_dir,
                            overwrite = TRUE)

    nz_test <- rpygeo_load("nz.shp") %>%
    expect_s3_class("sf")
})

test_that("Load raster dataset file geodatabase", {
  skip_on_cran()

  temp_dir <- tempdir()
  dir.create(paste0(temp_dir, "/test1_6"))
  temp_dir <- paste0(temp_dir, "/test1_6")
  writeRaster(elev, paste0(temp_dir, "/elev.tif"), overwrite=TRUE)

  arcpy <- rpygeo_build_env(workspace = temp_dir,
                            overwrite = TRUE)

  arcpy$CreateFileGDB_management(temp_dir, "test_db")

  arcpy <- rpygeo_build_env(workspace = paste0(temp_dir, "/test_db.gdb"),
                            overwrite = TRUE)

  a <- py_run_string(paste0("arcpy.RasterToGeodatabase_conversion('",
                       temp_dir,
                       "/elev.tif', '",
                       temp_dir,
                       "/test_db.gdb')"))

    rpygeo_load("elev") %>%
      expect_s4_class("RasterLayer")
})

test_that("Load feature class file geodatabase", {
  skip_on_cran()

  temp_dir <- tempdir()
  dir.create(paste0(temp_dir, "/test1_7"))
  temp_dir <- paste0(temp_dir, "/test1_7")
  expect_warning(st_write(nz, paste0(temp_dir, "/nz.shp"), quiet=TRUE))

  arcpy <- rpygeo_build_env(workspace = temp_dir,
                            overwrite = TRUE)

  arcpy$CreateFileGDB_management(temp_dir, "test_db")

  arcpy <- rpygeo_build_env(workspace = paste0(temp_dir, "/test_db.gdb"),
                            overwrite = TRUE)

  a <- py_run_string(paste0("arcpy.FeatureClassToGeodatabase_conversion('",
                       temp_dir,
                       "/nz.shp', '",
                       temp_dir,
                       "/test_db.gdb')"))

  rpygeo_load("nz") %>%
    expect_s3_class("sf")
})
