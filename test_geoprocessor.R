
a <- rpygeo_build_env(x64 = TRUE, workspace =
                        "C:/Users/f/Google Drive/MA/thesis/R_analysis/data")



a <- rpygeo_build_env(x64 = TRUE)


rpygeo_geoprocessor(lib = a, fun = "Slope_3d", args = c(
  "raster_kw.tif",
  "raster_kw_r.tif"
), overwrite = TRUE, extensions = c("Spatial"), detect_required_extension = TRUE,
workspace = "C:/Users/f/Google Drive/MA/thesis/R_analysis/data")



# get help for a function
py_function_docs("a$Slope_3d")



rpygeo_geoprocessor(lib = a, fun = "Slope_3d", args = c(
  "C:/Users/f/Google Drive/MA/thesis/R_analysis/data/raster_kw.tif",
  "C:/Users/f/Google Drive/MA/thesis/R_analysis/data/raster_kw_r.tif"
), overwrite = TRUE, extensions = c("Spatial"), detect_required_extension = TRUE)







# rpygeo_geoprocessor(a, "RasterToASCII_conversion",
#c("C:/Users/f/Google Drive/MA/thesis/R_analysis/data/raster_kw.tif",
#   "C:/Users/f/Google Drive/MA/thesis/R_analysis/data/raster_kw.asc"))
#




# arc gis pro tests
# exp <- rpygeo_build_env("C:/Program Files/ArcGIS/Pro/bin/Python/Scripts/conda.exe")

# test rpygeo_build_env extended functionallity
b <- rpygeo_build_env(overwrite = TRUE, extensions = c("3d", "Spatial", "na"))

b$Slope_3d(
  "C:/Users/f/Google Drive/MA/thesis/R_analysis/data/raster_kw.tif",
  "C:/Users/f/Google Drive/MA/thesis/R_analysis/data/raster_kw_r.tif"
)
