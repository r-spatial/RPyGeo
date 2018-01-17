a <- rpygeo_build_env()

# a$CheckOutExtension("3D")
py_run_string("arcpy.env.overwriteOutput = True")

rpygeo_geoprocessor(lib = a, fun = "Slope_3d", args = c("C:/Users/f/Google Drive/MA/thesis/R_analysis/data/raster_kw.tif",
                                     "C:/Users/f/Google Drive/MA/thesis/R_analysis/data/raster_kw_r.tif"), overwrite = FALSE)


# rpygeo_geoprocessor(a, "RasterToASCII_conversion", c("C:/Users/f/Google Drive/MA/thesis/R_analysis/data/raster_kw.tif",
#                                                      "C:/Users/f/Google Drive/MA/thesis/R_analysis/data/raster_kw.asc"))
#




# arc gis pro tests
# exp <- rpygeo_build_env("C:/Program Files/ArcGIS/Pro/bin/Python/Scripts/conda.exe")
