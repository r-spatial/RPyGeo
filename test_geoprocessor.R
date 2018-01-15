a <- rpygeo_build_env()

a$CheckOutExtension("3D")
py_run_string("arcpy.env.overwriteOutput = True")

rpygeo_geoprocessor(a, "Slope_3d", c("C:/Users/f/Google Drive/MA/thesis/R_analysis/data/raster_kw.tif",
                                     "C:/Users/f/Google Drive/MA/thesis/R_analysis/data/raster_kw_r.tif"))


rpygeo_geoprocessor(a, "RasterToASCII_conversion", c("C:/Users/f/Google Drive/MA/thesis/R_analysis/data/raster_kw.tif",
                                                     "C:/Users/f/Google Drive/MA/thesis/R_analysis/data/raster_kw.asc"))


# exp <- rpygeo_build_env("C:/Program Files/ArcGIS/Pro/bin/Python/Scripts/conda.exe")
