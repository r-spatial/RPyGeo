library(reticulate)

# paste arcgis python here
use_python(python = "C:/Python27/ArcGIS10.4/python.exe", required = TRUE)

# TODO check if python version is same bit

# check python version
py_config()


# import arcpy libraries
py_run_string("import arcpy")

arcpy <- import("arcpy")

arcpy$CheckOutExtension("Spatial")
arcpy$CheckOutExtension("3D")

# working!!
#arcpy$Slope_3d('C:/Users/f/Google Drive/MA/thesis/R_analysis/data/raster_kw.tif', 'C:/Users/f/Google Drive/MA/thesis/R_analysis/data/raster_kw_r.tif')


# TODO check if spatial extension etc is available
py_run_string("from arcpy import env")



# Set environment settings
py_run_string("env.workspace = 'C:/Users/f/Google Drive/MA/thesis/R_analysis/data'")

# enable overwrite
py_run_string("arcpy.env.overwriteOutput = True")




# Set local variables
py_run_string("inRaster = 'raster_kw.tif'")
py_run_string("outMeasurement = 'DEGREE'")


py_run_string("zFactor = 0.5")


# slope
# Check out the ArcGIS Spatial Analyst extension license
py_run_string("arcpy.CheckOutExtension('Spatial')")

# Execute Slope


# TODO import * from packages that is extendet
py_run_string("from arcpy.sa import *")

py_run_string("Con('raster_kw.tif', 'DEGREE', 0.5)")

# Save the output
py_run_string("outSlope.save('C:/Users/f/Google Drive/MA/thesis/R_analysis/data/hellofromR.tif')")

# check python config
use_python(python = "C:/Python27/ArcGIS10.4/python.exe", required = TRUE)
py_config()


