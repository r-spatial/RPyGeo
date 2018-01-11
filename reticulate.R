
# Filename: reticulate.R (2018-01-04)
#
# TO DO: test reticulate for ArcPy
#
# Author(s): Fabian Polakowski
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. init ArcPy and ArcPy libraries
# 2.
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages


library(reticulate)

# TODO create environment list where user can define pyhton ArcGIS path, otherwise check in C/:Python27 path as default
# paste arcgis python here
use_python(python = "C:/Python27/ArcGIS10.4/python.exe", required = TRUE)

# TODO check if python version is same bit



# TODO check pyconfig -> includes conf$python "ArcGIS"
# check python version
conf <- py_config()



# TODO write init function that sets paths and imports arcpy and all the selected libraries such as "spatial"
# if user does not define ArcGIS libraries try to include all of them



arcpy <- import("arcpy")

# alternative to inport functions from one extension
#arcpy_sa <- import("arcpy.sa")


# TODO seach engine for tools
# e.g. http://desktop.arcgis.com/en/arcmap/10.3/tools/spatial-analyst-toolbox/an-overview-of-the-conditional-tools.htm
#  http://desktop.arcgis.com/en/arcmap/VERSION/tools/TOOLBOX/TOOLNAME(space == '-').htm



# TODO check if spatial extension etc is available - just try all of them as default

# do we need this or can we access every module from our extensions -> we need it!
arcpy$CheckOutExtension("Spatial")
arcpy$CheckOutExtension("3D")


# define in init if overwrite should be TRUE
py_run_string("arcpy.env.overwriteOutput = True")


# working!!
arcpy$Slope_3d('C:/Users/f/Google Drive/MA/thesis/R_analysis/data/raster_kw.tif', 'C:/Users/f/Google Drive/MA/thesis/R_analysis/data/raster_kw_r.tif')
#arcpy$sa$Slope


# see all functions

py_list_attributes(arcpy)


#arcpy$Slope_3d('C:/Users/f/Google Drive/MA/thesis/R_analysis/data/raster_kw.tif', 'C:/Users/f/Google Drive/MA/thesis/R_analysis/data/raster_kw_r.tif')






# check source files C:\Program Files (x86)\ArcGIS\Desktop10.4\arcpy













# py_run_string("from arcpy import env")
#
#
#
# # Set environment settings
# py_run_string("env.workspace = 'C:/Users/f/Google Drive/MA/thesis/R_analysis/data'")
#
# # enable overwrite
# py_run_string("arcpy.env.overwriteOutput = True")
#
#
#
#
# # Set local variables
# py_run_string("inRaster = 'raster_kw.tif'")
# py_run_string("outMeasurement = 'DEGREE'")
#
#
# py_run_string("zFactor = 0.5")
#
#
# # slope
# # Check out the ArcGIS Spatial Analyst extension license
# py_run_string("arcpy.CheckOutExtension('Spatial')")
#
# # Execute Slope
#
#
# # TODO import * from packages that is extendet
# py_run_string("from arcpy.sa import *")
#
# py_run_string("Con('raster_kw.tif', 'DEGREE', 0.5)")
#
# # Save the output
# py_run_string("outSlope.save('C:/Users/f/Google Drive/MA/thesis/R_analysis/data/hellofromR.tif')")
#
# # check python config
# use_python(python = "C:/Python27/ArcGIS10.4/python.exe", required = TRUE)
# py_config()
#
#
