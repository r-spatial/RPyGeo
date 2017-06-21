

#' ArcGIS Geoprocessing in R via Python
#'
#' Provide access to (virtually any) ArcGIS Geoprocessing tool from within R by
#' running Python geoprocessing scripts without writing Python code or touching
#' ArcGIS.
#'
#' \tabular{ll}{ Package: \tab RPyGeo\cr Type: \tab Package\cr Version: \tab
#' 0.9-3\cr Date: \tab 2011-09-07\cr License: \tab GPL\cr } The function
#' \code{rpygeo.geoprocessor} is the core function of this package. It creates
#' and runs a Python script that executes your ArcGIS/Python geoprocessing
#' command from within R. This function can be used to define more convenient
#' wrappers for frequently used geoprocessing tools. Some are already
#' implemented, for example rpygeo.Slope.sa and rpygeo.EucDistance.sa, more are
#' to be added in future releases.
#'
#' @name RPyGeo-package
#' @aliases RPyGeo-package RPyGeo
#' @docType package
#' @author Alexander Brenning <brenning@@uwaterloo.ca>
#' @keywords package interface database
#' @examples
#'
#' \dontrun{rpygeo.geoprocessor("Slope_sa('dem','slope')",
#'   "Aspect_sa('dem','aspect')",
#'   "Hillshade_sa('dem','hshd')")
#' rpygeo.Slope.sa("dem","slope")}
#'
NULL





#' Wrappers for solar radiation and viewshed geoprocessing tools
#'
#' Wrappers for ArcGIS geoprocessing tools for calculating solar radiation and
#' viewsheds (Spatial Analyst extension).
#'
#' These functions simply interface the behaviour of the ArcGIS/Python
#' geoprocessing functions with the equivalent names. See
#' \code{\link{rpygeo.geoprocessor}} for details on what happens behind the
#' scenes.
#'
#' ArcGIS 9.2 online help for the georpocessing tools can be accessed through
#' the following URLs: \itemize{
#' \itemAreaSolarRadiation\url{http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Area_Solar_Radiation}
#' \itemViewShed\url{http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Viewshed}
#' }
#'
#' @aliases rpygeo.AreaSolarRadiation.sa rpygeo.Viewshed.sa
#' @param in.surface.raster,in.raster, Names of ArcGIS raster datasets, or
#' feature classes in a geodatabase (relative to the current workspace defined
#' in a \code{rpygeo.env} environment).
#' @param out.global.radiation.raster,out.raster, Names of ArcGIS raster
#' datasets, or feature classes in a geodatabase (relative to the current
#' workspace defined in a \code{rpygeo.env} environment).
#' @param out.direct.radiation.raster,out.diffuse.radiation.raster, Names of
#' ArcGIS raster datasets, or feature classes in a geodatabase (relative to the
#' current workspace defined in a \code{rpygeo.env} environment).
#' @param out.direct.duration.raster Names of ArcGIS raster datasets, or
#' feature classes in a geodatabase (relative to the current workspace defined
#' in a \code{rpygeo.env} environment).
#' @param latitude,sky.size,time.configuration, Arguments to be passed to the
#' Python geoprocessing tool. See ArcGIS help files (link below) for
#' information on the usage of scripting commands and their arguments.
#' @param day.interval,hour.interval,each.interval,z.factor, Arguments to be
#' passed to the Python geoprocessing tool. See ArcGIS help files (link below)
#' for information on the usage of scripting commands and their arguments.
#' @param slope.aspect.input.type, Arguments to be passed to the Python
#' geoprocessing tool. See ArcGIS help files (link below) for information on
#' the usage of scripting commands and their arguments.
#' @param calculation.directions,zenith.divisions,azimuth.divisions, Arguments
#' to be passed to the Python geoprocessing tool. See ArcGIS help files (link
#' below) for information on the usage of scripting commands and their
#' arguments.
#' @param diffuse.model.type,diffuse.proportion, Arguments to be passed to the
#' Python geoprocessing tool. See ArcGIS help files (link below) for
#' information on the usage of scripting commands and their arguments.
#' @param transmittivity,curvature.correction, Arguments to be passed to the
#' Python geoprocessing tool. See ArcGIS help files (link below) for
#' information on the usage of scripting commands and their arguments.
#' @param refractivity.coefficient Arguments to be passed to the Python
#' geoprocessing tool. See ArcGIS help files (link below) for information on
#' the usage of scripting commands and their arguments.
#' @param in.observer.features Name of shapefile (including file extension
#' \code{".shp"}) with observer point or polygon features, or \code{data.frame}
#' with observer point features. If this is a \code{data.frame}, it will be
#' written to a temporary shapefile in folder \code{tmpdir}, and x and y
#' coordinates will be taken from the attributes identified by \code{x.field}
#' and \code{y.field}
#' @param x.field,y.field (optional) names of x and y coordinates if
#' \code{in.observer.features} is a \code{data.frame}
#' @param tmpdir (optional) name of folder for temporary files (when
#' \code{in.observer.features} is a \code{data.frame}
#' @param \dots Additional arguments to be passed to
#' \code{\link{rpygeo.geoprocessor}}.
#' @return The functions return \code{NULL} if no error occurred, otherwise a
#' character vector containing the error message.
#' @author Alexander Brenning
#' @seealso \code{\link{rpygeo.geoprocessor}}, \code{\link{rpygeo.build.env}}
#' @keywords interface database
NULL





#' Wrappers for ASCII-to-raster conversion
#'
#' Wrappers for ASCII-to-raster conversion functions from the Conversion
#' toolbox.
#'
#' These functions simply interface the behaviour of the ArcGIS/Python
#' geoprocessing functions with the equivalent names. See
#' \code{\link{rpygeo.geoprocessor}} for details on what happens behind the
#' scenes.
#'
#' ArcGIS 9.2 online help for the georpocessing tools can be accessed through
#' the following URLs: \itemize{
#' \itemASCIIToRaster\url{http://webhelp.esri.com/arcgisdesktop/9.2/body.cfm?tocVisable=1&ID=1309&TopicName=ASCII
#' to Raster (Conversion)}
#' \itemRasterToASCII\url{http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Raster
#' to ASCII (Conversion)} }
#'
#' @aliases rpygeo.ASCIIToRaster.conversion rpygeo.RasterToASCII.conversion
#' @param in.ascii.file,in.raster,out.raster,out.ascii.file Names of ArcGIS
#' raster datasets, or raster feature classes in a geodatabase (relative to the
#' current workspace defined in a \code{rpygeo.env} environment).  Shapefiles
#' must include the extension \code{".shp"}.
#' @param data.type Arguments to be passed to the Python geoprocessing tool.
#' See ArcGIS help files (link below) for information on the usage of scripting
#' commands and their arguments.
#' @param \dots Additional arguments to be passed to
#' \code{\link{rpygeo.geoprocessor}}.
#' @return The functions return \code{NULL} if no error occurred, otherwise a
#' character vector containing the error message.
#' @author Alexander Brenning
#' @seealso \code{\link{rpygeo.geoprocessor}}, \code{\link{rpygeo.build.env}}
#' @keywords interface database
NULL





#' Wrappers for selected ArcGIS functions
#'
#' Wrappers for a small selection of ArcGIS geoprocessing functions based on
#' the \code{rpygeo.geoprocessor}.
#'
#' These functions simply try to replicate the behaviour of the ArcGIS/Python
#' geoprocessing functions of the same name. See
#' \code{\link{rpygeo.geoprocessor}} for details on what happens behind the
#' scenes.
#'
#' ArcGIS 9.2 online help for the georpocessing tools can be accessed through
#' the following URLs: \itemize{
#' \itemEucDistance\url{http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=EucDistance}
#' \itemHillshade\url{http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Hillshade}
#' \itemSlope\url{http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Slope}
#' \itemAspect\url{http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Aspect}
#' \itemCurvature\url{http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Curvature}
#' \itemDelete\url{http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Delete_(Data_Management)}
#' }
#'
#' @aliases rpygeo.EucDistance.sa rpygeo.Aspect.sa rpygeo.Slope.sa
#' rpygeo.Hillshade.sa rpygeo.Curvature.sa rpygeo.Delete.management
#' @param in.raster,in.data,out.raster,out.curvature.raster, Names of ArcGIS
#' raster or vector datasets or feature classes in a geodatabase (relative to
#' the current workspace defined in a \code{rpygeo.env} environment).
#' Shapefiles must include the extension \code{".shp"}.
#' @param out.profile.curve.raster,out.plan.curve.raster Names of ArcGIS raster
#' or vector datasets or feature classes in a geodatabase (relative to the
#' current workspace defined in a \code{rpygeo.env} environment).  Shapefiles
#' must include the extension \code{".shp"}.
#' @param env A list defining an RPyGeo working environment as built by
#' \code{rpygeo.build.env}.
#' @param maxdist,cellsize,out.direction.raster see ArcGIS online help
#' @param azimuth,altitude,model.shadows,z.factor see ArcGIS online help
#' @param unit,data.type Arguments to be passed to the Python geoprocessing
#' function. See ArcGIS help files for information on the usage of scripting
#' commands and their arguments.
#' @param \dots Additional arguments to be passed to
#' \code{\link{rpygeo.geoprocessor}}.
#' @return The function return \code{NULL} if no error occurred, otherwise a
#' character vector containing the error message.
#' @author Alexander Brenning
#' @seealso \code{\link{rpygeo.geoprocessor}}, \code{\link{rpygeo.build.env}}
#' @keywords interface database
#' @examples
#'
#' # Allow ArcGIS to overwrite existing datasets:
#' \dontrun{rpygeo.env$overwriteoutput = 1
#' # Calculate the slope of a DEM raster dataset
#' # in the current ArcGIS workspace:
#' rpygeo.geoprocessor("Slope_sa",c("dem","slope"))
#' # Same:
#' rpygeo.geoprocessor("Slope_sa('dem','slope')")
#' # Same, using the more convenient wrapper:
#' rpygeo.Slope.sa("dem","slope")}
#'
#' # Three at a time or separately:
#' \dontrun{date()
#' rpygeo.geoprocessor("Slope_sa('dem','slope')",
#'   "Aspect_sa('dem','aspect')", "Hillshade_sa('dem','hshd')")
#' date() # ~20 sec on my computer
#' rpygeo.Slope.sa("dem","slope")
#' rpygeo.Aspect.sa("dem","aspect")
#' rpygeo.Hillshade.sa("dem","hshd")
#' date() # ~50 sec
#' rpygeo.Delete.management("slope")
#' rpygeo.Delete.management("aspect")
#' rpygeo.Delete.management("hshd")}
#'
#' # Calculate the Euclidian distance from railway lines
#' # up to a max. distance of 1000 map units:
#' \dontrun{rpygeo.geoprocessor("EucDistance_sa",
#'     args=list("rail.shp","raildist",1000))
#' # Same:
#' rpygeo.EucDistance.sa("rail.shp","raildist",maxdist=1000)}
#'
#' # Use MapAlgebra to calculate a distance-decay function:
#' \dontrun{rpygeo.geoprocessor("SingleOutputMapAlgebra_sa",
#'     args=c("exp( raildist / -100 )","distdecay"))}
#'
#' # Or why not in just one step if you like MapAlgebra:
#' \dontrun{rpygeo.geoprocessor( "SingleOutputMapAlgebra_sa",
#'     args=c("exp( EucDistance( rail.shp, \#, \#, 1000 ) / -100 )","distdecay") )}
#'
NULL





#' Wrappers for functions from the Hydrology toolset
#'
#' Wrappers for selected geoprocessing tools from the ArcGIS Hydrology toolset
#' of the Spatial Analyst extension.
#'
#' These functions simply interface the behaviour of the ArcGIS/Python
#' geoprocessing functions with the equivalent names. See
#' \code{\link{rpygeo.geoprocessor}} for details on what happens behind the
#' scenes.
#'
#' ArcGIS 9.2 online help for the georpocessing tools can be accessed through
#' the following URLs: \itemize{
#' \itemFlowAccumulation\url{http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Flow_Accumulation}
#' \itemFlowDirection\url{http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Flow_Direction}
#' \itemFlowLength\url{http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Flow_Length}
#' \itemSink\url{http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Sink}
#' }
#'
#' @aliases rpygeo.FlowAccumulation.sa rpygeo.FlowDirection.sa
#' rpygeo.FlowLength.sa rpygeo.Sink.sa
#' @param in.flow.direction.raster,in.surface.raster,in.weight.raster, Names of
#' ArcGIS raster or vector datasets or feature classes in a geodatabase
#' (relative to the current workspace defined in a \code{rpygeo.env}
#' environment).  Shapefiles must include the extension \code{".shp"}.
#' @param out.accumulation.raster,out.flow.direction.raster, Names of ArcGIS
#' raster or vector datasets or feature classes in a geodatabase (relative to
#' the current workspace defined in a \code{rpygeo.env} environment).
#' Shapefiles must include the extension \code{".shp"}.
#' @param out.drop.raster,out.raster Names of ArcGIS raster or vector datasets
#' or feature classes in a geodatabase (relative to the current workspace
#' defined in a \code{rpygeo.env} environment).  Shapefiles must include the
#' extension \code{".shp"}.
#' @param data.type,direction.measurement Arguments to be passed to the Python
#' geoprocessing tool. See ArcGIS help files (link below) for information on
#' the usage of scripting commands and their arguments.
#' @param force.flow see ArcGIS help
#' @param \dots Additional arguments to be passed to
#' \code{\link{rpygeo.geoprocessor}}.
#' @return The functions return \code{NULL} if no error occurred, otherwise a
#' character vector containing the error message.
#' @author Alexander Brenning
#' @seealso \code{\link{rpygeo.geoprocessor}}, \code{\link{rpygeo.build.env}}
#' @keywords interface database
NULL





#' Wrapper for the Map Algebra tool
#'
#' Wrappers for the Single Output Map Algebra tool of the Spatial Analyst
#' extension.
#'
#' These functions simply interface the behaviour of the ArcGIS/Python
#' geoprocessing functions with the equivalent names. See
#' \code{\link{rpygeo.geoprocessor}} for details on what happens behind the
#' scenes.
#'
#' ArcGIS 9.2 online help for the georpocessing tools can be accessed through
#' the following URLs: \itemize{
#' \itemSingleOutputMapAlgebra\url{http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Single_Output_Map_Algebra}
#' }
#'
#' @aliases rpygeo.SingleOutputMapAlgebra.sa
#' @param in.data, Names of ArcGIS raster or vector datasets or feature classes
#' in a geodatabase (relative to the current workspace defined in a
#' \code{rpygeo.env} environment).  Shapefiles must include the extension
#' \code{".shp"}.
#' @param out.raster Names of ArcGIS raster or vector datasets or feature
#' classes in a geodatabase (relative to the current workspace defined in a
#' \code{rpygeo.env} environment).  Shapefiles must include the extension
#' \code{".shp"}.
#' @param expression.string Valid Map Algebra expression as described in the
#' ArcGIS help files (link below).
#' @param \dots Additional arguments to be passed to
#' \code{\link{rpygeo.geoprocessor}}.
#' @return The functions return \code{NULL} if no error occurred, otherwise a
#' character vector containing the error message.
#' @author Alexander Brenning
#' @seealso \code{\link{rpygeo.geoprocessor}}, \code{\link{rpygeo.build.env}}
#' @keywords interface database
NULL





#' Helper functions for RPyGeo
#'
#' Helper functions.
#'
#'
#' @aliases write.point.shapefile write.temp.point.shapefile
#' rpygeo.extent.to.character
#' @param d \code{data.frame} representing point data
#' @param file name of shapefile (WITHOUT file extension)
#' @param x.field,y.field names of attributes with x and y coordinates in
#' \code{d}
#' @param id.field (optional) name of attribute that serves as unique
#' identifier; will use values \code{1:nrow(d)} if not specified
#' @param pattern initial part of temporary shapefile name
#' @param tmpdir folder where temporary shapefiles should be stored
#' @param x list with components \code{x} and \code{y}, each a vector of length
#' 2, specifying lower and upper x/y limits.
#' @param \dots additional arguments for \code{write.point.shapefile}
#' @author Alexander Brenning
#' @seealso \code{\link{rpygeo.geoprocessor}}, \code{\link{rpygeo.build.env}},
#' \code{write.shapefile}
#' @keywords interface database
NULL



