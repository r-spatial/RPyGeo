# 2011-09-07
# - rpygeo.build.env and rpygeo.geoprocessor
#   snapraster now supported
# 2008-08-15
# - rpygeo.geoprocessor
#   - Now using os.chdir to set OS working directory in
#     Python prior to calling the geoprocessor; this seems to
#     help with some(!) relative paths in ArcGIS geoprocessor
#     calls...
#   - Mask did not work because of double double quotes;
#     now fixed
# - New functions rpygeo.ASCIIToRaster.conversion and
#   rpygeo.RasterToASCII.conversion
#   rpygeo.FlowDirection.sa, rpygeo.FlowAccumulation.sa,
#   rpygeo.FlowLength.sa, rpygeo.Sink.sa
# - New function rpygeo.Spline.sa, rpygeo.TopoToRaster.sa

# to do:
# - Adapt rpygeo.Delete.management to be able to delete multiple
#   files in one call (and to use Exist calls to prevent errors?).
# - Better: write a more flexible rpygeo.bundle function
#   that aggregates several calls using the same geoprocessing
#   environment!

library(RSAGA)
# TODO find out where RASAGE is used and import it correctly
# RPyGeo uses default.file.extension, match.arg.ext, get.file.extension
# functions from RSAGA package.

#*********************************************
# Helper functions --------------------------
#*********************************************

rpygeo.extent.to.character = function(x) {
    if (is.null(x)) return(x)
    if (is.na(x)) return(NULL)
    if (is.character(x)) return(x)
    x = paste(x$x[1], x$y[1], x$x[2], x$y[2], sep = " ")
}




#' RPyGeo Geoprocessing Environments
#'
#' Set up a geoprocessing environment for ArcGIS/Python scripting
#'
#' See ArcGIS documentation. This geoprocessing environment reflects only a
#' small fraction of the ArcGIS environment settings. Future releases of this
#' package may include more than the properties listed above.
#'
#' @aliases rpygeo.build.env rpygeo.env
#' @param modules (Do not modify!)  Name of Python module for ArcGIS
#' geoprocessing.
#' @param init (Do not modify!)  Python code for initializing the Python
#' geoprocessor.
#' @param workspace Path of ArcGIS workspace (or name of geodatabase) in which
#' to perform the geoprocessing.
#' @param cellsize Default cellsize (default: maximum(?) of inputs - see ArcGIS
#' documentation).
#' @param extent,mask,snapraster Optional datasets or character strings
#' defining the analysis extent and mask and what to snap to - see ArcGIS
#' documentation.
#' @param overwriteoutput Overwrite existing ArcGIS datasets (\code{=1}) or not
#' (\code{=0} - default)?
#' @param extensions Names of extensions to be used in geoprocessing; it is
#' usually not necessary to specify this here.  Possible values:
#' \code{"Spatial","3d","geostats","network", "datainteroperability"}.
#' @param python.path Where to find the Python interpreter (depends on Python
#' version).
#' @param python.command Name of the Python command line interpreter
#' executable.
#' @return A list whose components are exactly the arguments passed to the
#' \code{rpygeo.build.env} function.
#' @author Alexander Brenning
#' @seealso \code{\link{rpygeo.geoprocessor}}
#' @keywords interface database
#' @examples
#'
#' # Everything in this workspace will be masked with DEM extent
#' # and have a cellsize of 100m:
#' \dontrun{env.lo <- rpygeo.build.env( mask="clip", cellsize=100 )}
#' # and this is for high-resolution output:
#' \dontrun{env.hi <- rpygeo.build.env( mask="clip", cellsize=1 )}
#'
#' # Slope from different DEMs at different target resolutions
#' # (which may be different from the original DEM resolution):
#' \dontrun{rpygeo.Slope.sa("srtm-dem","slope-lo",env=env.lo)}
#' \dontrun{rpygeo.Slope.sa("laser-dem","slope-hi",env=env.hi)}
#'
#' @export rpygeo.build.env
#'
#'
rpygeo.build.env = function(
    modules = "arcgisscripting",
    init = "gp = arcgisscripting.create()",
    workspace = NULL,
    cellsize = NULL,
    extent = NULL,
    mask = NULL,
    snapraster = NULL,
    overwriteoutput = 0,
    extensions = NULL,
    python.path = "C:\\software\\Python27",
    python.command = "python.exe" )
{
    return( list(
        modules = modules,
        init = init,
        workspace = workspace,
        cellsize = cellsize,
        extent = rpygeo.extent.to.character(extent),
        mask = mask,
        snapraster = snapraster,
        overwriteoutput = overwriteoutput,
        extensions = extensions,
        python.path = python.path,
        python.command = python.command
    ) )
}

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
write.point.shapefile = function(d, file, x.field = "x", y.field = "y", id.field = NULL)
{
    # Prepare data for shapefile - see example in ?write.shapefile
    # in package shapefiles:
    if (is.null(id.field)) {
        id.field = "id"
        create.id.field = TRUE
    } else if (all(colnames(d) != id.field))
        create.id.field = TRUE

    if (create.id.field)
        d[[id.field]] = c(1:nrow(d))
    dd = data.frame( id = d[,id.field],
                        x = d[,x.field],
                        y = d[,y.field] )
    ddShapefile = shapefiles::convert.to.shapefile(dd, d, id.field, 1)

    # Write shapefile:
    shapefiles::write.shapefile(ddShapefile, file, arcgis = TRUE)
}

write.temp.point.shapefile = function(d, pattern = "file",
    tmpdir = tempdir(), ...)
{

    # Write shapefile:
    tmpfile = tempfile(pattern = pattern, tmpdir = tmpdir)
    tmpfile = gsub("\\", "/", tmpfile, fixed = TRUE)
    write.point.shapefile(d, file = tmpfile, ...)

    # Create an expression that will allow the caller to
    # delete the temporary shapefile:
    exit.expr = bquote( {
        unlink(.(paste(tmpfile,".shp",sep="")));
        unlink(.(paste(tmpfile,".shx",sep="")));
        unlink(.(paste(tmpfile,".dbf",sep=""))) } )
    tmpfile = paste(tmpfile, ".shp", sep="")

    return( list( tempfile = tmpfile, exit.expression = exit.expr ) )
}



#*********************************************
# Geoprocessing environment -----------------
#*********************************************


# TODO why was this commented out?
rpygeo.env = rpygeo.build.env()

rpygeo.env = list(
    modules = "arcgisscripting",
    init = "gp = arcgisscripting.create()",
    workspace = NULL,
    cellsize = NULL,
    extent = NULL,
    mask = NULL,
    overwriteoutput = 0,
    extensions = NULL,
    python.path = "C:\\software\\Python27",
    python.command = "python.exe" )




#' Check required ArcGIS extensions
#'
#' Internal function that checks which ArcGIS extensions have to be enabled to
#' evaluate a Python expression.
#'
#'
#' @param expr A vector or list of character strings with Python geoprocessing
#' expressions or function names.
#' @return Returns a character vector with the ArcGIS extension names
#' (currently e.g. "Spatial", "3d", "geostats", "network", and/or
#' "datainteroperability").
#' @note This internal function is used by \code{rpygeo.geoprocessor}.
#' @author Alexander Brenning
#' @seealso \code{\link{rpygeo.geoprocessor}}
#' @keywords interface database
#' @export rpygeo.required.extensions
rpygeo.required.extensions = function(expr) {
    # See ArcGIS help on the CheckOutExtension method:
    rpygeo.match.extensions = c("sa","3d","stats","na","di")
    names(rpygeo.match.extensions) = c("Spatial","3d","geostats","network","datainteroperability")
    ext = c()
    for (s in expr) {
        sub.s = strsplit(s,"(",fixed=TRUE)[[1]]
        for (t in sub.s) {
            t = gsub(" ","",t)
            if (substring(t,nchar(t)) == ")")  next
            for (i in 1:length(rpygeo.match.extensions)) {
                the.match = paste("_",tolower(rpygeo.match.extensions[i]),sep="")
                if ( tolower(substring(t,nchar(t)+1-nchar(the.match))) == the.match )
                    ext = c( ext, names(rpygeo.match.extensions)[i] )
            }
        }
    }
    return(unique(ext))
}



#*********************************************
# RPyGeo Geoprocessor - the workhorse --------
#*********************************************



#' ArcGIS Geoprocessor Workhorse
#'
#' This function creates a Python geoprocessing script file and runs it from
#' the operating system using the ArcGIS Geoprocessor.
#'
#' This function is the R geoprocessing workhorse that creates a Python
#' geoprocessing script, runs it, and returns any error messages.
#'
#' If \code{fun} is a ready-to-use Python expression such as \code{}, then
#' \code{add.gp} only determines whether the \code{"gp."} has to be added as a
#' prefix to access the Python geoprocessor or not.
#'
#' In most cases however, \code{fun} will be a single ArcGIS geoprocessing
#' script function such as \code{"Slope_sa"}, where \code{"_sa"} tells us that
#' this function can be found in the Spatial Analyst extension of ArcGIS
#' (\code{rpygeo.required.extensions} will check this for you if the
#' \code{detected...} argument is \code{TRUE}) Now \code{args} will be a vector
#' or list of arguments to \code{Slope_sa}, e.g. \code{c("dem","slope")} or
#' \code{list("dem","slope","PERCENT_RISE",2)} (see ArcGIS help files for
#' information on the arguments of \code{Slope_sa}).  These will result in
#' Python expressions \code{gp.Slope_sa("dem", "slope")} and
#' \code{gp.Slope_sa("dem", "slope", "PERCENT_RISE", 2)} if \code{add.gp==TRUE}
#' and if we use the \code{quote.args} arguments \code{TRUE} and
#' \code{c(T,T,T,F)}, respectively.
#'
#' Dataset names will always be relative to the path or geodatabase defined in
#' the geoprocessing environment settings \code{env$workspace}.  Also, ArcGIS
#' will be allowed to overwrite any existing output files
#' (\code{env$overwriteoutput==1}) or not (\code{==0}).  See
#' \code{\link{rpygeo.build.env}} for details.
#'
#' @param fun This can be either a complete Python geoprocessing command (see
#' examples), a single geoprocessing function name, or a vector of function or
#' Python expressions to be evaluated by the Python geoprocessor.
#' @param args Vector or list of arguments to be passed to the function listed
#' in \code{fun}. The argument \code{quote.args} determines whether these
#' arguments will be decorated with quotation marks.
#' @param py.file Name of the temporary Python script file (in the
#' \code{working.directory}).
#' @param msg.file Name of the temporary file in which to dump Python/ArcGIS
#' error messages (in the \code{working.directory}).
#' @param env A list defining the RPyGeo working environment.  Defaults to the
#' standard working environment \code{rpygeo.env}, which is created at
#' start-up.  See \code{\link{rpygeo.build.env}} for details.
#' @param extensions Optional character vector listing ArcGIS extension that
#' should be enabled before using the geoprocessing \code{fun}ction. This adds
#' to any extensions that are listed in the \code{env}ironment or eventually
#' detected by \code{rpygeo.required.extensions}.
#' @param working.directory The working directory for temporary files (i.e. the
#' Python script and error message files); defaults to R's current working
#' directory.
#' @param quote.args Logical value (default: \code{TRUE}) or logical vector
#' that determines whether quotation marks have to be added to the \code{args}
#' arguments before passing them to Python. If this is a vector, it must have
#' the same length as \code{args}. See Details.
#' @param add.gp Logical (default: \code{TRUE}). See Details.
#' @param wait Logical (default: \code{TRUE}). Experimental(!)  option. If
#' \code{FALSE} (NOT recommended), do not wait for the operating system /
#' ArcGIS to finish the Python geoprocessing script.
#' @param clean.up Logical (default \code{TRUE}) or character vector
#' (\code{"msg"}, \code{"py"}, or \code{c("msg","py")}).  Determines whether
#' the error message file, the Python script file, or both (default) should be
#' deleted after geoprocessing is finished. Ignored if \code{wait} is
#' \code{FALSE}.
#' @param detect.required.extensions Logical (default: \code{TRUE}).
#' Determines whether \code{rpygeo.required.extensions} should try to find out
#' which ArcGIS extensions are required to evaluate the \code{fun}ction(s).
#' @return The function returns \code{NULL} if is was successful, or otherwise
#' a character vector with the ArcGIS error message.  In addition, the ArcGIS
#' function will generate the output described in the ArcGIS help files etc.
#' Depending on the \code{clean.up} argument, the Python code may still be
#' available in the \code{py.file}, and error messages in \code{msg.file}.
#' @note The Python script created by this geoprocessor is loaded with
#' initialization code for setting up the ArcGIS workspace and enabling ArcGIS
#' extensions.  This makes this function pretty inefficient, but you save a lot
#' of time because you don't have to switch between three applications and two
#' programming languages...
#'
#' ArcGIS is pretty flexible with respect to numeric arguments such as the z
#' factor in \code{Slope_sa} being passed as character string.  As a
#' consequence, \code{quote.args=TRUE} will normally work fine.
#'
#' \code{wait==FALSE} is experimental and not recommended. Watch for file name
#' conflicts if you really want to try it - competing geoprocessing scripts
#' must use different temporary Python script files etc.
#' @author Alexander Brenning
#' @seealso \code{\link{rpygeo.build.env}}
#' @keywords interface database
#' @examples
#'
#' # Allow ArcGIS to overwrite existing datasets:
#' \dontrun{rpygeo.env$overwriteoutput = 1}
#' # Calculate the slope of a DEM raster dataset
#' # in the current ArcGIS workspace:
#' \dontrun{rpygeo.geoprocessor("Slope_sa",c("dem","slope"))}
#' # Same:
#' \dontrun{rpygeo.geoprocessor("Slope_sa('dem','slope')")}
#' # Same, using the more convenient wrapper:
#' \dontrun{rpygeo.Slope.sa("dem","slope")}
#'
#' # Three at a time or separately:
#' \dontrun{date()}
#' \dontrun{rpygeo.geoprocessor("Slope_sa('dem','slope')",
#'   "Aspect_sa('dem','aspect')", "Hillshade_sa('dem','hshd')")}
#' \dontrun{date()} # ~20 sec on my computer
#' \dontrun{rpygeo.Slope.sa("dem","slope")}
#' \dontrun{rpygeo.Aspect.sa("dem","aspect")}
#' \dontrun{rpygeo.Hillshade.sa("dem","hshd")}
#' \dontrun{date()} # ~50 sec
#' \dontrun{rpygeo.Delete.management("slope")}
#' \dontrun{rpygeo.Delete.management("aspect")}
#' \dontrun{rpygeo.Delete.management("hshd")}
#'
#' # Calculate the Euclidian distance from railway lines
#' # up to a max. distance of 1000 map units:
#' \dontrun{rpygeo.geoprocessor("EucDistance_sa",
#'     args=list("rail.shp","raildist",1000))}
#' # Same:
#' \dontrun{rpygeo.EucDistance.sa("rail.shp","raildist",maxdist=1000)}
#'
#' # Use MapAlgebra to calculate a distance-decay function:
#' \dontrun{rpygeo.geoprocessor("SingleOutputMapAlgebra_sa",
#'     args=c("exp( raildist / -100 )","distdecay"))}
#'
#' # Or why not in just one step if you like MapAlgebra:
#' \dontrun{rpygeo.geoprocessor( "SingleOutputMapAlgebra_sa",
#'     args=c("exp( EucDistance( rail.shp, \#, \#, 1000 ) / -100 )","distdecay") )}
#'
#' @export rpygeo.geoprocessor
rpygeo.geoprocessor = function(
  fun, args=NULL,
  py.file="rpygeo.py", msg.file="rpygeo.msg",
  env = rpygeo.env, extensions = NULL, working.directory = getwd(),
  quote.args = TRUE, add.gp = TRUE, wait = TRUE,
  clean.up = wait,
  detect.required.extensions = TRUE )
{
  if (is.logical(clean.up)) {
    if (clean.up) {
      clean.up = c("py","msg")
    } else
      clean.up = c()
  }

  # Convert to character string,
  # and add quotation marks if input was already a string:
  convert = function(x) {
    if (is.numeric(x)) {
      return( as.character(x) )
    } else
      return( paste('"', x, '"', sep="" ) )
  }
  # Consistent indentation is important in Python:
  indent = "    "

  # Expecting a list of arguments, not a vector,
  # because arguments may have different data types:
  if (is.vector(args)) args = as.list(args)
  if ((length(fun) > 1)  & (!is.null(args))) {
    warning("Multiple function calls only allowed if args is NULL. Using only first `fun' element.\n")
    fun = fun[1]
  }
  if (!is.null(args)) if (length(quote.args)==1) quote.args = rep(quote.args,length(args))

  # Create list of required ArcGIS extensions:
  extensions = c( env$extensions, extensions )
  if (detect.required.extensions)
    extensions = c( extensions, rpygeo.required.extensions(fun) )
  extensions = unique( extensions )

  # Have to distinguish between an R version and a Windows version of file names.
  to.windows.filename = function(x) gsub("/","\\",x,fixed=TRUE)
  to.R.filename       = function(x) gsub("\\","/",x,fixed=TRUE)
  py.file = to.windows.filename( paste(working.directory,"/",py.file,sep="") )
  R.py.file = to.R.filename(py.file)
  msg.file = to.windows.filename( paste(working.directory,"/",msg.file,sep="") )
  R.msg.file = to.R.filename(msg.file)

  #*******************************************
  # Build the Python geoprocessing script:
  expr = ""
  # Added 2008-08-15
  # This may help with some relative paths in ArcGIS:
  if (!is.null(env$workspace)) {
    expr = paste( expr, "import os\n", sep="" )
    expr = paste( expr, "os.chdir(", convert(to.R.filename(env$workspace)), ")\n", sep="" )
  }
  # End added
  for (mod in env$modules)
    expr = paste( expr, "import ", mod, "\n", sep="" )
  for (init in env$init)
    expr = paste( expr, init, "\n", sep="" )
  if (!is.null(env$workspace))
    expr = paste( expr, "gp.Workspace = ", convert(to.R.filename(env$workspace)), "\n", sep="" )
  if (!is.null(env$cellsize))
    expr = paste( expr, "gp.Cellsize = ", convert(env$cellsize), "\n", sep="" )
  if (!is.null(env$extent))
    expr = paste( expr, "gp.Extent = ", convert(env$extent), "\n", sep="" )
  if (!is.null(env$mask))
    expr = paste( expr, "gp.Mask = ", convert(env$mask), "\n", sep="" )
  if (!is.null(env$snapraster))
    expr = paste( expr, "gp.snapRaster = ", convert(env$snapraster), "\n", sep="" )
  if (!is.null(env$overwriteoutput))
    expr = paste( expr, "gp.Overwriteoutput = ", convert(env$overwriteoutput), "\n", sep="" )
  if (!is.null(extensions))
    for (ext in extensions)
      expr = paste( expr, "gp.CheckOutExtension(", convert(ext), ")\n", sep="" )
  expr = paste( expr, 'rpygeoresult = ""', "\n", sep="" )
  expr = paste( expr, "\n", sep="" )

  expr = paste( expr, "try:\n", sep="" )

  if (is.null(args)) {
    # Simplest case - each element of `fun' is a complete Python expression:
    for (the.fun in fun)
      expr = paste( expr, indent, ifelse(add.gp,"gp.",""), the.fun, "\n", sep="" )
  } else {
    # More complicated:
    # Only one `fun' call, but many arguments need to be concatenated:
    expr = paste( expr, indent, ifelse(add.gp,"gp.",""), fun, "( ", sep="" )
    for (i.arg in 1:length(args)) {
      if (i.arg > 1)  expr = paste( expr, ", ", sep="" )
      # Character string arguments will usually have to be decorated with quotes,
      # assuming that they are string constants, not variable names or expressions:
      expr = paste( expr,
                    ifelse(quote.args[i.arg], convert(args[[i.arg]]), as.character(args[[i.arg]])),
                    sep="" )
      # to do: use intelligent line breaks in expressions??
    }
    expr = paste( expr, " )\n", sep="" )
  }

  # Catch exceptions: get error messages
  expr = paste( expr, "except:\n", sep="" )
  expr = paste( expr, indent, "rpygeoresult = gp.GetMessages()\n", sep="" )

  #expr = paste( expr, "\n", sep="")

  # If an error occurred, write the error message to the `msg.file':
  expr = paste( expr, 'if rpygeoresult != "":\n', sep="")
  expr = paste( expr, indent, 'f = open("', R.msg.file, '", "w")\n', sep="")
  expr = paste( expr, indent, "f.write(rpygeoresult)\n", sep="")
  expr = paste( expr, indent, "f.close()\n", sep="")
  #**************************************************


  # Write the Python geoprocessing script to the `py.file':
  py.script = file( R.py.file, open="wt" )
  write(expr, file=py.script)
  close(py.script)
  rm(py.script)

  # Delete message file;
  # otherwise msg file would only be overwritten if an geoprocessing error occurred.
  if (file.exists(R.msg.file))
    unlink(R.msg.file)

  # Set up the system call expression:
  py.call = ""
  if (!is.null(env$python.path))
    py.call = paste( py.call, env$python.path, "\\", sep="" )
  py.call = paste( py.call, env$python.command, " ", py.file, sep="" )
  py.call = to.windows.filename(py.call)

  ######## Run Python:
  system(py.call, invisible = TRUE, minimize = TRUE, wait = wait)

  # Read error messages from the `msg.file', if available:
  res = NULL
  if (file.exists(R.msg.file) & wait) {
    f.msg = file(R.msg.file,"rt")
    res = readLines(f.msg, warn=FALSE)
    close(f.msg)
    if ("msg" %in% clean.up)
      unlink(R.msg.file)
  }

  # Delete the `py.file' script:
  if ("py" %in% clean.up)
    if (file.exists(R.py.file))
      unlink(R.py.file)

  # Return error message or NULL:
  return(res)
}


#'
#'
#' #*********************************************
#' # Spatial Analyst tools ---------------------
#' #*********************************************
#'
#' # http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Hillshade
rpygeo.Hillshade.sa = function( in.raster, out.raster,
    azimuth = 315, altitude = 45,
    model.shadows = c("NO_SHADOWS","SHADOWS"), z.factor = 1, ...)
{
    model.shadows = match.arg.ext(model.shadows, ignore.case = TRUE)

    rpygeo.geoprocessor( fun="Hillshade_sa",
        args=list(in.raster,out.raster,azimuth,altitude,model.shadows,z.factor),
        quote.args=c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE), ... )
}

rpygeo.Slope.sa = function( in.raster, out.raster,
    unit = c("DEGREE","PERCENT_RISE"), z.factor = 1, ... )
{
    unit = match.arg.ext(unit, ignore.case = TRUE)

    rpygeo.geoprocessor( fun="Slope_sa",
        args=list(in.raster, out.raster, unit, z.factor),
        quote.args=c(TRUE,TRUE,TRUE,FALSE), ... )
}

#' # http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Aspect
#' rpygeo.Aspect.sa = function( in.raster, out.raster, ... )
#' {
#'     rpygeo.geoprocessor( fun = "Aspect_sa",
#'         args = list(in.raster, out.raster), ... )
#' }
#'
#'
#'
#'
#' #' Wrappers for selected ArcGIS functions
#' #'
#' #' Wrappers for a small selection of ArcGIS geoprocessing functions based on
#' #' the \code{rpygeo.geoprocessor}.
#' #'
#' #' These functions simply try to replicate the behaviour of the ArcGIS/Python
#' #' geoprocessing functions of the same name. See
#' #' \code{\link{rpygeo.geoprocessor}} for details on what happens behind the
#' #' scenes.
#' #'
#' #' ArcGIS 9.2 online help for the georpocessing tools can be accessed through
#' #' the following URLs: \itemize{
#' #' \item EucDistance \url{http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=EucDistance}
#' #' \item Hillshade \url{http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Hillshade}
#' #' \item Slope \url{http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Slope}
#' #' \item Aspect \url{http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Aspect}
#' #' \item Curvature \url{http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Curvature}
#' #' \item Delete \url{http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Delete_(Data_Management)}
#' #' }
#' #'
#' #' @aliases rpygeo.EucDistance.sa rpygeo.Aspect.sa rpygeo.Slope.sa
#' #' rpygeo.Hillshade.sa rpygeo.Curvature.sa rpygeo.Delete.management
#' #' @param in.raster,in.data,out.raster,out.curvature.raster, Names of ArcGIS
#' #' raster or vector datasets or feature classes in a geodatabase (relative to
#' #' the current workspace defined in a \code{rpygeo.env} environment).
#' #' Shapefiles must include the extension \code{".shp"}.
#' #' @param out.profile.curve.raster,out.plan.curve.raster Names of ArcGIS raster
#' #' or vector datasets or feature classes in a geodatabase (relative to the
#' #' current workspace defined in a \code{rpygeo.env} environment).  Shapefiles
#' #' must include the extension \code{".shp"}.
#' #' @param env A list defining an RPyGeo working environment as built by
#' #' \code{rpygeo.build.env}.
#' #' @param maxdist,cellsize,out.direction.raster see ArcGIS online help
#' #' @param azimuth,altitude,model.shadows,z.factor see ArcGIS online help
#' #' @param unit,data.type Arguments to be passed to the Python geoprocessing
#' #' function. See ArcGIS help files for information on the usage of scripting
#' #' commands and their arguments.
#' #' @param \dots Additional arguments to be passed to
#' #' \code{\link{rpygeo.geoprocessor}}.
#' #' @return The function return \code{NULL} if no error occurred, otherwise a
#' #' character vector containing the error message.
#' #' @author Alexander Brenning
#' #' @seealso \code{\link{rpygeo.geoprocessor}}, \code{\link{rpygeo.build.env}}
#' #' @keywords interface database
#' #' @examples
#' #'
#' #' # Allow ArcGIS to overwrite existing datasets:
#' #' \dontrun{rpygeo.env$overwriteoutput = 1
#' #' # Calculate the slope of a DEM raster dataset
#' #' # in the current ArcGIS workspace:
#' #' rpygeo.geoprocessor("Slope_sa",c("dem","slope"))
#' #' # Same:
#' #' rpygeo.geoprocessor("Slope_sa('dem','slope')")
#' #' # Same, using the more convenient wrapper:
#' #' rpygeo.Slope.sa("dem","slope")}
#' #'
#' #' # Three at a time or separately:
#' #' \dontrun{date()
#' #' rpygeo.geoprocessor("Slope_sa('dem','slope')",
#' #'   "Aspect_sa('dem','aspect')", "Hillshade_sa('dem','hshd')")
#' #' date() # ~20 sec on my computer
#' #' rpygeo.Slope.sa("dem","slope")
#' #' rpygeo.Aspect.sa("dem","aspect")
#' #' rpygeo.Hillshade.sa("dem","hshd")
#' #' date() # ~50 sec
#' #' rpygeo.Delete.management("slope")
#' #' rpygeo.Delete.management("aspect")
#' #' rpygeo.Delete.management("hshd")}
#' #'
#' #' # Calculate the Euclidian distance from railway lines
#' #' # up to a max. distance of 1000 map units:
#' #' \dontrun{rpygeo.geoprocessor("EucDistance_sa",
#' #'     args=list("rail.shp","raildist",1000))
#' #' # Same:
#' #' rpygeo.EucDistance.sa("rail.shp","raildist",maxdist=1000)}
#' #'
#' #' # Use MapAlgebra to calculate a distance-decay function:
#' #' \dontrun{rpygeo.geoprocessor("SingleOutputMapAlgebra_sa",
#' #'     args=c("exp( raildist / -100 )","distdecay"))}
#' #'
#' #' # Or why not in just one step if you like MapAlgebra:
#' #' \dontrun{rpygeo.geoprocessor( "SingleOutputMapAlgebra_sa",
#' #'     args=c("exp( EucDistance( rail.shp, \#, \#, 1000 ) / -100 )","distdecay") )}
#' #'
#'
#' # http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=EucDistance
#' rpygeo.EucDistance.sa = function( in.data, out.raster,
#'     maxdist=NULL, cellsize=NULL, out.direction.raster=NULL,
#'     env = rpygeo.env, ... )
#' {
#'     if (!is.null(maxdist)) if (maxdist==Inf) maxdist = NULL
#'     args = list(in.data, out.raster)
#'     args = c(args, maxdist, cellsize, out.direction.raster)
#'     rpygeo.geoprocessor( fun = "EucDistance_sa",
#'         args=args, ... )
#' }
#'
#'
#' # http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=EucDistance
#' # http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Curvature
#' rpygeo.Curvature.sa = function(in.raster, out.curvature.raster,
#'     z.factor = 1, out.profile.curve.raster = NULL,
#'     out.plan.curve.raster = NULL, ...)
#' {
#'     args = list(in.raster, out.curvature.raster, z.factor,
#'         out.profile.curve.raster, out.plan.curve.raster)
#'
#'     rpygeo.geoprocessor(fun = "Curvature_sa", args = args, ...)
#' }
#'
#' # http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Determining_flow_direction
#' rpygeo.FlowDirection.sa = function(in.surface.raster,
#'     out.flow.direction.raster, force.flow = c("NORMAL","FORCE"),
#'     out.drop.raster = NULL, ...)
#' {
#'     force.flow = match.arg.ext(force.flow, ignore.case = TRUE)
#'     args = list(in.surface.raster, out.flow.direction.raster,
#'         force.flow, out.drop.raster)
#'     rpygeo.geoprocessor( fun = "FlowDirection_sa", args = args, ...)
#' }
#'
#'
#'
#' # http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Calculating_flow_accumulation
#' rpygeo.FlowAccumulation.sa = function(in.flow.direction.raster,
#'     out.accumulation.raster, in.weight.raster = NULL,
#'     data.type = c("FLOAT","INTEGER"), ...)
#' {
#'     data.type = match.arg.ext(data.type, ignore.case = TRUE)
#'     args = list(in.flow.direction.raster, out.accumulation.raster,
#'         in.weight.raster, data.type)
#'     rpygeo.geoprocessor( fun = "FlowAccumulation_sa", args = args, ...)
#' }
#'
#' rpygeo.FlowLength.sa = function(in.flow.direction.raster,
#'     out.raster, direction.measurement = c("DOWNSTREAM","UPSTREAM"),
#'     in.weight.raster = NULL, ...)
#' {
#'     direction.measurement = match.arg.ext(direction.measurement, ignore.case = TRUE)
#'     args = list(in.flow.direction.raster, out.raster,
#'         direction.measurement, in.weight.raster)
#'     rpygeo.geoprocessor( fun = "FlowLength_sa", args = args, ...)
#' }
#'
#' rpygeo.Sink.sa = function(in.flow.direction.raster, out.raster, ...)
#' {
#'     args = list(in.flow.direction.raster, out.raster)
#'     rpygeo.geoprocessor( fun = "Sink_sa", args = args, ...)
#' }
#'
#'
#' # http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Viewshed
#' rpygeo.Viewshed.sa = function(in.raster, in.observer.features,
#'     out.raster, z.factor = 1,
#'     curvature.correction = c("FLAT_EARTH", "CURVED_EARTH"),
#'     refractivity.coefficient = 0.13,
#'     x.field = "x", y.field = "y", tmpdir = tempdir(), ...)
#' {
#'     if (is.data.frame(in.observer.features)) {
#'         res = write.temp.point.shapefile(in.observer.features, x.field = x.field, y.field = y.field,
#'             pattern = "rpygeo", tmpdir = tmpdir)
#'         in.observer.features = res$tempfile
#'         on.exit(res$exit.expression)
#'     }
#'     curvature.correction = match.arg.ext(curvature.correction, ignore.case = TRUE)
#'     args = list(in.raster, in.observer.features, out.raster, z.factor,
#'             curvature.correction, refractivity.coefficient)
#'     rpygeo.geoprocessor("Viewshed_sa", args = args, ...)
#' }
#'
#' # http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Area_Solar_Radiation
#' # http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Solar%20radiation%20analysis%20equations
#' rpygeo.AreaSolarRadiation.sa = function(in.surface.raster,
#'         out.global.radiation.raster, latitude = 45, sky.size = 200,
#'         time.configuration, day.interval = 14, hour.interval = 0.5,
#'         each.interval = c("NOINTERVAL", "INTERVAL"),
#'         z.factor = NULL, slope.aspect.input.type = c("FROM_DEM", "FLAT_SURFACE"),
#'         calculation.directions = 32, zenith.divisions = 8,
#'         azimuth.divisions = 8,
#'         diffuse.model.type = c("UNIFORM_SKY", "STANDARD_OVERCAST_SKY"),
#'         diffuse.proportion = 0.3, transmittivity = 0.5,
#'         out.direct.radiation.raster = NULL, out.diffuse.radiation.raster = NULL,
#'         out.direct.duration.raster = NULL, ...)
#' {
#'     if (is.list(time.configuration)) {
#'         if (time.configuration[[1]] == "WithinDay") {
#'             stopifnot(time.configuration[[2]] > 0 & time.configuration[[2]] <= 366)
#'             stopifnot(time.configuration[[3]] >= 0)
#'             stopifnot(time.configuration[[4]] >= 0)
#'             stopifnot(time.configuration[[3]] <= 24)
#'             stopifnot(time.configuration[[4]] <= 24)
#'             stopifnot(time.configuration[[3]] < time.configuration[[4]])
#'         } else if (time.configuration[[1]] == "Year") {
#'             if (length(time.configuration) > 1)
#'                 warning("calculating solar radiation for whole year\nignoring additional parameters in 'time.configuration'")
#'             time.configuration = "Year"
#'         }
#'         time.configuration = paste(time.configuration, collapse = " ")
#'     }
#'
#'     args = list(in.surface.raster, out.global.radiation.raster, latitude,
#'                 sky.size, time.configuration, day.interval, hour.interval,
#'                 each.interval, z.factor, slope.aspect.input.type,
#'                 calculation.directions, zenith.divisions, azimuth.divisions,
#'                 diffuse.model.type, diffuse.proportion, transmittivity,
#'                 out.direct.radiation.raster, out.diffuse.radiation.raster,
#'                 out.direct.duration.raster)
#'
#'     rpygeo.geoprocessor( fun = "AreaSolarRadiation_sa", args = args, ...)
#' }
#'
#'
#'
#' # http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Single_Output_Map_Algebra
#' rpygeo.SingleOutputMapAlgebra.sa = function(expression.string, out.raster,
#'     in.data = NULL, ...)
#' {
#'     args = list(expression.string, out.raster)
#'     if (!is.null(in.data)) args = c(args, in.data)
#'     rpygeo.geoprocessor(fun = "SingleOutputMapAlgebra_sa",
#'         args = args, ...)
#' }
#'
#'
#' #*********************************************
#' # Data Management functions ------------------
#' #*********************************************
#'
#' # http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Delete_(Data_Management)
#' rpygeo.Delete.management = function( in.data, data.type = NULL, ... )
#' {
#'     rpygeo.geoprocessor( fun = "Delete_management",
#'         args = c(in.data, data.type), ... )
#' }
#'
#'
#'
#'
#'
#' #*********************************************
#' # Conversion functions ----------------------
#' #*********************************************
#'
#'
#' # http://webhelp.esri.com/arcgisdesktop/9.2/body.cfm?tocVisable=1&ID=1309&TopicName=ASCII%20to%20Raster%20(Conversion)
#' rpygeo.ASCIIToRaster.conversion = function( in.ascii.file, out.raster,
#'     data.type = c("FLOAT","INTEGER"), ... )
#' {
#'     in.ascii.file = default.file.extension(in.ascii.file, ".asc")
#'     if (!(tolower(get.file.extension(in.ascii.file)) %in% c(".asc",".txt")))
#'         stop("'in.ascii.file' must have extension '.asc' or '.txt'.\n")
#'     data.type = match.arg.ext(data.type, ignore.case = TRUE)
#'     args = list(in.ascii.file, out.raster, data.type)
#'     rpygeo.geoprocessor( fun = "ASCIIToRaster_conversion",
#'         args = args, ... )
#' }
#'
#' # http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Raster%20to%20ASCII%20(Conversion)
#' rpygeo.RasterToASCII.conversion = function( in.raster, out.ascii.file, ... )
#' {
#'     out.ascii.file = default.file.extension(out.ascii.file, ".asc")
#'     if (!(tolower(get.file.extension(out.ascii.file)) %in% c(".asc",".txt")))
#'         stop("'out.ascii.file' must have extension '.asc' or '.txt'.\n")
#'     args = list(in.raster, out.ascii.file)
#'     rpygeo.geoprocessor( fun = "RasterToASCII_conversion",
#'         args = args, ... )
#' }
