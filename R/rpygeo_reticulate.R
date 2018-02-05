#' @title  Initialize Arcpy module in R
#'
#' @description Initialises the Python "arcpy" site-package in R with the help
#'   of reticulate
#'
#'
#' @param path Root path to the Python version which contains "arcpy". If left
#' empty, the function
#' looks for `python.exe` in the most likely location (C:\Python27\)
#'
#' @return Returns arcpy module in R
#' @author Fabian Polakowski
#' @export
#'
#'


# TODO add workspace
# TODO add parameters such as overwrite or cellsize or extensions
# to build_env or to geoprocessor function? -> both
# TODO add option to load ArcGIS Pro arcpy version

rpygeo_build_env <- function(path = NULL,
                             overwrite = TRUE,
                             extensions = NULL,
                             pro = FALSE) {

  # set path
  # TODO check if it is really a arcpy python
  if (is.null(path)) {
    if (pro) {
      dirs <- list.files(
        path = "C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3",
        pattern = "python.exe", recursive = TRUE, full.names = TRUE
      )
    }

    if (!pro) {
      dirs <- list.files(
        path = "C:/Python27", pattern = "python.exe", recursive = TRUE,
        full.names = TRUE
      )
    }

    if (length(dirs) == 1) {
      path <- dirs
    }

    if (length(dirs) > 1) {
      stop("multiple paths found, define ArcGIS Path\n")
    }

    if (length(dirs) < 1) {
      stop("No python function found in 'C:/Python27' - please define python path\n")
    }
  }
  if (!is.null(path)) {
    path == path
    # TODO check if path is correct
  }

  # init
  use_python(python = path, required = TRUE)
  import("arcpy")

  # handle initial parameters
  input_check(overwrite = overwrite, extensions = extensions)


  return(import("arcpy"))

}


#' @title ArcGIS Geoprocessor Workhorse
#'
#' @description This function utilzes the arcpy site-package in R via the reticulate
#' connection to perform a arcpy
#' calculation in R. It returns error messages if an error appears.
#'
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
#'
#' @param lib arcpy library name saved with \code{arcpy}
#' @param fun This can be either a complete Python geoprocessing command (see
#' examples), a single geoprocessing function name, or a vector of function or
#' Python expressions to be evaluated by the Python geoprocessor.
#' @param args Vector or list of arguments to be passed to the function listed
#' in \code{fun}. The argument \code{quote.args} determines whether these
#' arguments will be decorated with quotation marks.
#' @param env A list defining the RPyGeo working environment.  Defaults to the
#' standard working environment \code{rpygeo.env}, which is created at
#' start-up.  See \code{\link{rpygeo.build.env}} for details.
#' @param extensions Optional character vector listing ArcGIS extension that
#' should be enabled before using the geoprocessing \code{fun}ction. This adds
#' to any extensions that are listed in the \code{env}ironment or eventually
#' detected by \code{rpygeo.required.extensions}.
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
#'
#' @author Alexander Brenning, Fabian Polakowski
#' @seealso \code{\link{rpygeo.build.env}}
#' @examples
#' #TODO add examples
#' @export




# TODO change all eval parse text to get-paste
# TODO remove print statements
rpygeo_geoprocessor <- function(
                                lib,
                                fun,
                                args = NULL,
                                env = NULL,
                                extensions = NULL,
                                overwrite = FALSE,
                                detect_require_extension = TRUE) {

  # lib to string
  lib <- deparse(substitute(lib))

  # handle initial parameters
  input_check(overwrite = overwrite, extensions = extensions)


  # checkout extension
  if (detect_require_extension) {
    req_extension <- required_extensions(fun)
    if (!is.null(req_extension)) {
      e <- paste0(lib, "$CheckOutExtension('", req_extension, "')")
      eval(parse(text = e))
    }
  }


  # process
  # paste togehter string to evalutate
  args <- paste0("'", args, "'", collapse = ",")


  e <- paste0(lib, "$", fun, "(", args, ")")

  # run process with eval
  eval(parse(text = paste0(e)))

  return(NULL)
}
