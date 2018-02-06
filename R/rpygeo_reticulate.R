#' @title  Initialize ArcPy module and environment in R
#'
#' @description Initialises the Python ArcPy site-package in R with the help
#'   of reticulate. Also setting up a geoprocessing environment and define
#'   parameters such as `overwrite` and `extensions` to add.
#' @param path Root path to the Python version which contains the Python version
#'   which is linked to the ArcPy site-package. If left empty, the function looks
#'   for `python.exe` in the most likely location
#'   (C:/Python27/)
#' @param overwrite If set to `TRUE` (default) existing ArcGIS datasets can be
#'   overwritten.
#' @param extensions Optional character vector listing ArcGIS extension that
#'   should be enabled.
#' @param x64 Logical (default: \code{TRUE}). Determines if path search should
#' look for 64 bit Python ArcPy version in default folder (C:/Python27)
#'
#' @param pro If set to `TRUE` \code{rpygeo_build_env} tries to find Python version
#'   to use in the default ArcGIS Pro location
#'   (C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3/)
#' @return Returns ArcPy module in R
#' @author Fabian Polakowski
#' @seealso \code{\link{rpygeo_geoprocessor}}
#' @examples
#'
#' # load the ArcPy module related to ArcGIS Pro (and save it as an R
#' # object called "arcpy_m") in R and also set the overwrite parameter
#' # to FALSE and add some extensions. Note that we do not have to set the path
#' # because the Python version is located in the default location
#' # (C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3/)in this example.
#' \dontrun{arcpy_m <- rpygeo_build_env(overwrite = TRUE,
#'                                      extensions = c("3d", "Spatial", "na"),
#'                                      pro = TRUE)}
#'
#' # load the ArcPy module when your Python version is located in a different
#' # folder
#' \dontrun{arc <- rpygeo_build_env(path = "C:/YourPath/YourSubPath/python.exe")}
#'
#' @export
#'


# TODO add workspace
# TODO add parameters such as overwrite or cellsize or extensions
rpygeo_build_env <- function(path = NULL,
                             overwrite = TRUE,
                             extensions = NULL,
                             x64 = FALSE,
                             pro = FALSE,
                             workspace = NULL) {
  # set path
  # TODO check if it is really a ArcPy python
  if (is.null(path)) {
    if (x64) {
      dirs1 <- list.files(
        path = "C:/Python27",
        pattern = "64", recursive = FALSE, full.names = TRUE
      )

      dirs <- list.files(
        path = dirs1,
        pattern = "python.exe", recursive = TRUE, full.names = TRUE
      )
    }


    if (pro) {
      dirs <- list.files(
        path = "C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3",
        pattern = "python.exe", recursive = TRUE, full.names = TRUE
      )
    }

    if (!pro && !x64) {
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

  # set workspace if set in function parameter
  if (!is.null(workspace)) {
    set_workspace(workspace)
  }

  return(import("arcpy"))
}


#' @title ArcGIS Geoprocessor Workhorse
#'
#' @description This function utilzes the ArcPy site-package in R via the reticulate
#'   connection to perform ArcPy calculation in R. It returns error messages if
#'   an error appears.
#'
#' @param lib ArcPy R module name assigned using \code{rpygeo_build_env}
#' @param fun Single geoprocessing function name to be evaluated by the Python
#'   geoprocessor.
#' @param args Vector or list of arguments to be passed to the function listed
#' in \code{fun}.
#' @param extensions Optional character vector listing ArcGIS extension that
#'   should be enabled. This adds to any extensions that are eventually
#'   detected by \code{rpygeo_required_extensions}.
#' @param overwrite If set to `TRUE` (default) existing ArcGIS datasets can be
#'   overwritten.
#' @param detect_required_extensions Logical (default: \code{TRUE}).
#'   Determines whether \code{\link{required_extensions}} should try to find out
#'   which ArcGIS extensions are required to evaluate the \code{fun}ction(s).
#' @return The function returns \code{NULL} if is was successful, or otherwise
#'   a ArcGIS error message.
#'
#' @author Alexander Brenning, Fabian Polakowski
#' @seealso \code{\link{rpygeo_build_env}}
#'
#' @examples
#'
#' # Build a ArcGIS environment (assined to an R object called arcpy_m)
#' # and set overwrite to TRUE.
#' \dontrun{arcpy_m <- arcpy_build_env(overwrite = TRUE)}
#'
#' # Use the ArcGIS Slope alogrithm to calulate a slope from a Digital Elveation
#' # Model
#' \dontrun{rpygeo_geoprocessor(lib = a, fun = "Slope_3d",
#'                              args = c("dem.tif", "output_slope.tif"))}
#'
#' @export




# TODO change all eval parse text to get-paste
# TODO remove print statements
rpygeo_geoprocessor <- function(
                                lib,
                                fun,
                                args = NULL,
                                extensions = NULL,
                                overwrite = FALSE,
                                detect_required_extension = TRUE) {

  # lib to string
  lib <- deparse(substitute(lib))

  # handle initial parameters
  input_check(overwrite = overwrite, extensions = extensions)


  # checkout extension
  if (detect_required_extension) {
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
