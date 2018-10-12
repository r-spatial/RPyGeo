#' @title  Initialize ArcPy module and environment in R
#'
#' @description Initialises the Python ArcPy site-package in R with the help
#'   of reticulate. Also setting up a geoprocessing environment and define
#'   parameters such as `overwrite` and `extensions` to add.
#' @param path Root path to the Python version which contains the Python version
#'   which is linked to the ArcPy site-package. If left empty, the function looks
#'   for `python.exe` in the most likely location (C:/Python27/). It is also
#'   possible to provide a path to the \code{ArcGIS API for Python} here.
#'   In order to do so you need to provide the path to the python anaconda library
#'   were the arcgis package is installed. Additionally \code{arcgisAPI} must be
#'   set to true.
#' @param overwrite If set to `TRUE` (default) existing ArcGIS datasets can be
#'   overwritten (does not work while using ArcGIS API for Python).
#' @param extensions Optional character vector listing ArcGIS extension that
#'   should be enabled (does not work while using ArcGIS API for Python)
#' @param x64 Logical (default: \code{FALSE}). Determines if path search should
#' look for 64 bit Python ArcPy version in default folder (C:/Python27)
#' @param pro Logical (default: \code{FALSE}). If set to `TRUE`
#'   \code{rpygeo_build_env} tries to find Python version
#'   to use in the default ArcGIS Pro location
#'   (C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3/)
#' @param arcgisAPI Logical (default: \code{FALSE}). Must be set to `TRUE`
#'   in order to use the ArcGIS API. This is the only option to work with
#'   the \code{RPyGeo} Package under a linux operation system.
#' @param workspace Path of ArcGIS workspace in which to perform the
#'    geoprocessing (does not work while using ArcGIS API for Python).
#' @param scratch_workspace Path to ArcGIS scratch workspace in which to store
#'   temporary files (does not work while using ArcGIS API for Python). If
#'   \code{NULL} a folder named scratch is created inside the workspace folder.
#' @return Returns ArcPy or ArcGIS API module in R
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

# TODO modify helper function for arcgis package
# TODO add parameters such as overwrite or cellsize or extensions
rpygeo_build_env <- function(path = NULL,
                             overwrite = TRUE,
                             extensions = NULL,
                             x64 = FALSE,
                             pro = FALSE,
                             arcgisAPI = FALSE,
                             workspace = NULL,
                             scratch_workspace = NULL) {
  # set path
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
      stop("No python version found in 'C:/Python27' - please define python path\n")
    }
  }

  if (!is.null(path)) {
    path == path
    # TODO check if path is correct
  }

  # init
  use_python(python = path, required = TRUE)

  if (!arcgisAPI) {
    import("arcpy")
  }
  if (arcgisAPI) {
    import("arcgis")
  }


  # handle initial parameters
  if (!arcgisAPI) {
    input_check(overwrite = overwrite, extensions = extensions)
  }

  # set workspace if set in function parameter

  if (!arcgisAPI) {
    if (!is.null(workspace)) {
      set_workspace(workspace)
    }
  }

  # set scratch workspace
  if (!arcgisAPI) {
    if (!is.null(workspace) & !is.null(scratch_workspace)) {
      set_scratch_workspace(scratch_workspace)
    } else if (!is.null(workspace) & is.null(scratch_workspace)) {
      dir.create(paste0(workspace, "/scratch"), showWarnings = FALSE)
      set_scratch_workspace(paste0(workspace, "/scratch"))
    }
  }

  # return Python ArcGIS library as R object
  if (!arcgisAPI) {
    return(import("arcpy"))
  }

  if (arcgisAPI) {
    return(import("arcgis"))
  }
}


#' @title ArcGIS Geoprocessor Workhorse
#'
#' @description This function utilizes the ArcPy site-package in R via the reticulate
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
#'   which ArcGIS extensions are required to evaluate the \code{function(s)}.
#' @param workspace Path of ArcGIS workspace in which to perform the
#'    geoprocessing.
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
                                workspace = NULL,
                                detect_required_extension = TRUE) {

  # lib to string
  lib <- deparse(substitute(lib))

  # handle initial parameters
  input_check(overwrite = overwrite, extensions = extensions)

  # set workspace if set in function parameter
  if (!is.null(workspace)) {
    set_workspace(workspace)
  }


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

#' @title Search for ArcPy functions
#'
#' @description Search for ArcPy functions with a character string or regular expression.
#'
#' @param search_term Search term. Regular expressions are possible.
#' @param module ArcPy or ArcGIS API module created with \code{\link{rpygeo_build_env}}.
#'
#' @return List of matching ArcPy functions
#'
#' @author Marc Becker
#' @seealso \code{\link{rpygeo_build_env}}
#'
#' @examples
#'
#' \dontrun{
#' # Load the ArcPy module and build environment
#' env <- arcpy_build_env(overwrite = TRUE, workspace = "C:/")
#'
#' # Search for ArcPy functions, which contain the term 3d
#' rpygeo_search("3d")
#' }
#' @export

rpygeo_search <- function(search_term = NULL, module = NULL) {

  # Get all ArcPy functions
  functions <- py_list_attributes(module)

  # Query available functions
  grep(search_term, functions, ignore.case = TRUE, value = TRUE) %>%
    return()
}

#' @title Load output of ArcPy functions into R session
#'
#' @description This function loads the output of an ArcPy function into the R session. Raster files are loaded as `raster` objects and vector files as `sf` objects. Currently .tif, .img and .shp files are supported. Usually this function is used with the pipe operator, hence the data parameter is not manually set.
#'
#' @param data Path to the ArcPy function output file
#'
#' @return `raster` or `sf` object
#'
#' @author Marc Becker
#'
#' @examples
#'
#' \dontrun{
#' # Load packages
#' library(spData)
#' library(dplyr)
#'
#' # Load the ArcPy module and build environment
#' env <- arcpy_build_env(overwrite = TRUE, workspace = "C:/")
#'
#' # Write raster to workspace directory
#' writeRater(elev, "C:/elev.tif")
#'
#' # Create a slope raster and load it into the R Session
#' env$Slope_3d(in_raster = "elev.tif", out_raster = "slope.tif") %>%
#'   rpygeo_load() -> slope
#' }
#' @export

rpygeo_load <- function(data) {

  # Get file path from environment object
  data %>%
    type.convert() %>%
    as.character() -> path

  # Get file extension
  path %>%
    file_ext() -> extension

  # Check file extension
  if (any(extension %in% c("tif", "img"))) {
    # Raster
    raster::raster(path) %>%
      return()
  } else if (any(extension %in% c("shp"))) {
    # Vector
    sf::st_read() %>%
      return()
  } else {
    stop("Unsupported data type. rpygeo_load supports Tagged image file format (.tif), Erdas Imagine Images (.img) and Shapefiles (.shp)")
  }
}

#' @title Get help file for ArcPy function
#'
#' @description This function opens the help file for ArcPy function in viewer panel.
#'
#' @param arcpy_function ArcPy module with function
#'
#' @return NULL
#'
#' @author Marc Becker
#'
#' @examples
#'
#' \dontrun{
#' # Load the ArcPy module and build environment
#' env <- arcpy_build_env(overwrite = TRUE, workspace = "C:/")
#'
#' # Get help file
#' rpygeo_help(env$Slope_3d)
#' }
#' @export

rpygeo_help <- function(arcpy_function) {

  # Get function documentation
  substitute(arcpy_function) %>%
    deparse() %>%
    py_function_docs() -> doc

  # Get parameters
  arcpy_function$func_doc %>%
    str_match("(INPUTS:|Arguments:)") -> help_type

  if(is.na(help_type[[1]])) {
    # No parameters
    parameters <- c("No input parameters", "No output parameters")
    template <- "help_template_generic.Rmd"
    template_parameter <- list(
      name = doc$name,
      description = arcpy_function$func_doc
    )
  } else if(help_type[[1]] == "INPUTS:") {
    # Main module help file
    arcpy_function$func_doc %>%
      str_match("OUTPUTS:") -> output_type

    if(is.na(output_type)) {
      # No output
      arcpy_function$func_doc %>%
        str_match("(INPUTS:)([\\S\\s]*)") %>%
        str_replace_all("\\n {6}", "\\\n") %>%
        str_replace("^\\n", "") %>%
        str_replace("\\s*$", "") -> res
      template <- "help_template_no_output.Rmd"
      template_parameter <- list(
        name = doc$name,
        input = res[[3]],
        example = doc$signature
      )
    } else {
      # Input and output
      arcpy_function$func_doc %>%
        str_match("(INPUTS:)([\\S\\s]*)(OUTPUTS:)([\\S\\s]*)") %>%
        str_replace_all("\\n {6}", "\\\n") %>%
        str_replace("^\\n", "") %>%
        str_replace("\\s*$", "") -> res
      template <- "help_template.Rmd"
      template_parameter <- list(
        name = doc$name,
        input = res[[3]],
        output = res[[5]],
        example = doc$signature
      )
    }
  } else if (help_type[[1]] == "Arguments:"){
    # SA module help file
    arcpy_function$func_doc %>%
      str_match("(Arguments:)([\\S\\s]*)(Results:)([\\S\\s]*)") %>%
      str_replace_all("\\n {4}", "\\\n") %>%
      str_replace("^\\n", "") %>%
      str_replace("\\s*$", "") -> res
    template <- "help_template.Rmd"
    template_parameter <- list(
      name = doc$name,
      input = res[[3]],
      output = res[[5]],
      example = doc$signature
    )
  }

  # Create temp dir for viewer
  temp_dir <- tempfile()
  if(!dir.exists(temp_dir)) {
    dir.create(temp_dir)
  }

  # Render help file
  rmarkdown::render(paste0(find.package("RPyGeo"), "/template/", template),
                    output_file = "help.html",
                    output_dir = temp_dir,
                    params = template_parameter,
                    quiet = TRUE) %>%
    rstudioapi::viewer()
}

#' @title Save temporary file to workspace
#'
#' @description This function saves temporary raster files as permanent files to the workspace. The raster format is inferred from the file extension.
#'
#' @param data Path to the temporary file
#'
#' @param filename Filename with extension
#'
#' @return RasterLayer
#'
#' @details Some ArcPy functions have no parameter to specify an output file. Instead they return an object and a temporary file is saved to the workspace. This functions writes the temporary file as a permanent file to the workspace. For supported formats s. \link[raster]{writeFormats}.
#'
#' @author Marc Becker
#'
#' @examples
#'
#' \dontrun{
#' # Load packages
#' library(spData)
#' library(dplyr)
#'
#' # Load the ArcPy module and build environment
#' env <- arcpy_build_env(overwrite = TRUE, workspace = "C:/workspace/")
#'
#' # Write raster to workspace directory
#' writeRater(elev, "C:/workspace/elev.tif")
#'
#' # Calculate
#' env$sa$Aspect(in_raster = "elev.tif") %>%
#'   rpygeo_save("aspect.tif")
#' }
#'
#' @export

rpygeo_save <- function(data, filename) {

  # Get file path from environment object
  data %>%
    type.convert() %>%
    as.character() -> path

  # Get overwrite setting
  overwrite <- py_run_string("overwrite = arcpy.env.overwriteOutput")

  # Get current workspace
  workspace <- py_run_string("workspace = arcpy.env.workspace")

  # Read raster and write to new file
  raster(path) %>%
    writeRaster(paste0(workspace$workspace, "/", filename), overwrite = overwrite$overwrite)
}
