#' @title  Initialize ArcPy site-package in R
#'
#' @description Initialises the Python ArcPy site-package in R via the
#'  \code{reticulate} package. Addtionally environment settings and extensions
#'  are configured.
#'
#' @param path Full path to folder containing Python version which is linked to
#'   the ArcPy site-package. If left empty, the function looks
#'   for \code{python.exe} in the most likely location (\code{C:/Python27/}). It is also
#'   possible to provide a path to the \code{ArcGIS API for Python} here.
#'   In order to do so you need to provide the path to the python anaconda library
#'   were the \code{arcgis} package is installed. Additionally \code{arcgisAPI}
#'   must be set to true.
#'
#' @param overwrite If \code{TRUE} (default), existing ArcGIS datasets can be
#'   overwritten (does not work while using ArcGIS API for Python).
#'
#' @param extensions Optional character vector listing ArcGIS extension that
#'   should be enabled (does not work while using ArcGIS API for Python)
#'
#' @param x64 Logical (default: \code{FALSE}). Determines if path search should
#' look for 64 bit Python ArcPy version in default folder (\code{C:/Python27})
#'
#' @param pro Logical (default: \code{FALSE}). If set to \code{TRUE}`
#'   \code{rpygeo_build_env} tries to find Python version
#'   to use in the default ArcGIS Pro location
#'   (\code{C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3/})
#'
#' @param arcgisAPI Logical (default: \code{FALSE}). Must be set to \code{TRUE}
#'   in order to use the ArcGIS API. This is the only option to work with
#'   the \code{RPyGeo} Package under a linux operation system.
#'
#' @param workspace Path of ArcGIS workspace in which to perform the
#'    geoprocessing (does not work while using ArcGIS API for Python).
#'
#' @param scratch_workspace Path to ArcGIS scratch workspace in which to store
#'   temporary files (does not work while using ArcGIS API for Python). If
#'   \code{NULL} a folder named scratch is created inside the workspace folder
#'   or on the same directory level as the workspace file geodatabase.
#'
#' @return Returns ArcPy or ArcGIS modules in R
#'
#' @author Fabian Polakowski, Marc Becker
#'
#' @examples
#'
#' \dontrun{
#' # Load ArcPy side-package of ArcGIS Pro with 3D and Spatial Analysis extension.
#' # and set environment setting 'overwrite' to TRUE.
#' # Note that no path parameter is necessary because Python is located in the
#' # default location.
#' arcpy <- rpygeo_build_env(overwrite = TRUE,
#'                           extensions = c("3d", "Spatial"),
#'                           pro = TRUE)}
#'
#' # load the ArcPy module when your Python version is located in a different
#' # folder
#  arcpy <- rpygeo_build_env(path = "C:/YourPath/YourSubPath/python.exe")
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
  reticulate::use_python(python = path, required = TRUE)

  if (!arcgisAPI) {
    reticulate::import("arcpy")
  }
  if (arcgisAPI) {
    reticulate::import("arcgis")
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
      if(tools::file_ext(basename(workspace)) == "gdb") {
        dir.create(paste0(dirname(workspace), "/scratch"), showWarnings = FALSE)
        set_scratch_workspace(paste0(dirname(workspace), "/scratch"))
      } else {
        dir.create(paste0(workspace, "/scratch"), showWarnings = FALSE)
        set_scratch_workspace(paste0(workspace, "/scratch"))
      }
    }
  }

  # return Python ArcGIS library as R object
  if (!arcgisAPI) {
    return(reticulate::import("arcpy"))
  }

  if (arcgisAPI) {
    return(reticulate::import("arcgis"))
  }
}

#' @title Search for ArcPy functions and classes
#'
#' @description Search for ArcPy functions and classes with a character string or regular expression.
#'
#' @param search_term Search term. Regular expressions are possible.
#'
#' @details The list members are referenced by the ArcPy module names. Each
#' member contains a character vector of matching ArcPy functions and classes.
#' Except for the main module, functions and classes have to be accessed by their
#' module names (s. examples).
#'
#' @return Named list of character vectors of matching ArcPy functions and classes
#'
#' @author Marc Becker
#'
#' @examples
#'
#' \dontrun{
#' # Get data
#' data(dem, package = "RQGIS")
#'
#' # Load the ArcPy module and build environment
#' arcpy <- rpygeo_build_env(overwrite = TRUE,
#'                           workspace = tempdir(),
#'                           extensions = "Spatial")
#'
#' # Write raster to workspace directory
#' writeRaster(dem, file.path(tempdir(), "dem.tif"))
#'
#' # Search for ArcPy functions, which contain the term slope
#' rpygeo_search("slope")
#'
#' #> $toolbox
#' #> [1] "Slope_3d"        "SurfaceSlope_3d"
#' #>
#' #> $main
#' #> [1] "Slope_3d"        "SurfaceSlope_3d"
#' #>
#' #> $sa
#' #> [1] "Slope"
#' #>
#' #> $ddd
#' #> [1] "Slope"        "SurfaceSlope"
#'
#' # Run function from sa module
#' arcpy$sa$Slope(in_raster="dem.tif")
#'
#' # Run function from main module
#' arcpy$Slope_3d(in_raster="dem.tif")
#' }
#' @export
#'
#' @importFrom magrittr "%>%"

rpygeo_search <- function(search_term = "") {

  # Get modules with functions and classes
  modules <- reticulate::py_run_file(paste0(find.package("RPyGeo", lib.loc = .libPaths()), "/python/get_modules.py"))

  # Query available functions
  modules$module %>%
    purrr::map(function(a) stringr::str_subset(a, stringr::regex(search_term, ignore_case = TRUE))) %>%
    purrr::keep(function(a) length(a) > 0) -> search_result

  if(length(search_result) < 1) {
    return(NULL)
  } else {
    return(search_result)
  }
}

#' @title Load output of ArcPy functions into R session
#'
#' @description This function loads the output of an ArcPy function into the R session. Raster files are loaded as \code{raster} objects and vector files as \code{sf} objects.
#'
#' @param data \code{reticulate} object or filename of the ArcPy function output
#'
#' @return \code{raster} or \code{sf} object
#'
#' @details Currently files and datasets stored in file geodatabases are supported.
#'
#' Supported file formats:
#' \itemize{
#'   \item Tagged Image File Format (.tif)
#'   \item Erdas Imagine Images (.img)
#'   \item Esri Arc/Info Binary Grid (.adf)
#'   \item Esri ASCII Raster (.asc)
#'   \item Esri Shapefiles (.shp)
#'   }
#'
#' Supported datasets:
#' \itemize{
#'   \item Feature Class
#'   \item Raster Dataset
#' }
#'
#' Esri has not released an API for raster datasets in file geodatabases. \code{rpygeo_load} converts a raster dataset to a temporary ASCII raster first and then loads it into the R session. Be aware that this can take a long time for large raster datasets.
#'
#' This function can be used with the \code{\%>\%} operator from the \code{dplyr} package. The \code{\%>\%} operator forwards the \code{reticulate} object from the ArcPy function to \code{rpygeo_load} (s. Example 1). If used without the \code{\%>\%} operator an \code{reticulate} object can be specified for the \code{data} parameter (s. Example 2). It is also possible to use the filename of the ArcPy function output (s. Example 3). For Arc/Info Binary Grids the \code{data} parameter is just the name of the directory, which contains the \code{adf} files.
#'
#' @author Marc Becker
#'
#' @examples
#'
#' \dontrun{
#' # Load packages
#' library(RPyGeo)
#' library(spData)
#' library(dplyr)
#'
#' # Load the ArcPy module and build environment
#' arcpy <- arcpy_build_env(overwrite = TRUE, workspace = "C:/workspace")
#'
#' # Write raster to workspace directory
#' writeRater(elev, "C:/workspace/elev.tif")
#'
#' # Create a slope raster and load it into the R session (Example 1)
#' arcpy$Slope_3d(in_raster = "elev.tif", out_raster = "slope.tif") %>%
#'   rpygeo_load() -> slope
#'
#' # Create a aspect raster and load it into the R session (Example 2)
#' ras_aspect <- arcpy$sa$Aspect(in_raster = "elev.tif")
#' rpygeo_load(ras_aspect)
#'
#' # Convert elevation raster to polygon shapefile and load it into R session (Example 3)
#' arcpy$RasterToPolygon_conversion("elev.tif", "C:/workspace/elev.shp")
#' rpygeo_load("elev.shp")
#' }
#'
#' @export
#'
#' @importFrom magrittr "%>%"

rpygeo_load <- function(data) {

  # Get path from reticulate object
  data %>%
    utils::type.convert() %>%
    as.character() -> path

  # Get info
  info <- reticulate::py_run_string(paste0("info = arcpy.Describe('", path,"')"))

  # File or dataset in file geodatabase
  if(tools::file_ext(basename(info$info$path)) == "gdb") {
    # File geodatabase
    if(info$info$dataType == "FeatureClass") {
      # Vector
      sf::st_read(dsn = info$info$path, layer = info$info$baseName, quiet=TRUE) %>%
        return()
    } else if(info$info$dataType == "RasterDataset") {
      # Raster
      # Create temporary file with less than 8 characters
      tempdir() %>%
        paste0("/r", paste0(floor(stats::runif(7, min=0, max=9)), collapse = ""), ".asc") ->  temp_file

      # Export raster from geodatabase to temporary directory
      reticulate::py_run_string(paste0("arcpy.RasterToASCII_conversion('", info$info$baseName,"', '", temp_file,"')"))

      raster::raster(temp_file) %>%
        return()
    } else {
      stop("Unsupported dataset. rpygeo_load supports Feature Class and Raster Dataset.")
    }
  } else {
    # File
    # Check file extension
    if (any(info$info$extension %in% c("tif", "img", "asc"))) {
      # Raster
      raster::raster(paste0(info$info$path, "/" ,info$info$file)) %>%
        return()
    } else if (any(info$info$extension %in% c("shp"))) {
      # Vector
      sf::st_read(paste0(info$info$path, "/" ,info$info$file), quiet=TRUE) %>%
        return()
    } else if(info$info$extension == "" & file.exists(paste0(info$info$path, "/" ,info$info$file, "/hdr.adf"))) {
      # Arc/Info Binary Grid
      raster::raster(paste0(info$info$path, "/" ,info$info$file)) %>%
        return()
    } else {
      stop("Unsupported data type. rpygeo_load supports Tagged Image File Format (.tif), Erdas Imagine Images (.img), Arc/Info Binary Grid (.adf), Esri ASCII Raster (.asc) and Shapefiles (.shp)")
    }
  }
}

#' @title Get help file for ArcPy function
#'
#' @description This function opens the help file for ArcPy function in viewer panel or if not available in the browser.
#'
#' @param arcpy_function ArcPy module with function or class
#'
#' @author Marc Becker
#'
#' @examples
#'
#' \dontrun{
#' # Load the ArcPy module and build environment
#' arcpy <- arcpy_build_env(overwrite = TRUE, workspace = "C:/workspace/")
#'
#' # Open help file
#' rpygeo_help(arcpy$Slope_3d)
#' }
#'
#' @export
#'
#' @importFrom magrittr "%>%"

rpygeo_help <- function(arcpy_function) {

  # Get function documentation
  substitute(arcpy_function) %>%
    deparse() %>%
    reticulate::py_function_docs() -> doc

  # Get parameters
  arcpy_function$func_doc %>%
    stringr::str_match("(INPUTS:|Arguments:)") -> help_type

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
      stringr::str_match("OUTPUTS:") -> output_type

    if(is.na(output_type)) {
      # No output
      arcpy_function$func_doc %>%
        stringr::str_match("(INPUTS:)([\\S\\s]*)") %>%
        stringr::str_replace_all("\\n {6}", "\\\n") %>%
        stringr::str_replace("^\\n", "") %>%
        stringr::str_replace("\\s*$", "") -> res
      template <- "help_template_no_output.Rmd"
      template_parameter <- list(
        name = doc$name,
        input = res[[3]],
        example = doc$signature
      )
    } else {
      # Input and output
      arcpy_function$func_doc %>%
        stringr::str_match("(INPUTS:)([\\S\\s]*)(OUTPUTS:)([\\S\\s]*)") %>%
        stringr::str_replace_all("\\n {6}", "\\\n") %>%
        stringr::str_replace("^\\n", "") %>%
        stringr::str_replace("\\s*$", "") -> res
      template <- "help_template.Rmd"
      template_parameter <- list(
        name = doc$name,
        input = res[[3]],
        output = res[[5]],
        example = doc$signature
      )
    }
  } else if (help_type[[1]] == "Arguments:"){
    # Spatial Analylist help file
    arcpy_function$func_doc %>%
      stringr::str_match("(Arguments:)([\\S\\s]*)(Results:)([\\S\\s]*)") %>%
      stringr::str_replace_all("\\n {4}", "\\\n") %>%
      stringr::str_replace("^\\n", "") %>%
      stringr::str_replace("\\s*$", "") -> res
    template <- "help_template.Rmd"
    template_parameter <- list(
      name = doc$name,
      input = res[[3]],
      output = res[[5]],
      example = doc$signature
    )
  }

  # Render help file
  help_file <- rmarkdown::render(paste0(find.package("RPyGeo"), "/template/", template),
                                 output_file = "help.html",
                                 output_dir = tempdir(),
                                 params = template_parameter,
                                 quiet = TRUE)

  # Check if viewer is available
  if (!is.null(getOption("viewer"))){
    rstudioapi::viewer(help_file)
  } else {
    utils::browseURL(help_file)
  }
}

#' @title Save temporary raster to workspace
#'
#' @description This function saves temporary a raster as permanent raster to the workspace.
#'
#' @param data \code{reticulate} object or full path of the ArcPy function output
#'
#' @param filename Filename with extension or without extension if the workspace is file geodatabase
#'
#' @details Some ArcPy functions have no parameter to specify an output raster. Instead they return a raster object and a temporary raster is saved to the scratch workspace. This functions writes the temporary raster as a permanent raster to the workspace.
#'
#' How the file is written depends on the workspace and scratch workspace environment settings.
#'
#' \itemize{
#'   \item Workspace and scratch workspace are directories: Raster is loaded with the \code{raster} package and is written to workspace directory. The file format is inferred from the file extension in the \code{filename} parameter.
#'   \item Workspace and scratch workspace are file geodatabases: Raster is copied to workspace file geodatabase. No file extension necessary for the \code{filename} parameter.
#'   \item Workspace is file geodatabase and scratch workspace is directory: Raster is copied to workspace file geodatabase. No file extension necessary for the \code{filename} parameter.
#'   \item Workspace is directory and scratch workspace is file geodatabase: Raster is exported to workspace directory. The \code{filename} parameter is ignored due to restrictions in \code{arcpy.RasterToOtherFormat_conversion} function. If the automatically generated filename already exists, a number is appended to the end of the filename.
#' }
#'
#' @author Marc Becker
#'
#' @examples
#'
#' \dontrun{
#' # Load packages
#' library(RPyGeo)
#' library(spData)
#' library(dplyr)
#'
#' # Load the ArcPy module and build environment
#' arcpy <- arcpy_build_env(overwrite = TRUE, workspace = "C:/workspace/")
#'
#' # Write raster to workspace directory
#' writeRater(elev, "C:/workspace/elev.tif")
#'
#' # Calculate temporary aspect file and save to workspace
#' arcpy$sa$Aspect(in_raster = "elev.tif") %>%
#'   rpygeo_save("aspect.tif")
#' }
#'
#' @export
#'
#' @importFrom magrittr "%>%"

rpygeo_save <- function(data, filename) {

  # Get file path from environment object
  data %>%
    utils::type.convert() %>%
    as.character() -> path

  # Get overwrite setting
  overwrite <- reticulate::py_run_string("overwrite = arcpy.env.overwriteOutput")

  # Get current workspace
  workspace <- reticulate::py_run_string("workspace = arcpy.env.workspace")

  # Get info
  info <- reticulate::py_run_string(paste0("info = arcpy.Describe('", path,"')"))

  if(info$info$dataType != "RasterDataset") {
    stop("Only raster files or raster datasets in file geodatabases are supported.")
  }

  # File or dataset in file geodatabase
  if(tools::file_ext(basename(info$info$path)) == "gdb" & tools::file_ext(basename(workspace$workspace)) == "gdb") {
    # Workspace and scratch workspace are file geodatabase
    # Copy from scratch file geodatabase to workspace file geodatabase
    reticulate::py_run_string(paste0("arcpy.Copy_management('", info$info$catalogpath,"', '", workspace$workspace,"/",filename,"')"))
  } else if (tools::file_ext(basename(workspace$workspace)) == "gdb") {
    # Workspace is file geodatabase and scratch workspace is directory
    reticulate::py_run_string(paste0("arcpy.Copy_management('", info$info$catalogpath,"', '", workspace$workspace,"/",filename,"')"))
  } else if (tools::file_ext(basename(info$info$path)) == "gdb") {
    # Workspace is directory and scratch workspace is file geodatabase
    reticulate::py_run_string(paste0("arcpy.RasterToOtherFormat_conversion('", info$info$catalogpath,"', '", workspace$workspace,"')"))
  } else {
    # Workspace and scratch workspace are directories
    raster::raster(info$info$catalogpath) %>%
      raster::writeRaster(paste0(workspace$workspace, "/", filename), overwrite = overwrite$overwrite)
  }
}
