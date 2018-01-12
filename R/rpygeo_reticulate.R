#' @title  Initialize Arcpy module in R
#'
#' @description Initialises the Python "arcpy" site-package in R with the help
#'   of reticulate
#'
#'
#' @param path Root path to the Python version which contains "arcpy". If left empty, the function
#'   looks for `python.exe` in the most likely location (C:\Python27\)
#' @return Returns arcpy module in R
#' @author Fabian Polakowski
#' @export

init = function (path = NULL)
  {

  # set path
  # TODO check if it is really a arcpy python
  if (is.null(path)) {
    # TODO search for path
    # search python.exe in most common path
    dirs <- list.files(path = "C:/Python27", pattern = "python.exe", recursive = TRUE, full.names = TRUE)

    if (length(dirs) == 1) {
      path <- dirs
    }

    if (length(dirs) > 1) {
      stop("multiple paths found, define ArcGIS Path\n")
    }

    if (length(dirs) < 1){
      stop("No python function found in 'C:/Python27' - please define python path\n")
    }

  }
  if (!is.null(path)) {
    path == path
    #TODO check if path is correct
  }

  # init
  use_python(python = path, required = TRUE)


}







