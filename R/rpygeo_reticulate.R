#library(reticulate)

init = function (
  path = NULL,
  extensions = NULL
  )
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

  }

  # init
  use_python(python = path, required = TRUE)

  # TODO handle extensions

}




