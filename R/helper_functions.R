#' @title  Set overwrite and extension parameters
#'
#' @description Internal helper function that sets the overwrite parameter for
#'  the ArcPy session and can check out multiple extensions.
#'
#' @param overwrite If set to `TRUE` (default) existing ArcGIS datasets can be
#'   overwritten.
#' @param extensions Optional character vector listing ArcGIS extension that
#'   should be enabled. This adds to any extensions that are eventually
#'   detected by \code{rpygeo_required_extensions}.
#' @note This internal function is used by \code{rpygeo_geoprocessor} and
#'   by \code{rpygeo_build_env}.
#' @author Fabian Polakowski, Alexander Brenning
#'
#' @export

input_check = function (overwrite, extensions) {

  # handle overwrite
  if (overwrite) {
    py_run_string("arcpy.env.overwriteOutput = True")
  }

  # edit 'overwrite' back to FALSE if it was TRUE for a previous function
  if (!overwrite) {
    py_run_string("arcpy.env.overwriteOutput = False")
  }

  # handle extensions
  if (!is.null(extensions)) {

    sapply(extensions, function(x) {

      ext <- paste0("arcpy.CheckOutExtension('", x, "')")
      py_run_string(ext)

      })
  }

}

#' @title  Set workspace
#'
#' @description set workspace for ArcPy environment
#'
#' @param path path to the workspace containing your data
#' @author Fabian Polakowski
#'
#' @export
set_workspace = function (path) {

  e <- paste0("arcpy.env.workspace = '",path,"'" )
  py_run_string(e)
}

#' @title  Set scratch workspace
#'
#' @description Set scratch workspace for ArcPy environment
#'
#' @param path path to the scratch workspace
#'
#' @author Fabian Polakowski and Marc Becker
#'
#' @export
set_scratch_workspace = function (path) {

  e <- paste0("arcpy.env.scratchWorkspace = '",path,"'" )
  py_run_string(e)
}
