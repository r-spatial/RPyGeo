#' @title Check required ArcGIS extensions
#'
#' @description  Internal function that checks which ArcGIS extensions have to be enabled to
#'   evaluate a Python expression.
#'
#' @param expr A vector or list of character strings with Python geoprocessing
#'   expressions or function names.
#' @return Returns a character vector with the ArcGIS extension names
#'   (currently e.g. "Spatial", "3d", "geostats", "network", and/or
#'   "datainteroperability").
#' @note This internal function is used by \code{rpygeo_geoprocessor}.
#' @author Alexander Brenning, Fabian Polakowski
#' @seealso \code{\link{rpygeo_geoprocessor}}
#' @keywords interface database
#'
#'


# TODO add note, that some extensions need to be added manually
required_extensions <- function(expr) {
  # See ArcGIS help on the CheckOutExtension method:
  rpygeo_match_extensions <- c("sa", "3d", "stats", "na", "di", "ta")
  names(rpygeo_match_extensions) <- c("Spatial", "3d", "geostats", "network", "datainteroperability", "Tracking")
  ext <- c()
  for (s in expr) {
    sub_s <- strsplit(s, "(", fixed = TRUE)[[1]]
    for (t in sub_s) {
      t <- gsub(" ", "", t)
      if (substring(t, nchar(t)) == ")") next
      for (i in 1:length(rpygeo_match_extensions)) {
        the.match <- paste("_", tolower(rpygeo_match_extensions[i]), sep = "")
        if (tolower(substring(t, nchar(t) + 1 - nchar(the.match))) == the.match) {
          ext <- c(ext, names(rpygeo_match_extensions)[i])
        }
      }
    }
  }
  return(unique(ext))
}




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
#'

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
#'
set_workspace = function (path) {

  e <- paste0("arcpy.env.workspace = '",path,"'" )
  py_run_string(e)
}


