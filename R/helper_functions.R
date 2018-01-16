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
#' @export required_extensions
required_extensions <- function(expr) {
  # See ArcGIS help on the CheckOutExtension method:
  rpygeo_match_extensions <- c("sa", "3d", "stats", "na", "di")
  names(rpygeo_match_extensions) <- c("Spatial", "3d", "geostats", "network", "datainteroperability")
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
