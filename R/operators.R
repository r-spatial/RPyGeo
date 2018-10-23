#' @title Addition operator
#'
#' @description Addition operator for map algebra. Spatial Analylist extension is requiered for map algebra.
#'
#' @param raster_1 raster dataset or numeric
#'
#' @param raster_2 raster dataset or numeric
#'
#' @return {reticulate} object
#'
#' @author Marc Becker
#'
#' @examples
#'
#' \dontrun{
#' # Load the ArcPy module and build environment
#' arcpy <- arcpy_build_env(overwrite = TRUE, workspace = "C:/workspace")
#'
#' # Write raster to workspace directory
#' writeRater(elev, "C:/workspace/elev.tif", extensions = "Spatial")
#'
#' # Create raster object
#' ras <- arcpy$sa$Raster("elev.tif")
#'
#' # Add raster to itself
#' ras %rpygeo_+% ras %>%
#'   rpygeo_load()
#' }
#' @export

"%rpygeo_+%" <- function(raster_1, raster_2) {
  operator <- reticulate::import("operator")
  operator$add(raster_1, raster_2)
}

#' @title Subtraction operator
#'
#' @description Subtraction operator for map algebra. Spatial Analylist extension is requiered for map algebra.
#'
#' @param raster_1 raster dataset or numeric
#'
#' @param raster_2 raster dataset or numeric
#'
#' @return {reticulate} object
#'
#' @author Marc Becker
#'
#' @examples
#'
#' \dontrun{
#' # Load the ArcPy module and build environment
#' arcpy <- arcpy_build_env(overwrite = TRUE, workspace = "C:/workspace")
#'
#' # Write raster to workspace directory
#' writeRater(elev, "C:/workspace/elev.tif", extensions = "Spatial")
#'
#' # Create raster object
#' ras <- arcpy$sa$Raster("elev.tif")
#'
#' # Substract raster from itself
#' ras %rpygeo_+% ras %>%
#'   rpygeo_load()
#' }
#' @export

"%rpygeo_-%" <- function(raster_1, raster_2) {
  operator <- reticulate::import("operator")
  operator$sub(raster_1, raster_2)
}

#' @title Multiplication operator
#'
#' @description Multiplication operator for map algebra. Spatial Analylist extension is requiered for map algebra.
#'
#' @param raster_1 raster dataset or numeric
#'
#' @param raster_2 raster dataset or numeric
#'
#' @return {reticulate} object
#'
#' @author Marc Becker
#'
#' @examples
#'
#' \dontrun{
#' # Load the ArcPy module and build environment
#' arcpy <- arcpy_build_env(overwrite = TRUE, workspace = "C:/workspace")
#'
#' # Write raster to workspace directory
#' writeRater(elev, "C:/workspace/elev.tif", extensions = "Spatial")
#'
#' # Create raster object
#' ras <- arcpy$sa$Raster("elev.tif")
#'
#' # Multiply raster to itself
#' ras %rpygeo_+% ras %>%
#'   rpygeo_load()
#' }
#' @export

"%rpygeo_*%" <- function(raster_1, raster_2) {
  operator <- reticulate::import("operator")
  operator$mul(raster_1, raster_2)
}

#' @title Division operator
#'
#' @description Division operator for map algebra. Spatial Analylist extension is requiered for map algebra.
#'
#' @param raster_1 raster dataset or numeric
#'
#' @param raster_2 raster dataset or numeric
#'
#' @return {reticulate} object
#'
#' @author Marc Becker
#'
#' @examples
#'
#' \dontrun{
#' # Load the ArcPy module and build environment
#' arcpy <- arcpy_build_env(overwrite = TRUE, workspace = "C:/workspace")
#'
#' # Write raster to workspace directory
#' writeRater(elev, "C:/workspace/elev.tif", extensions = "Spatial")
#'
#' # Create raster object
#' ras <- arcpy$sa$Raster("elev.tif")
#'
#' # Divide raster by itself
#' ras %rpygeo_+% ras %>%
#'   rpygeo_load()
#' }
#' @export

"%rpygeo_/%" <- function(raster_1, raster_2) {
  operator <- reticulate::import("operator")
  operator$div(raster_1, raster_2)
}
