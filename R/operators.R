#' @export

"%rpygeo_+%" <- function(raster_1, raster_2) {
  operator <- reticulate::import("operator")
  operator$add(raster_1, raster_2)
}

#' @export

"%rpygeo_-%" <- function(raster_1, raster_2) {
  operator <- reticulate::import("operator")
  operator$sub(raster_1, raster_2)
}

#' @export

"%rpygeo_*%" <- function(raster_1, raster_2) {
  operator <- reticulate::import("operator")
  operator$mul(raster_1, raster_2)
}

#' @export

"%rpygeo_/%" <- function(raster_1, raster_2) {
  operator <- reticulate::import("operator")
  operator$div(raster_1, raster_2)
}
