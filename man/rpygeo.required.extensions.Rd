\name{rpygeo.required.extensions}
\alias{rpygeo.required.extensions}
\title{Check required ArcGIS extensions}
\description{Internal function that checks which ArcGIS extensions have to
be enabled to evaluate a Python expression.}
\usage{
rpygeo.required.extensions(expr)
}
\arguments{
  \item{expr}{A vector or list of character strings with Python geoprocessing
              expressions or function names.}
}
\value{Returns a character vector with the ArcGIS extension names
(currently e.g. "Spatial", "3d", "geostats", "network", and/or "datainteroperability").}
\author{Alexander Brenning}
\note{This internal function is used by \code{rpygeo.geoprocessor}.}
\seealso{\code{\link{rpygeo.geoprocessor}}}
\keyword{ interface }
\keyword{ database }
