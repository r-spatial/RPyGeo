\name{RPyGeo-package}
\alias{RPyGeo-package}
\alias{RPyGeo}
\docType{package}
\title{ArcGIS Geoprocessing in R via Python}
\description{Provide access to (virtually any) ArcGIS Geoprocessing
        tool from within R by running Python geoprocessing scripts
        without writing Python code or touching ArcGIS.}
\details{
\tabular{ll}{
Package: \tab RPyGeo\cr
Type: \tab Package\cr
Version: \tab 0.9-3\cr
Date: \tab 2011-09-07\cr
License: \tab GPL\cr
}
The function \code{rpygeo.geoprocessor} is the core function of this package.
It creates and runs a Python script that executes your ArcGIS/Python
geoprocessing command from within R.
This function can be used to define more convenient wrappers for frequently
used geoprocessing tools. Some are already implemented, for example
rpygeo.Slope.sa and rpygeo.EucDistance.sa, more are to be added in future
releases.
}
\author{Alexander Brenning <brenning@uwaterloo.ca>}
%\references{}
\keyword{ package }
\keyword{ interface }
\keyword{ database }
% \seealso{Optional links to other man pages,
% e.g. \code{\link[<pkg>:<pkg>-package]{<pkg>}}  }
\examples{
\dontrun{rpygeo.geoprocessor("Slope_sa('dem','slope')",
  "Aspect_sa('dem','aspect')", 
  "Hillshade_sa('dem','hshd')")
rpygeo.Slope.sa("dem","slope")}
}
