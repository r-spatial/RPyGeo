\name{ArcGIS Map Algebra}
\alias{rpygeo.SingleOutputMapAlgebra.sa}
\title{Wrapper for the Map Algebra tool}
\description{Wrappers for the Single Output Map Algebra tool
of the Spatial Analyst extension.}
\usage{
rpygeo.SingleOutputMapAlgebra.sa(expression.string, out.raster, 
    in.data = NULL, ...)
}
\arguments{
  \item{in.data,
      out.raster}{Names 
      of ArcGIS raster or vector 
      datasets or feature classes in a geodatabase (relative to the
      current workspace defined in a \code{rpygeo.env} environment).
      Shapefiles must include the extension \code{".shp"}.}
  \item{expression.string}{Valid Map Algebra expression as described
       in the ArcGIS help files (link below).}
  \item{\dots}{Additional arguments to be passed to
        \code{\link{rpygeo.geoprocessor}}.}
}
\details{
These functions simply interface the behaviour of the
ArcGIS/Python geoprocessing functions with the equivalent names.
See \code{\link{rpygeo.geoprocessor}} for details on what happens behind
the scenes.

ArcGIS 9.2 online help for the georpocessing tools can be accessed 
through the following URLs:
\itemize{
\item{SingleOutputMapAlgebra}{\url{http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Single_Output_Map_Algebra}}
}
}
\value{
The functions return \code{NULL} if no error occurred, otherwise
a character vector containing the error message.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Alexander Brenning}
%\note{}
\seealso{\code{\link{rpygeo.geoprocessor}}, \code{\link{rpygeo.build.env}}}
%\examples{}
\keyword{interface}
\keyword{database}
