\name{ArcGIS ASCII/Raster Conversion}
\alias{rpygeo.ASCIIToRaster.conversion}
\alias{rpygeo.RasterToASCII.conversion}
\title{Wrappers for ASCII-to-raster conversion}
\description{Wrappers for ASCII-to-raster conversion functions from the
    Conversion toolbox.}
\usage{
rpygeo.ASCIIToRaster.conversion(in.ascii.file, out.raster,
    data.type = c("FLOAT","INTEGER"), ...)
rpygeo.RasterToASCII.conversion(in.raster, out.ascii.file, ...)
}
\arguments{
  \item{in.ascii.file, in.raster, out.raster, out.ascii.file}{Names 
      of ArcGIS raster datasets, or raster feature classes in a 
      geodatabase (relative to the
      current workspace defined in a \code{rpygeo.env} environment).
      Shapefiles must include the extension \code{".shp"}.}
  \item{data.type}{Arguments to be passed 
       to the Python geoprocessing tool. See ArcGIS help files 
       (link below) for information on the usage of scripting 
       commands and their arguments.}
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
\item{ASCIIToRaster}{\url{http://webhelp.esri.com/arcgisdesktop/9.2/body.cfm?tocVisable=1&ID=1309&TopicName=ASCII to Raster (Conversion)}}
\item{RasterToASCII}{\url{http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Raster to ASCII (Conversion)}}
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
