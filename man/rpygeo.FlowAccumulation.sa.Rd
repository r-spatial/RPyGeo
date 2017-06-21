\name{ArcGIS Hydrology Geoprocessing Tools}
\alias{rpygeo.FlowAccumulation.sa}
\alias{rpygeo.FlowDirection.sa}
\alias{rpygeo.FlowLength.sa}
\alias{rpygeo.Sink.sa}
\title{Wrappers for functions from the Hydrology toolset}
\description{Wrappers for selected geoprocessing tools from
the ArcGIS Hydrology toolset of the Spatial Analyst extension.}
\usage{
rpygeo.FlowAccumulation.sa(in.flow.direction.raster,
    out.accumulation.raster, in.weight.raster = NULL,
    data.type = c("FLOAT","INTEGER"), ...)
rpygeo.FlowDirection.sa(in.surface.raster,
    out.flow.direction.raster, force.flow = c("NORMAL","FORCE"),
    out.drop.raster = NULL, ...)
rpygeo.FlowLength.sa(in.flow.direction.raster,
    out.raster, direction.measurement = c("DOWNSTREAM","UPSTREAM"),
    in.weight.raster = NULL, ...)
rpygeo.Sink.sa(in.flow.direction.raster, out.raster, ...)
}
\arguments{
  \item{in.flow.direction.raster, in.surface.raster, in.weight.raster,
      out.accumulation.raster, out.flow.direction.raster, 
      out.drop.raster, out.raster}{Names 
      of ArcGIS raster or vector 
      datasets or feature classes in a geodatabase (relative to the
      current workspace defined in a \code{rpygeo.env} environment).
      Shapefiles must include the extension \code{".shp"}.}
  \item{data.type, direction.measurement}{Arguments to be passed 
       to the Python geoprocessing tool. See ArcGIS help files 
       (link below) for information on the usage of scripting 
       commands and their arguments.}
  \item{force.flow}{see ArcGIS help}
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
\item{FlowAccumulation}{\url{http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Flow_Accumulation}}
\item{FlowDirection}{\url{http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Flow_Direction}}
\item{FlowLength}{\url{http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Flow_Length}}
\item{Sink}{\url{http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Sink}}
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
