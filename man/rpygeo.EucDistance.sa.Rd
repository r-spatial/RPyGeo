\name{ArcGIS Geoprocessing Tools}
\alias{rpygeo.EucDistance.sa}
\alias{rpygeo.Aspect.sa}
\alias{rpygeo.Slope.sa}
\alias{rpygeo.Hillshade.sa}
\alias{rpygeo.Curvature.sa}
\alias{rpygeo.Delete.management}
\title{Wrappers for selected ArcGIS functions}
\description{Wrappers for a small selection of ArcGIS geoprocessing
functions based on the \code{rpygeo.geoprocessor}.}
\usage{
rpygeo.EucDistance.sa(in.data, out.raster, maxdist = NULL, 
    cellsize = NULL, out.direction.raster = NULL, 
    env = rpygeo.env, ...)
rpygeo.Hillshade.sa(in.raster, out.raster, azimuth = 315, 
    altitude = 45, model.shadows = c("NO_SHADOWS", "SHADOWS"), 
    z.factor = 1, ...)
rpygeo.Slope.sa(in.raster, out.raster, 
    unit = c("DEGREE", "PERCENT_RISE"), z.factor = 1, ...)
rpygeo.Aspect.sa(in.raster, out.raster, ...)
rpygeo.Curvature.sa(in.raster, out.curvature.raster, 
    z.factor = 1, out.profile.curve.raster = NULL,
    out.plan.curve.raster = NULL, ...)
rpygeo.Delete.management(in.data, data.type = NULL, ...)
}
\arguments{
  \item{in.raster, in.data, out.raster, out.curvature.raster, 
      out.profile.curve.raster, out.plan.curve.raster}{Names 
      of ArcGIS raster or vector 
      datasets or feature classes in a geodatabase (relative to the
      current workspace defined in a \code{rpygeo.env} environment).
      Shapefiles must include the extension \code{".shp"}.}
  \item{env}{A list defining an RPyGeo working environment as built
      by \code{rpygeo.build.env}.}
  \item{maxdist, cellsize, out.direction.raster}{ see ArcGIS online help  }
  \item{azimuth, altitude, model.shadows, z.factor}{ see ArcGIS online help }
  \item{unit, data.type}{Arguments to be passed to the Python
       geoprocessing function. See ArcGIS help files for information
       on the usage of scripting commands and their arguments.}
  \item{\dots}{Additional arguments to be passed to
        \code{\link{rpygeo.geoprocessor}}.}
}
\details{
These functions simply try to replicate the behaviour of the
ArcGIS/Python geoprocessing functions of the same name.
See \code{\link{rpygeo.geoprocessor}} for details on what happens behind
the scenes.

ArcGIS 9.2 online help for the georpocessing tools can be accessed 
through the following URLs:
\itemize{
\item{EucDistance}{\url{http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=EucDistance}}
\item{Hillshade}{\url{http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Hillshade}}
\item{Slope}{\url{http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Slope}}
\item{Aspect}{\url{http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Aspect}}
\item{Curvature}{\url{http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Curvature}}
\item{Delete}{\url{http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Delete_(Data_Management)}}
}
}
\value{
The function return \code{NULL} if no error occurred, otherwise
a character vector containing the error message.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Alexander Brenning}
%\note{}
\seealso{\code{\link{rpygeo.geoprocessor}}, \code{\link{rpygeo.build.env}}}
\examples{
# Allow ArcGIS to overwrite existing datasets:
\dontrun{rpygeo.env$overwriteoutput = 1
# Calculate the slope of a DEM raster dataset
# in the current ArcGIS workspace:
rpygeo.geoprocessor("Slope_sa",c("dem","slope"))
# Same:
rpygeo.geoprocessor("Slope_sa('dem','slope')")
# Same, using the more convenient wrapper:
rpygeo.Slope.sa("dem","slope")}

# Three at a time or separately:
\dontrun{date()
rpygeo.geoprocessor("Slope_sa('dem','slope')",
  "Aspect_sa('dem','aspect')", "Hillshade_sa('dem','hshd')")
date() # ~20 sec on my computer
rpygeo.Slope.sa("dem","slope")
rpygeo.Aspect.sa("dem","aspect")
rpygeo.Hillshade.sa("dem","hshd")
date() # ~50 sec
rpygeo.Delete.management("slope")
rpygeo.Delete.management("aspect")
rpygeo.Delete.management("hshd")}

# Calculate the Euclidian distance from railway lines
# up to a max. distance of 1000 map units:
\dontrun{rpygeo.geoprocessor("EucDistance_sa",
    args=list("rail.shp","raildist",1000))
# Same:
rpygeo.EucDistance.sa("rail.shp","raildist",maxdist=1000)}

# Use MapAlgebra to calculate a distance-decay function:
\dontrun{rpygeo.geoprocessor("SingleOutputMapAlgebra_sa",
    args=c("exp( raildist / -100 )","distdecay"))}

# Or why not in just one step if you like MapAlgebra:
\dontrun{rpygeo.geoprocessor( "SingleOutputMapAlgebra_sa",
    args=c("exp( EucDistance( rail.shp, \#, \#, 1000 ) / -100 )","distdecay") )}
}
\keyword{interface}
\keyword{database}
