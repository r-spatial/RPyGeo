\name{ArcGIS Solar Radiation}
\alias{rpygeo.AreaSolarRadiation.sa}
\alias{rpygeo.Viewshed.sa}
\title{Wrappers for solar radiation and viewshed geoprocessing tools}
\description{Wrappers for ArcGIS geoprocessing tools for
calculating solar radiation and viewsheds (Spatial Analyst extension).}
\usage{
rpygeo.AreaSolarRadiation.sa(in.surface.raster, 
        out.global.radiation.raster, latitude = 45, sky.size = 200, 
        time.configuration, day.interval = 14, hour.interval = 0.5, 
        each.interval = c("NOINTERVAL", "INTERVAL"), 
        z.factor = NULL, slope.aspect.input.type = c("FROM_DEM", "FLAT_SURFACE"), 
        calculation.directions = 32, zenith.divisions = 8, 
        azimuth.divisions = 8, 
        diffuse.model.type = c("UNIFORM_SKY", "STANDARD_OVERCAST_SKY"), 
        diffuse.proportion = 0.3, transmittivity = 0.5, 
        out.direct.radiation.raster = NULL, out.diffuse.radiation.raster = NULL, 
        out.direct.duration.raster = NULL, ...)
rpygeo.Viewshed.sa(in.raster, in.observer.features,
    out.raster, z.factor = 1, 
    curvature.correction = c("FLAT_EARTH", "CURVED_EARTH"),
    refractivity.coefficient = 0.13,
    x.field = "x", y.field = "y", tmpdir = tempdir(), ...)
}
\arguments{
  \item{in.surface.raster, in.raster, 
      out.global.radiation.raster, out.raster,
      out.direct.radiation.raster, out.diffuse.radiation.raster,
      out.direct.duration.raster}{Names 
      of ArcGIS raster datasets, or feature classes in a geodatabase
      (relative to the
      current workspace defined in a \code{rpygeo.env} environment).}
  \item{latitude, sky.size, time.configuration,
    day.interval, hour.interval, each.interval, z.factor,
    slope.aspect.input.type,
    calculation.directions, zenith.divisions, azimuth.divisions,
    diffuse.model.type, diffuse.proportion, 
    transmittivity, curvature.correction, 
    refractivity.coefficient}{Arguments to be passed 
       to the Python geoprocessing tool. See ArcGIS help files 
       (link below) for information on the usage of scripting 
       commands and their arguments.}
  \item{in.observer.features}{Name of shapefile (including 
    file extension \code{".shp"}) with observer point or
    polygon features, or \code{data.frame} with observer point
    features. If this is a \code{data.frame}, it will be written to
    a temporary shapefile in folder \code{tmpdir}, and x and y
    coordinates will be taken from the attributes identified by
    \code{x.field} and \code{y.field} }
  \item{x.field, y.field}{(optional) names of x and y coordinates
    if \code{in.observer.features} is a \code{data.frame}}
  \item{tmpdir}{(optional) name of folder for temporary files
    (when \code{in.observer.features} is a \code{data.frame} }
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
\item{AreaSolarRadiation}{\url{http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Area_Solar_Radiation}}
\item{ViewShed}{\url{http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Viewshed}}
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
