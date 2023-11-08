\name{readGeo_CM}

\alias{readGeo_CM}

\title{Reading the weather and soil data for crop model and transforming it to DSSAT format}

\usage{
readGeo_CM(country, useCaseName, Crop, AOI = FALSE, season = 1)
}

\arguments{
\item{country}{country name}
\item{useCaseName}{use case name    name}
\item{Crop}{the name of the crop to be used in creating file name to write out the result.}
\item{AOI}{True if the data is required for target area, and false if it is for trial sites}
\item{season}{when data is needed for more than one season, this needs to be provided to be used in the file name}
}

\value{
weather and soil data in DSSAT format
}

\description{
Reading the weather and soil data for crop model and transforming it to DSSAT format
}

\examples{
readGeo_CM(country = "Rwanda",	useCaseName = "RAB", Crop = "Maize", AOI = FALSE, season=1)
}
