\name{process_grid_element}

\alias{process_grid_element}

\title{Function that creates the soil and weather file for one location/folder}

\usage{
process_grid_element(i,country, path.to.extdata, path.to.temdata, Tmaxdata, Tmindata, Sraddata, Rainfalldata, RelativeHum, coords, Soil, AOI)
}

\arguments{
\item{i}{last digits of the folder (folder ID)}
\item{country}{country name}
\item{path.to.extdata}{working directory to save the weather and soil data in DSSAT format}
\item{path.to.temdata}{directory with template weather and soil data in DSSAT format}
\item{Tmaxdata}{dataframe with the maximum data for all the locations}
\item{Tmindata}{dataframe with the minimum temperature data for all the locations}
\item{Sraddata}{dataframe with the solar radiation data for all the locations}
\item{Rainfalldata}{dataframe with the rainfall data for all the locations}
\item{RelativeHum}{dataframe with the relative humidity data for all the locations}
\item{coords}{dataframe with the locations and metadata}
\item{Soil}{dataframe with the soil data information}
\item{AOI}{True if the data is required for target area, and false if it is for trial sites}
}

\value{
soil and weather file in DSSAT format
}

\description{
Function that creates the soil and weather file for one location/folder
}

\examples{
process_grid_element(1)
}
