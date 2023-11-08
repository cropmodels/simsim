\name{dssat.expfile}

\alias{dssat.expfile}

\title{Create multiple experimental files}

\usage{
dssat.expfile(country, useCaseName, Crop, AOI = FALSE, filex_temp, Planting_month_date = NULL,
  Harvest_month_date = NULL, ID = "TLID", season = NULL, plantingWindow = 1, ingenoid)
}

\arguments{
\item{country}{country name}
\item{useCaseName}{use case name  name}
\item{Crop}{the name of the crop to be used in creating file name to write out the result.}
\item{AOI}{True if the data is required for target area, and false if it is for trial sites}
\item{filex_temp}{Name of the template experimental file in DSSAT format (FILEX)}
\item{Planting_month_date}{is needed only for AOI and should be provided as month_date, for trial locations the actual planting date is be used so no need to change the default value}
\item{Harvest_month_date}{if AOI is TRUE, Harvest_month_date should be provided in mm-dd format.  weather data across years between Planting_month_date and Harvest_month_date will be provided}
\item{ID}{trial ID}
\item{season}{when data is needed for more than one season, this needs to be provided to be used in the file name}
\item{plantingWindow}{number of weeks starting considering the Planting_month_date as earliest planting week. It is given when several planting dates are to be tested to determine optimal planting date}
\item{ingenoid}{?}
}

\description{
Create multiple experimental files
}

\examples{
dssat.expfile(country = "Rwanda",  useCaseName = "RAB", Crop = "Maize", AOI = FALSE, filex_temp="MZRL8142.MZX", Planting_month_date = NULL,jobs=10)
}

