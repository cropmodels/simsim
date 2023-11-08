\name{create_filex}

\alias{create_filex}

\title{Create one experimental file (repetitive function)}

\description{
Create one experimental file (repetitive function)
}


\usage{
create_filex(i,path.to.temdata,filex_temp, path.to.extdata, coords, AOI, code, plantingWindow, number_years, ingenoid)
}

\arguments{
\item{i}{point/folder from a list}
\item{path.to.temdata}{directory with template weather and soil data in DSSAT format}
\item{filex_temp}{Template experimental file name in DSSAT format (FILEX)}
\item{path.to.extdata}{working directory to save the weather and soil data in DSSAT format}
\item{coords}{dataframe with the locations and metadata}
\item{AOI}{True if the data is required for target area, and false if it is for trial sites}
\item{code}{crop code in DSSAT format}
\item{plantingWindow}{number of weeks that define the planting window  considering the Planting_month_date as earliest planting week. It is given when several planting dates are to be tested to determine optimal planting date (applies to AOI)}
\item{number_years}{number of years the simulations are run for the AOI (it does not apply to the trial location data)}
\item{ingenoid}{?}
}


\examples{
create_filex(1)
}
