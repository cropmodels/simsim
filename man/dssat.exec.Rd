\name{dssat.exec}

\alias{dssat.exec}

\title{Main function that define the files to run DSSAT}

\usage{
dssat.exec(country,useCaseName, Crop, AOI = FALSE, TRT = 1, cultivarType = "mediumDuration")
}

\arguments{
\item{country}{country name}
\item{useCaseName}{use case name  name}
\item{Crop}{the name of the crop to be used in creating file name to write out the result.}
\item{AOI}{True if the data is required for target area, and false if it is for trial sites}
\item{TRT}{is the number of treatments to run from the experimental file}
}

\description{
Main function that define the files to run DSSAT
}

\examples{
dssat.exec(country = "Rwanda",  useCaseName = "RAB", Crop = "Maize", AOI = FALSE, TRT=1:36)
}

