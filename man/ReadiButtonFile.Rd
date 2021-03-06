% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MainFunctions.R
\name{ReadiButtonFile}
\alias{ReadiButtonFile}
\title{Read Individual iButton File}
\usage{
ReadiButtonFile(path, StartDate, EndDate, DailyStartTime, DailyEndTime,
  exceltime = FALSE, hdlen = 14)
}
\arguments{
\item{path}{Desired file with the data.}

\item{StartDate}{First desired date (and time) of the dataset (in format 'YYYY-MM-DD' or 'YYY-MM-DD HH:MM:SS' (in 24 hour time))}

\item{EndDate}{Last desired date (and time) of the dataset (in format 'YYYY-MM-DD' or 'YYY-MM-DD HH:MM:SS' (in 24 hour time))}

\item{DailyStartTime}{If only a portion of the day is desired, the start daily start time (in format 'HH:MM:SS' (in 24 hour time))}

\item{DailyEndTime}{If only a portion of the day is desired, the start daily end time (in format 'HH:MM:SS' (in 24 hour time))}

\item{exceltime}{Indicates whether function should read the date and time stamp as formatted when an iButton file is saved in Microsoft Excel, as it changes the format. Default is FALSE.}

\item{hdlen}{Specifies the length of the header, as not all iButton data files have same header length. For example, model DS1921G has a header of 14 lines (which is the default for the function), while DS1922L has a header of 19 lines. This can be determined by opening an iButton data file in a text editor and counting how many lines are before the column names for the data.}
}
\value{
zoo object
}
\description{
Import an individual iButton data file as a zoo object.
}
\examples{
    packagePath <- find.package("iButtonDataOrganizer", lib.loc=NULL, quiet = TRUE)
    pathToFile <- paste(packagePath,
     "/extdata/iButtonData_JulOct2014_2hr/site2_logger1.csv", sep="")

   site2logger1 <- ReadiButtonFile(pathToFile)

    # An example of importing a dataset where the time stamps were re-formatted by Microsoft Excel
    packagePath <- find.package("iButtonDataOrganizer", lib.loc=NULL, quiet = TRUE)
    pathToFile <- paste(packagePath,
     "/extdata/Sample_2hr_ExcelTime.csv", sep="")

   site2logger1_excel <- ReadiButtonFile(pathToFile, exceltime=TRUE)

}
\author{
Mike Treglia \email{mtreglia@gmail.com}
}

