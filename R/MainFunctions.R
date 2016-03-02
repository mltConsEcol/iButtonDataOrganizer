############################################################
#' Read Temperature Data Files from Folder
#'
#' Imports all iButton temperature data from a specified folder, and aligns them to desired specifications based on rounding and start/end dates and times.
#'
#' @export
#' @param path Desired folder
#' @param rounding Desired Rounding (options: '10min', '1hr', '2hrs')
#' @param StartDate First desired date (and time) of the dataset (in format 'YYYY-MM-DD' or 'YYY-MM-DD HH:MM:SS' (in 24 hour time))
#' @param EndDate Last desired date (and time) of the dataset (in format 'YYYY-MM-DD' or 'YYY-MM-DD HH:MM:SS' (in 24 hour time))
#' @param DailyStartTime If only a portion of the day is desired, the start daily start time (in format 'HH:MM:SS' (in 24 hour time))
#' @param DailyEndTime If only a portion of the day is desired, the start daily end time (in format 'HH:MM:SS' (in 24 hour time))
#' @param exceltime Indicates whether function should read the date and time stamp as formatted when an iButton file is saved in Microsoft Excel, as it changes the format. Default is FALSE.
#' @author Mike Treglia \email{mtreglia@@gmail.com}
#' @return zoo object.
#' @examples
#'     packagePath <- find.package("iButtonDataOrganizer", lib.loc=NULL, quiet = TRUE)
#'     pathToFolder <- paste(packagePath, "/extdata/iButtonData_JulOct2014_2hr", sep="")
#'
#'     # Note: Time can also be included as part of the date, in this example, only date is used;
#'     # Date/Time formatted as "YYYY-MM-DD HH-MM-SS", based on 24 hour time
#'     JulOct2014 <- ReadiButtonFolder(path=pathToFolder, rounding="2hrs",
#'      StartDate="2014-07-26", EndDate="2014-10-31")
#'
#'     ## Users can write output to CSV file a CSV file with Date in the first column:
#'     # write.csv(as.data.frame(JulOct2014), "JulOct2014.csv")

ReadiButtonFolder <- function(path=path, rounding=rounding, StartDate, EndDate, DailyStartTime, DailyEndTime, exceltime=FALSE){
  library(zoo)
  library(xts)

  if(missing(path) | is.character(path)==FALSE)(stop("Make sure that the folder path is specified correctly"))

  if(rounding != "10min" & rounding != "1hr" & rounding != "2hrs" & !missing(rounding))(stop("Rounding arguments can only be '10min', '1hr', or '2hrs'"))

  AllTempData <- lapply(list.files(path=path, pattern='*.csv', full.names=TRUE), read.csv, skip=14) #Import all temperature files in path

  AllTempData <-  lapply(AllTempData, function(AllTempData) {
    if(exceltime==TRUE){
      AllTempData$Time <- as.POSIXct(strptime(as.character(paste(AllTempData[,1])), format="%m/%d/%Y\ %R", tz=""))
    } else 	AllTempData$Time <- as.POSIXct(strptime(as.character(paste(AllTempData[,1])), format="%m/%d/%y\ %I:%M:%S\ %p", tz=""))
    AllTempData <- AllTempData[,3:4]
  })

tryCatch({
##############
#For Rounding
	if(rounding=="10min"){
		for(i in 1:length(AllTempData)){
			for (j in 1:nrow(AllTempData[[i]])){
				if(as.POSIXlt(AllTempData[[i]]$Time[j])$min >= 10){
					if((as.POSIXlt(AllTempData[[i]]$Time[j])$min/5) %% 1 == 0){
					AllTempData[[i]]$Time[j] <- AllTempData[[i]]$Time[j] + 60
					AllTempData[[i]]$Time[j] <- AllTempData[[i]]$Time[j] - (60*as.POSIXlt(AllTempData[[i]]$Time[j])$min) + (60*signif(as.POSIXlt(AllTempData[[i]]$Time[j])$min,1)) - (as.POSIXlt(AllTempData[[i]]$Time[j])$sec)
					}
				else #if((as.POSIXlt(AllTempData[[i]]$Time[j])$min/5) %% 1 != 0)
					{
					AllTempData[[i]]$Time[j] <- AllTempData[[i]]$Time[j] - (60*as.POSIXlt(AllTempData[[i]]$Time[j])$min) + (60*signif((as.POSIXlt(AllTempData[[i]]$Time[j])$min),1)) - (as.POSIXlt(AllTempData[[i]]$Time[j])$sec)
					}
				}
			else if(as.POSIXlt(AllTempData[[i]]$Time[j])$min >= 5 & as.POSIXlt(AllTempData[[i]]$Time[j])$min <10){
					AllTempData[[i]]$Time[j] <- AllTempData[[i]]$Time[j] - (60*as.POSIXlt(AllTempData[[i]]$Time[j])$min)+(60*10)-(as.POSIXlt(AllTempData[[i]]$Time[j])$sec)
					}
			else if(as.POSIXlt(AllTempData[[i]]$Time[j])$min <5){
				AllTempData[[i]]$Time[j] <- AllTempData[[i]]$Time[j] - (60*as.POSIXlt(AllTempData[[i]]$Time[j])$min)-(as.POSIXlt(AllTempData[[i]]$Time[j])$sec)
					}
				}
		}
	}
	else if (rounding=="2hrs"){
		for(i in 1:length(AllTempData)){
			if(as.POSIXlt(AllTempData[[i]]$Time[[nrow(AllTempData[[i]])-24]])$hour %% 2 != 0) #Looks at 4 days before last row for odd/even times; this avoids problems driven by different start dates when one is pre-daylight savings time.
				{
				AllTempData[[i]]$Time <- AllTempData[[i]]$Time + 3600 - (as.POSIXlt(AllTempData[[i]]$Time)$min*60) - (as.POSIXlt(AllTempData[[i]]$Time)$sec)
				}
			else{
				AllTempData[[i]]$Time <- AllTempData[[i]]$Time - (as.POSIXlt(AllTempData[[i]]$Time)$min*60) - (as.POSIXlt(AllTempData[[i]]$Time)$sec)
				}
				}}
	else if (rounding=="1hr"){
		for(i in 1:length(AllTempData)){
			if(as.POSIXlt(AllTempData[[i]]$Time[[nrow(AllTempData[[i]])]])$min >= 30) #Looks at 4 days before last row for odd/even times; this avoids problems driven by different start dates when one is pre-daylight savings time.
				{
				AllTempData[[i]]$Time <- AllTempData[[i]]$Time + 3600 - (as.POSIXlt(AllTempData[[i]]$Time)$min*60) - (as.POSIXlt(AllTempData[[i]]$Time)$sec)
				}
			else{
				AllTempData[[i]]$Time <- AllTempData[[i]]$Time - (as.POSIXlt(AllTempData[[i]]$Time)$min*60) - (as.POSIXlt(AllTempData[[i]]$Time)$sec)
				}
				}}
	##Adding code for having aligned data [collected at same time]
	#else if (missing(rounding)){
	#AllTempData <- AllTempData
	#}
			} ,
			error=function(cond){
			message("An error occurred. Check that all files in the specified folder are formatted correctly, simply as .csv files downloaded from iButtons")
			message("Here's the original error message:")
			message(cond)
			})


######
#Convert AllTempData to zoo object, then to xts object, then subset it by the DateRange and Time range if needed
AllTempData <-  lapply(AllTempData, function(AllTempData) {
		AllTempData <- zoo::read.zoo(AllTempData, index.column=2, sep = "\t", header=TRUE, format="%Y-%m-%d %H:%M:%S", FUN=as.POSIXct)
		AllTempData <- xts::as.xts(AllTempData)
		AllTempData <- AllTempData[paste(StartDate,"/",EndDate, sep="")]
	})

	if (missing(DailyStartTime) & missing(DailyEndTime)){  #DailyStartTime, DailyEndTime
	AllTempData <- AllTempData
	} else if (missing(DailyStartTime) | missing(DailyEndTime)){
	stop("If specifying daily time frames, must provide start and end times")
	} else{
	AllTempData <- lapply(AllTempData, function(AllTempData){
		AllTempData <- AllTempData[paste("T",DailyStartTime,"/","T",DailyEndTime, sep="")]
		})
	}

AllTempDataDF <- na.omit(do.call(cbind, lapply(AllTempData, zoo::as.zoo)))

filenames <- sub("([^.]+)\\.[[:alnum:]]+$", "\\1", list.files(path=path,pattern='*.csv'))
colnames(AllTempDataDF) <- paste(filenames)

return(AllTempDataDF)
}


############################################################
#' Read Individual iButton File
#'
#' Import an individual iButton data file as a zoo object.
#'
#' @export
#' @param path Desired file with the data.
#' @param StartDate First desired date (and time) of the dataset (in format 'YYYY-MM-DD' or 'YYY-MM-DD HH:MM:SS' (in 24 hour time))
#' @param EndDate Last desired date (and time) of the dataset (in format 'YYYY-MM-DD' or 'YYY-MM-DD HH:MM:SS' (in 24 hour time))
#' @param DailyStartTime If only a portion of the day is desired, the start daily start time (in format 'HH:MM:SS' (in 24 hour time))
#' @param DailyEndTime If only a portion of the day is desired, the start daily end time (in format 'HH:MM:SS' (in 24 hour time))
#' @param exceltime Indicates whether function should read the date and time stamp as formatted when an iButton file is saved in Microsoft Excel, as it changes the format. Default is FALSE.
#' @author Mike Treglia \email{mtreglia@@gmail.com}
#' @return zoo object
#' @examples
#'     packagePath <- find.package("iButtonDataOrganizer", lib.loc=NULL, quiet = TRUE)
#'     pathToFile <- paste(packagePath,
#'      "/extdata/iButtonData_JulOct2014_2hr/site2_logger1.csv", sep="")
#'
#'	   site2logger1 <- ReadiButtonFile(pathToFile)
#'
#'     # An example of importing a dataset where the time stamps were re-formatted by Microsoft Excel
#'     packagePath <- find.package("iButtonDataOrganizer", lib.loc=NULL, quiet = TRUE)
#'     pathToFile <- paste(packagePath,
#'      "/extdata/Sample_2hr_ExcelTime.csv", sep="")
#'
#'	   site2logger1_excel <- ReadiButtonFile(pathToFile, exceltime=TRUE)
#'

ReadiButtonFile <- function(path, StartDate, EndDate, DailyStartTime, DailyEndTime, exceltime=FALSE){

	library(zoo)
	library(xts)

	if(missing(path) | is.character(path)==FALSE)(stop("Make sure that the folder path/file name is specified correctly"))

	iButtonData <- read.csv(path, skip=14)

	if(exceltime==TRUE){
	iButtonData$Time <- as.POSIXct(strptime(as.character(paste(iButtonData[,1])), format="%m/%d/%Y\ %R", tz=""))
	} else iButtonData$Time <- as.POSIXct(strptime(as.character(paste(iButtonData[,1])), format="%m/%d/%y\ %I:%M:%S\ %p", tz=""))
	iButtonData <- iButtonData[,3:4]
	iButtonData <- zoo::read.zoo(iButtonData, index.column=2, sep = "\t", header=TRUE, format="%Y-%m-%d %H:%M:%S", FUN=as.POSIXct)
	iButtonData <- xts::as.xts(iButtonData)

	if(hasArg(StartDate) | hasArg(EndDate)){
	iButtonData <- iButtonData[paste(StartDate,"/",EndDate, sep="")]
	}
	if (missing(DailyStartTime) & missing(DailyEndTime)){  #DailyStartTime, DailyEndTime
		iButtonData <- iButtonData
	} else if (missing(DailyStartTime) | missing(DailyEndTime)){
		stop("If specifying daily time frames, must provide start and end times")
	} else{
		iButtonData <- lapply(iButtonData, function(iButtonData){
		iButtonData <- iButtonData[paste("T",DailyStartTime,"/","T",DailyEndTime, sep="")]
		})
	}

	iButtonData <- zoo::as.zoo(iButtonData)

}



############################################################
#' Join Multi-Temporal iButton Datasets
#'
#' Join a list of objects created by ReadiButtonFolder into a single dataset as a zoo object
#'
#' @export
#' @param datalist List of objects created using ReadiButtonFolder
#' @author Mike Treglia \email{mtreglia@@gmail.com}
#' @return zoo object
#' @examples
#'     packagePath <- find.package("iButtonDataOrganizer", lib.loc=NULL, quiet = TRUE)
#'     pathToFolder1 <- paste(packagePath, "/extdata/iButtonData_JulOct2014_2hr", sep="")
#'     pathToFolder2 <- paste(packagePath, "/extdata/iButtonData_NovDec2014_2hr", sep="")
#'
#'     #Note: Time can also be included as part of the date; Date/Time
#'     #formatted as "YYYY-MM-DD HH-MM-SS", based on 24 hour time
#'     JulOct2014 <- ReadiButtonFolder(path=pathToFolder1, rounding="2hrs",
#'      StartDate="2014-07-26", EndDate="2014-10-31")
#'     NovDec2014 <- ReadiButtonFolder(path=pathToFolder2, rounding="2hrs",
#'      StartDate="2014-11-03", EndDate="2014-12-12")
#'     iButtonDatasets <- list(JulOct2014, NovDec2014)
#'
#'     CompilediButtonData <- JoiniButtonDatasets(iButtonDatasets)

JoiniButtonDatasets <- function(datalist){
library(gtools)
  for(i in 1:length(datalist)){
    datalist[[i]] <- data.frame(Date=as.character(time(datalist[[i]])), datalist[[i]], check.names=FALSE, row.names=NULL)
  }
  AllDat <- do.call(gtools::smartbind,datalist)
  AllDat <- zoo::read.zoo(AllDat, index.column=1, sep = "\t", header=TRUE, format="%Y-%m-%d %H:%M:%S", FUN=as.POSIXct)

  return(AllDat)
}
