############################################################
#' Calcualte Temperature Statistics by Day
#'
#' Calculate various statistics from iButton datasets, by day.
#'
#' @export
#' @param dataname Name of time-series data, generally a set of iButton data files imported through ReadiButtonFolder
#' @param stat Desired statistic to be calcualted as a daily aggregate. Can be any of the following: "var", "mean", "max", "min", "sd", or in format of: "function(x) var(x)"
#' @author Mike Treglia \email{mtreglia@@gmail.com}
#'
#' @examples
#'     packagePath <- find.package("iButtonDataOrganizer", lib.loc=NULL, quiet = TRUE)
#'     pathToFolder <- paste(packagePath, "/extdata/iButtonData_JulOct2014_2hr", sep="")
#'
#'     JulOct2014 <- ReadiButtonFolder(path=pathToFolder, rounding="2hrs",
#'      StartDate="2014-07-26", EndDate="2014-10-31")
#'
#'     #Calculate the Daily Variance for each logger from which the data were imported
#'     JulOct2014.DailyVar <- temp.agg.daily(JulOct2014, var)


#Dataname should be an xts object; stat can be any of the following: var, mean, max, min, sd, or in format of: "function(x) var(x)"
temp.agg.daily <- function(dataname, stat){
  library(xts)
  library(zoo)
	agg <- as.list(rep("", ncol(dataname)))
	for (i in 1:ncol(dataname))
		{
		agg[[i]]<-data.frame(ivec = 1:i)
		agg[[i]]<-zoo::as.zoo(xts::apply.daily(xts::as.xts(dataname[,i]),stat))
		}
	agg <- do.call(cbind, lapply(agg, zoo::as.zoo))
	colnames(agg)<-colnames(dataname)

	return(agg)
	}
