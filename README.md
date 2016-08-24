# iButtonDataOrganizer

## Installation and General Usage

This package is designed to facilitate processing and organization of datafiles from iButton temperature dataloggers. To install the package, you will need the *devtools* R package, which can be installed using `install.packages("devtools")`. Then, follow the example below for installation.

```
library(devtools)
install_github("mltconsecol/iButtonDataOrganizer")
```

There are four main functions currently in the package - brief descriptions of them are below, but users should use `help()` for details and examples
  
  * `ReadiButtonFolder()` - which imports many files of iButton Data from a single folder, aligns the time to 1 minute, 10 minutes, 15 minutes, 30 minutes, 1 hour, or 2 hours.
  * `ReadiButtonFile()` - which imports a single file of iButton Data. The resulting dataset is a ZOO object;
  * `JoiniButtonDatasets()` - which joins multiple sets of data created using the `ReadiButtonFolder()`; and
  * `temp.agg.daily()` - which calculates various statistics (e.g., mean, variance, etc.) by day, for a dataset created using `ReadiButtonFolder()`.

*This work is authored by [Mike Treglia](http://mltconsecol.github.io/). Initial development was been funded by the the [NSF Oklahoma EPSCoR program](http://www.okepscor.org/) (Grant No. IIA-1301789).*


### Update Notes

#### 24 August 2016:

* Agument 'hdlen' added to `ReadiButtonFile()` and `ReadiButtonFolder()`, which allows user to set the length of the header from the data files (default is 14 lines, which works with iButton model DS1921G).
* Added more rounding options for `ReadiButtonFolder()`.
	