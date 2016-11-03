#' Middle- or back-end-y method for .sysdata: NOT RUN 
#' 
#' @keywords internal
#' @return Shapefile to store in .sysdata
#' @import rgdal devtools
#' @export 
#' 
#' 

loadshp <- function(){
  requireNamespace('rgdal', quietly = TRUE)
  requireNamespace('devtools', quietly = TRUE)

#for the lazy windows user (requires install):
	localPath <- paste0('C:/users/', Sys.info()[[7]], '/documents/github/eu.us.opendata/inst/extdata/shp')
  #Go through each geo levels
  eu.us.od.County = rgdal::readOGR(dsn =localPath, layer = "USCounty")
	eu.us.od.State =  rgdal::readOGR(dsn =localPath, layer = "USState")
	eu.us.od.MSA = rgdal::readOGR(dsn =localPath, layer = "USMSA")
	eu.us.od.NUTS1 = rgdal::readOGR(dsn =localPath, layer = "NUTS1")
	eu.us.od.NUTS2 = rgdal::readOGR(dsn =localPath, layer = "NUTS2")
	eu.us.od.NUTS3 = rgdal::readOGR(dsn =localPath, layer = "NUTS3")
	eu.us.od.Metro = rgdal::readOGR(dsn =localPath, layer = "NUTMetro") 
	oldwd <- getwd();
	setwd(paste0('C:/users/', Sys.info()[[7]], '/documents/github/eu.us.opendata'))
	devtools::use_data(
		eu.us.od.County	,
		eu.us.od.State 	,
		eu.us.od.MSA  	,
		eu.us.od.NUTS1 	,
		eu.us.od.NUTS2 	,
		eu.us.od.NUTS3 	,
		eu.us.od.Metro  , 
		pkg = 'eu.us.opendata', overwrite = TRUE, internal = TRUE)
		setwd(oldwd)
}	
	
