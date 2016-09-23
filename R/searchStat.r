#' Return data from the eurostat and bea APIs
#' 
#' @param statID 	 ID of the statistic requested
#' @return By default, an object of class 'data.table'
#' @import beaR rsmdx data.table
#' @export 
#' @examples 
#' DT <- getStat('gross domestic product')

searchStat <- function(statID = 'all', geogID = 'all', apiKey = NULL, meta = FALSE) { 

		esData <- readSDMX(
			providerId = "ESTAT", 
			agencyId="ESTAT", 
			resource = "dataflow", 
			resourceId = statID
		)
