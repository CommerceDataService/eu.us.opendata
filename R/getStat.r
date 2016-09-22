#' Return data from the eurostat and bea APIs
#' 
#' @param statID 	 ID of the statistic requested
#' @return By default, an object of class 'data.table'
#' @import beaR rsdmx data.table
#' @export 
#' @examples 
#' DT <- getStat('gross domestic product')

getStat <- function(statID = 'all', NUTS = '2', apiKey = NULL, meta = FALSE, freq = 'A') { 
#Think we should set validate = TRUE for all requests
	if(class(beaSpec) != 'character'){
		warning('Please specify the ID or of the data you are looking for.')
		return(paste0('Invalid object class passed to beaGet([list of API parameters]): ', class(beaSpec), '. Should be of class "list"'))
	}
	requireNamespace('beaR', quietly = TRUE)
	requireNamespace('rsdmx', quietly = TRUE)
	requireNamespace('data.table', quietly = TRUE)
	
	if(NUTS == '2'){
		beaGeo <- 'STATE'
	} else {
		if(NUTS == '3'){
			beaGeo <- 'MSA'
		}
	}
#	beaSpec <- list(
#		'userID' = apiKey
#	)
	
	esData <- readSDMX(providerId = "ESTAT", resource = "data", flowRef = statID)
	


}
