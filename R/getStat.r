#' Return data from the eurostat and bea APIs
#' 
#' @param statID 	 ID of the statistic requested
#' @return By default, an object of class 'data.table'
#' @import beaR rsmdx data.table
#' @export 
#' @examples 
#' DT <- getStat('gross domestic product')

getStat <- function(statID = 'all', geogID = 'all', apiKey = NULL, meta = FALSE) { 
	if(class(beaSpec) != 'character'){
		warning('Please specify the ID or of the data you are looking for.')
		return(paste0('Invalid object class passed to beaGet([list of API parameters]): ', class(beaSpec), '. Should be of class "list"'))
	}
	requireNamespace('beaR', quietly = TRUE)
	requireNamespace('rsmdx', quietly = TRUE)
	requireNamespace('data.table', quietly = TRUE)
	
	beaSpec <- list(
		'userID' = apiKey
	)
	
	if (meta) {
	 if (tolower(statID) == 'all'){
		esData <- readSDMX(
			providerId = "ESTAT", 
			agencyId="ESTAT", 
			resource = "dataflow", 
			resourceId = statID
		)
		beaData <- 

	 } else {
		esData <- readSDMX(
			providerId = "ESTAT", 
			agencyId="ESTAT", 
			resource = "datastructure", 
			resourceId = statID
		)
	 }
		
	} else {
		esData <- readSDMX(providerId = "ESTAT", resource = "data", flowRef = statID)
	}
	


}
