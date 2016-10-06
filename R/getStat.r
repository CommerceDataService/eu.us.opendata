#' Return data from the eurostat and bea APIs
#' 
#' @param statID 	 ID of the statistic requested
#' @return By default, an object of class 'data.table'
#' @import beaR rsdmx data.table
#' @export 

getStat <- function(statID = '', NUTS = '2', euParms = list(), usParms = list(), apiKey = NULL, meta = FALSE, freq = 'A') { 
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
	
	if(!meta){
		esData <- rsdmx::readSDMX(providerId = "ESTAT", resource = "data", flowRef = statID)
	} else {
		esPreData <- rsdmx::readSDMX(providerId = "ESTAT", resource = "datastructure", resourceId = paste0('DSD_',statID))
		esData <- data.table::as.data.table(esPreData@datastructures@datastructures[[1]]@Components)
	}
#e.g.,
#nama_10r_2gdp	

		usData <- beaR::beaGet(list(
			'userID' = apiKey,
			'datasetname' = ifelse()
		))
}
