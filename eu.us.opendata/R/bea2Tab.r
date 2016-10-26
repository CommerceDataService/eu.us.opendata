#' Convert BEA API httr response or list payload to data.table. . Method taken from https://github.com/us-bea/beaR/blob/master/R/bea2Tab.r  
#' 
#' @param beaPayload An object of class 'list' or httr 'response' returned from beaGet() call to BEA API
#' @param asWide 	 		Return data.table in wide format (default: TRUE)
#' @param iTableStyle If "asWide = TRUE", setting "iTableStyle = TRUE" will return data.table in same format as shown on BEA website, with dates and attributes as column headers and series as rows; otherwise, results have series codes as column headers (default: TRUE)
#' @description Convert BEA API httr response or list payload to data.table. Also, converts LONG data frame (default API format - see bea2List results) to WIDE data (with years as columns) by default 
#' @return An object of class 'data.table' containing data from beaGet(...) with custom attributes(BDT)$params.
#' @keywords internal
#' @import data.table
#' @export
#' @examples 
#' userSpecList <- list('UserID' = 'yourKey' ,
#'									'Method' = 'GetData',
#'									'datasetname' = 'NIPA',
#'									'Frequency' = 'A',
#'									'TableID' = '68',
#'									'Year' = 'X')
#' resp <- beaGet(userSpecList)
#' BDT <- bea2Tab(resp)

bea2Tab <- function(beaPayload, asWide = TRUE, iTableStyle = TRUE) {
	requireNamespace('data.table', quietly = TRUE)
	if('response' %in% class(beaPayload)){
		beaResponse <- eu.us.openR::bea2List(beaPayload)
	} else {
		beaResponse <- beaPayload
	}
	
	if('error' %in% tolower(
			attributes(beaResponse)$names
		)
	){
		return(beaResponse$Error$APIErrorDescription)
	}

	DataValue <- NULL
	TimePeriod <- NULL
	LineNumber <- NULL
	beaResults <- data.table::as.data.table(beaResponse)
	attributes(beaResults)$is.wide <- FALSE

	#Convert wide matrix to long 
	#(less common as data comes as long, but needed for beaViz)
	if('data.frame' %in% class(beaPayload)){
		if(
			attributes(beaPayload)$is.wide == TRUE && 
			!asWide
		) {

			beaTab <- beaResults
			id <- NULL
			dateColNames <- sort(attributes(beaTab)$names[
				grepl(
					'DataValue_', 
					attributes(beaTab)$names, 
					fixed = TRUE
				)
			])

			dateVector <- sort(gsub(
				'DataValue_',
				'',
				dateColNames
			))
			
			beaResults <- try(stats::reshape(
				beaTab, 
				varying = dateColNames, 
				v.names = 'DataValue', 
				timevar = 'TimePeriod', 
				times = dateVector, 
				direction = 'long')[, 
					id:=NULL
				]
			)
			
			attributes(beaResults)$is.wide <- FALSE
		}
	}
	#Convert long matrix to wide (if needed)
	if(
			asWide && 
			!is.null(attributes(beaResponse)$detail)
		){
		beaTab <- beaResults
		data.table::setkey(beaTab, key = TimePeriod)
		noDV <- attributes(beaTab)$names != 'DataValue'
		noTS <- attributes(beaTab)$names != 'TimePeriod'
		noNotes <- attributes(beaTab)$names != 'NoteRef'

		#A weird fix to push NA values down to bottom for reshaping
		beaTab[, DataValue := ifelse(is.na(DataValue), 0, DataValue)]
		
#		beaResults <- try(stats::reshape(
#			beaTab, 
#			timevar = 'TimePeriod', 
#			idvar = attributes(beaTab)$names[noDV & noTS & noNotes], 
#			direction = 'wide')
#		)
		eval(
			parse(
				text=paste0(
					'beaResults <- data.table::dcast(data.table::melt(beaTab, measure = "DataValue"),', 
					paste(
						attributes(beaTab)$names[noDV & noTS & noNotes], 
						collapse='+'
					),
					' ~ variable + TimePeriod)'
				)
			)
		)
		if(
			any(
				tolower(
					attributes(beaResponse)$params$ParameterValue
				) %in% 
					c('nipa', 'niunderlyingdetail', 'fixedassets')
			)
		){
			beaResults <- beaResults[order(as.numeric(LineNumber))]
		}
		attributes(beaResults)$is.wide <- TRUE
		if (!iTableStyle){
			beaTrans <- beaResults 
			
	#		beaStrMatrix <-  t(
			beaColHeaders <-  
				eval(
					parse(
	#					text = paste0('beaTrans[ , .(', paste(
							text = paste0('beaTrans[ , paste(', paste(
							attributes(beaTrans)$names[
								!grepl('DataValue_', attributes(beaTrans)$names, fixed = T)
							], collapse = ','
						), ')]')
					)
				)
	#		)

			beaNumMatrix <-  t(
				eval(
					parse(
						text = paste0('beaTrans[ , .(', paste(
							sort(attributes(beaTrans)$names[
								grepl('DataValue_', attributes(beaTrans)$names, fixed = T)
							]), collapse = ','
						), ')]')
					)
				)
			)


	#		headRows <- data.table(beaStrMatrix)
	#		dataRows <- data.table(beaNumMatrix)
			
	#		beaResults <- rbindlist(list(headRows, dataRows))

			colnames(beaNumMatrix) <- beaColHeaders

			beaResults <- data.table(beaNumMatrix)
			beaResults[, TimePeriod := gsub('DataValue_', 
				'', attributes(beaTrans)$names[
					grepl('DataValue_', attributes(beaTrans)$names, fixed = T)
				], 
				fixed = TRUE
			)]	
			data.table::setkey(beaResults, key = TimePeriod)
		}
	}
	
	attributes(beaResults)$params <- attributes(beaResponse)$params
	attributes(beaResults)$detail <- attributes(beaResponse)$detail
	
	if(is.null(attributes(beaResults)$params)){
		warning('Request response data not found; returned values may not contain successful BEA API response.')
	}
	
	return(beaResults)
}
