#' Check to see if metadata has been updated
#' 
#' @param beaKey Character string representation of user API key. Necessary for first time use and updates; recommended for anything beyond one-off searches from the console.
#' @keywords cache
#' @return Boolean (should we update?)
#' @import data.table 
#' @export
#' @examples 
#' beaMetaCheck();

 beaMetaCheck<- function(beaKey){ 
#TO DO: Do initial timestamp check, then post-func timestamp check. Just use that for your bool out
#beaSearch throws spurious NOTEs on check() without this due to data.table Depends
	boolOut <- FALSE
 'LineDescription' <- NULL
 'SeriesCode'      <- NULL
 'Key'             <- NULL
 'LineNumber'      <- NULL
 'Tier'            <- NULL
 'ParentLine'      <- NULL
 'Desc'     			 <- NULL
 'DatasetName'     <- NULL
 'Dataset'		     <- NULL
 'TableID'         <- NULL
 'TableName'       <- NULL
 'Parameter'       <- NULL
 'APImtime'        <- NULL
 'mtime' 	      	 <- NULL
 'Account'         <- NULL
 '.'               <- NULL
 'apiCall'         <- NULL
 'nipaIndex'       <- NULL
 'fixaIndex'       <- NULL
 'niudIndex'       <- NULL
 'rdatIndex'       <- NULL
 'rprdIndex'       <- NULL
 'rincIndex'       <- NULL
 'JSONUpdateDate'  <- NULL
 'XMLUpdateDate'   <- NULL

	requireNamespace('data.table', quietly = TRUE)
	beaMetadataStore <- paste0(.libPaths()[1], '/euroStates/rawdata')
	
	beaMetaFiles <- list.files(path = beaMetadataStore, full.names = TRUE);
	beaMetaFilesTimes <- file.info(beaMetaFiles, extra_cols = TRUE)
	beaMetaFilesTimes$Dataset <- gsub(
		paste0(beaMetadataStore, '/'), 
		'', 
		attributes(beaMetaFilesTimes)$row.names, 
		fixed=T
	)
	beaMetaMtime <- data.table::as.data.table(beaMetaFilesTimes)[, 
		.(
			Dataset = gsub('.RData', '', Dataset, fixed=T), 
			mtime
		)
	]
	data.table::setkey(beaMetaMtime, key = Dataset)
	
	#Temporarily remove FixedAssets for V1
	beaKnownMetaSets <- list(
#		'nipa',
#		'niunderlyingdetail',
#		'fixedassets',
		'regionaldata',
		'regionalproduct',
		'regionalincome'
	)

	if ((length(beaMetaFiles) == 0) & is.null(beaKey)){
		warning(paste0('No API key provided and no local metadata storage detected in ', beaMetadataStore, '. 
		Please provide a valid key to use beaSearch.'))
		return(paste0('No API key provided and no local metadata storage detected in ', beaMetadataStore, '. Please provide a valid key to use beaSearch.'))
	}
#Check to see if this is the first time using the search function; if so, update all metadata currently handled.
	if (length(beaMetaFiles) < 5){
	#Create directory and make single call to get all metadata if there are missing meta .RData files
		message('Creating first-time local copy of metadata for all datasets - only done once.')
		message('Datasets will be updated only if timestamps indicate metadata obsolete in future searches,')
		message("and only obsolete metadata sets will be updated (it's faster this way).")
		message("")
		dir.create(beaMetadataStore, showWarnings = FALSE, recursive = TRUE)
		boolOut <- TRUE
		#call function to update metadata - remember to specify beaR namespace
		beaUpdateMetadata(beaKnownMetaSets,	beaKey)
		
	} else {
	 if (!is.null(beaKey)){
		#Make a "GetParameterValues" call to get timestamps of latest metadata update
		beaMetaTimeSpec <- list(
			'UserID' = beaKey ,
			'method' = 'GetParameterValues',
			'datasetname' = 'APIDatasetMetaData',
			'parametername' = 'dataset',
			'ResultFormat' = 'json'
		)
		#Get metadata response with timestamps we need to check for updates as list
		beaMetaParams <- beaGet(beaMetaTimeSpec, asList = TRUE, isMeta = TRUE)	
		
		beaMetaInfo <- data.table::as.data.table(beaMetaParams$ParamValue)
		
		data.table::setkey(beaMetaInfo, key = Dataset)
		
		tryCatch({
		#If JSON has been updated, set check param = false
		
			timeCompare <- beaMetaMtime[beaMetaInfo][, .(
				Dataset, 
				mtime, 
				APImtime = as.POSIXct(
					JSONUpdateDate, 
					format = "%Y-%m-%dT%H:%M:%S"
				)
			)][!is.na(APImtime)]

			outdatedLocalMeta <- timeCompare[
				(is.na(mtime) & !is.na(APImtime)) | 
				APImtime > mtime,
				Dataset
				]
			
			beaMetaFirstToCache <- FALSE 
			if(length(timeCompare[is.na(APImtime) & Dataset %in% beaKnownMetaSets, Dataset]) > 0){
				beaMetaFirstToCache <- TRUE
			}
		},
		error = function(e){
			beaMetaFirstToCache <- TRUE
			beaUpdateMetadata(beaKnownMetaSets,	beaKey)
		}, 
		finally = {''})

		if(length(outdatedLocalMeta[!tolower(outdatedLocalMeta) %in% beaKnownMetaSets]) > 0){
			warning('BEA API contains newly-available metadata for datasets not handled.
			This version of beaR is either not the latest, or will soon be replaced.')
			outdatedLocalMeta <- outdatedLocalMeta[tolower(outdatedLocalMeta) %in% beaKnownMetaSets]
		}

		if(beaMetaFirstToCache){
			beaUpdateMetadata(beaKnownMetaSets,	beaKey)
			boolOut <- TRUE
		} else {
			if(length(outdatedLocalMeta) > 0){
				beaUpdateMetadata(as.list(tolower(outdatedLocalMeta)),	beaKey)
				boolOut <- TRUE
			}
		}
	 }
	}

	beaMetaFiles <- list.files(path = beaMetadataStore, full.names = TRUE);

#Temporarily remove FixedAssets from V1
	if(
#		length(grep('FixedAssets', beaMetaFiles, fixed = TRUE)) == 0 | 
#		length(grep('NIPA', beaMetaFiles, fixed = TRUE)) == 0 | 
#		length(grep('NIUnderlyingDetail', beaMetaFiles, fixed = TRUE)) == 0 | 
		length(grep('RegionalData', beaMetaFiles, fixed = TRUE)) == 0 | 
		length(grep('RegionalProduct', beaMetaFiles, fixed = TRUE)) == 0 | 
		length(grep('RegionalIncome', beaMetaFiles, fixed = TRUE)) == 0 
	){
			warning(paste0('Metadata is missing from ',beaMetadataStore,' and may be locked for updating on the BEA API; please try again later.'))
			return(paste0('Metadata is missing from ',beaMetadataStore,' and may be locked for updating on the BEA API; please try again later.'))
 }
 
 return(boolOut)
}
 