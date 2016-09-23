#' Search metadata from the eurostat and bea APIs
#' 
#' @param statID 	 ID of the statistic requested
#' @return By default, an object of class 'data.table'
#' @import beaR rsdmx data.table
#' @export 
#' @examples 
#' statSearch('gross domestic product')

statSearch <- function(searchTerm, apiKey = NULL){
#Think we should set validate = TRUE for all requests

	requireNamespace('beaR', quietly = TRUE)
	requireNamespace('rsdmx', quietly = TRUE)
	requireNamespace('data.table', quietly = TRUE)

	localMeta <- paste0(.libPaths()[1], '/euroState/data')

	tryCatch({
		load(paste0(localMeta, '/euMetadata.RData'))
	}, error = function(e){
		dir.create(localMeta, showWarnings = FALSE, recursive = TRUE)
			esDataFlow <- rsdmx::readSDMX(
				providerId = "ESTAT", 
				agencyId="ESTAT", 
				resource = "dataflow", 
				resourceId = "all"
			)		
			esData <- data.table::as.data.table(esDataFlow)[,.(
				id, 
				Ref = dsdRef,
				Name.fr,
				Name.en,
				Name.de
			)]		

		save(esDataFlow, file = paste0(localMeta, '/euFlow.RData'))
		save(esData, file = paste0(localMeta, '/euMetadata.RData'))
		}, finally = {''})
		
		nut2Dash <- esData[
			grep('NUTS-2', 
				paste(
					toupper(Name.en), 
					toupper(Name.fr), 
					toupper(Name.de)
				), fixed = T
			)
		]
		nut2Spce <- esData[
			grep('NUTS 2', 
				paste(
					toupper(Name.en), 
					toupper(Name.fr), 
					toupper(Name.de)
				), fixed = T
			)
		]
		nut2Comp <- esData[
			grep('NUTS2', 
				paste(
					toupper(Name.en), 
					toupper(Name.fr), 
					toupper(Name.de)
				), fixed = T
			)
		]

		euN2 <- unique(data.table::rbindlist(list(nut2Dash, nut2Spce, nut2Comp)))
		euN2[, NUTS := '2']
		

		nut3Dash <- esData[
			grep('NUTS-3', 
				paste(
					toupper(Name.en), 
					toupper(Name.fr), 
					toupper(Name.de)
				), fixed = T
			)
		]
		nut3Spce <- esData[
			grep(
				'NUTS 3', 
				paste(
					toupper(Name.en), 
					toupper(Name.fr), 
					toupper(Name.de)
				), 
				fixed = T
			)
		]
		nut3Comp <- esData[
			grep('NUTS3', 
				paste(
					toupper(Name.en), 
					toupper(Name.fr), 
					toupper(Name.de)
				), fixed = T
			)
		]

		euN3 <- unique(data.table::rbindlist(list(nut3Dash, nut3Spce, nut3Comp)))
		euN3[, NUTS := '3']
		
		euMSA <- esData[
			grep(
				'METROPOLITAN', 
				paste(
					toupper(Name.en), 
					toupper(Name.fr), 
					toupper(Name.de)
				), 
				fixed = T
			)
		]
		
		euMSA[, NUTS := 'Metropolitan']
		
		
		euData <- data.table::rbindlist(list(euN2, euN3, euMSA))
		
		
		beaSrch <- beaR::beaSearch('',apiKey)
		beaData <- beaSrch[
			Account == 'Regional' & tolower(Parameter) != 'geofips', 
			.(
				id = Key, 
				Ref = paste0(DatasetName, ':', Parameter), 
#				MetaDataUpdated,
				Name.en = Desc 
			)
		]

		beaRegInc <- beaData[grep('RegionalIncome', Ref, fixed=T)]



		beaN2p <- beaData[grep('[state', tolower(Name.en), fixed = T)]
		beaN2d <- beaData[grep('(state', tolower(Name.en), fixed = T)]


		beaN2 <- data.table::rbindlist(list(beaN2p, beaN2d, beaRegInc))
		beaN2[, NUTS := 'State']
		

		beaN3p <- beaData[grep('[county', tolower(Name.en), fixed = T)]
		beaN3d <- beaData[grep('(county', tolower(Name.en), fixed = T)]

		
		beaN3 <- data.table::rbindlist(list(beaN3p, beaN3d, beaRegInc))
		beaN3[, NUTS := 'County']
		

		beaMSAp <- beaData[grep('[msa', tolower(Name.en), fixed = T)]
		beaMSAd <- beaData[grep('(msa', tolower(Name.en), fixed = T)]
		
		beaMSA <- data.table::rbindlist(list(beaMSAp, beaMSAd, beaRegInc))
		beaMSA[, NUTS := 'MSA']
		
		joinUSD <- data.table::rbindlist(list(beaN3, beaN2, beaMSA))
		usData <- joinUSD[!grep('SIC', Name.en, fixed = TRUE)]
		usData <- usData[!grep('(SIC)', Name.en, fixed = TRUE)]
		
		beaNA <- beaData[!(Ref %in% unique(usData[, Ref]))]

		usData[, Source := 'US']
		data.table::setkey(usData, key = Name.en)
		
		transKey <- data.table::fread(paste0(.libPaths()[1], '/euroState/rawdata/transKey.txt'))
		data.table::setkey(transKey, key = Name.en)
		
		usTrans <- usData[transKey][!is.na(NUTS)]
		data.table::setkey(usTrans, key = NULL)

#Worth worrying about, just not right now
		#indKey <- data.table::fread(paste0(.libPaths()[1], '/euroState/rawdata/IndustryID_conc.csv'))
		#indKey[, NAICS := strsplit(NAICS_Codes, ',', fixed = T)]
		

		
		euData[, Source := 'eurostat']
		
		worldData <- data.table::rbindlist(list(euData, unique(usTrans)), use.names = TRUE, fill = TRUE)

		searchOut <- worldData[grepl(tolower(searchTerm), tolower(paste(Ref, id, Name.en, Name.fr, Name.de)))]
		
		return(searchOut)
		
}

