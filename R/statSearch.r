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
		load(paste0(localMeta, '/euLocal.RData'))
	}, error = function(e){
		dir.create(localMeta, showWarnings = FALSE, recursive = TRUE)
			esDataFlow <- rsdmx::readSDMX(
				providerId = "ESTAT", 
				agencyId="ESTAT", 
				resource = "dataflow", 
				resourceId = statID
			)		
		save(esDataFlow, file = paste0(localMeta, '/euLocal.RData'))
		}, finally = {
			esData <- data.table::as.data.table(esDataFlow)[,.(
				id, 
				Ref = dsdRef,
				Name.fr,
				Name.en,
				Name.de
			)]		
		})
		
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

		euN2 <- unique(rbindlist(list(nut2Dash, nut2Spce, nut2Comp)))
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

		euN3 <- unique(rbindlist(list(nut3Dash, nut3Spce, nut3Comp)))
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
		
		
		euData <- rbindlist(list(euN2, euN3, euMSA))
		
		beaData <- beaR::beaSearch('',apiKey)[
			Account == 'Regional' & tolower(Parameter) != 'geofips', 
			.(
				id = Key, 
				Ref = paste0(DatasetName, ':', Parameter), 
#				MetaDataUpdated,
				Name.en = Desc 
			)
		]

		beaN2p <- usData[grep('[state', tolower(Name.en), fixed = T)]
		beaN2d <- usData[grep('(state', tolower(Name.en), fixed = T)]
		beaN2i <- usData[grep('RegionalIncome', Ref)]

		beaN2 <- rbindlist(list(beaN2p, beaN2d, beaN2i))
		beaN2[, NUTS := 'State']
		

		beaN3p <- usData[grep('[county', tolower(Name.en), fixed = T)]
		beaN3d <- usData[grep('(county', tolower(Name.en), fixed = T)]
		
		beaN3 <- rbindlist(list(beaN3p, beaN3d))
		beaN3[, NUTS := 'County']
		

		beaMSAp <- usData[grep('[msa', tolower(Name.en), fixed = T)]
		beaMSAd <- usData[grep('(msa', tolower(Name.en), fixed = T)]
		
		beaMSA <- rbindlist(list(beaMSAp, beaMSAd))
		beaMSA[, NUTS := 'MSA']
		
		
		usData <- rbindlist(list(beaN3, beaN2, beaMSA))
		usData <- usData[!grep('(SIC)', beaData, fixed = TRUE)]
		
		beaNA <- beaData[!(Ref %in% unique(usData[, Ref]))]

		usData[, Source := 'US']
		euData[, Source := 'eurostat']
		
		worldData <- rbindlist(list(euData, usData), use.names = TRUE, fill = TRUE)

		searchOut <- worldData[grepl(tolower(searchTerm), tolower(paste(Ref, id, Name.en, Name.fr, Name.de)))]
		
}