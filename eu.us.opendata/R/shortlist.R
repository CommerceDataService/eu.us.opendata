#' Pass list of user specifications (including API key) to return data from BEA API.
#' 
#' @keywords internal
#' @return By default, an object of class 'data.table' containing a mapping of BEA regional IndustryIDs to NAICS codes
#'	@export

shortlist <- function(){
	requireNamespace('data.table');
	`.` 										<- NULL
	ind1						 				<- NULL
	ind2						 				<- NULL
	ind3						 				<- NULL
	ind4						 				<- NULL
	NAICS								 		<- NULL
	include							 		<- NULL
	IndustryId						 	<- NULL
	IndustryID						 	<- NULL
	Description 						<- NULL
	IndustryClassification 	<- NULL

	
	localPath <- paste0(.libPaths()[1], '/eu.us.opendata/rawdata/bea_indmap.csv')

	tryCatch(
	
	{
		toGet <- data.table::fread(localPath);
		return(toGet);
	}, 

	error = function(e){
		tf <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".zip")
		
		utils::download.file('http://www.bea.gov/regional/zip/gsp/gsp_naics_all_C.zip', tf, quiet = TRUE)
		
		tuz <- gsub('.zip', '', tf, fixed = T)
		
		utils::unzip(tf, exdir=tuz)
		
		indies <- data.table::data.table(utils::read.delim(
		 	paste0(tuz, '/gsp_naics_all_C.csv'), 
		 	sep=',', 
		 	quote = '\"' ,
		 	fill = TRUE
		))
		
		indies[, 
			include := ifelse(
				nchar(
					strsplit(
						as.character(IndustryClassification), 
						'[[:punct:]]'
					)[1]
				) > 2,
				FALSE,
				TRUE
			) 
		]
		
		
		indt <- indies[, 
			.(Description,
			NAICS = as.character(IndustryClassification),
			IndustryID = IndustryId)
		]
		
		indt[NAICS != '...', c('ind1', 'ind2', 'ind3', 'ind4') := tstrsplit(NAICS, '[[:punct:]]')]
		
		toGet <- unique(
			indt[
				!is.na(NAICS) & 
				!is.na(IndustryID) & 
				(nchar(ind1) < 3 | is.na(ind1)), 
				.(Description, IndustryID, ind1, ind2, ind3, ind4)])
	
		utils::write.csv2(toGet, localPath, quote = FALSE, row.names = FALSE)
	
		return(toGet)
		},

	finally = {''})
}
