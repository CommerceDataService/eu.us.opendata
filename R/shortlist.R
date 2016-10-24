
#' @example
#' getList <- shortlist();
#' outfile <- paste0(getwd(), '/ind_shortlist.csv')
#' write.table(getList, outfile, row.names = F);
#' outfile

shortlist <- function(){
	requireNamespace('data.table');
	localPath <- paste0(.libPaths()[1], '/euroStates/rawdata/bea_indmap.csv')

	tryCatch(
	
	{
		toGet <- data.table::fread(localPath);
		return(toGet);
	}, 

	error = function(e){
		tf <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".zip")
		
		download.file('http://www.bea.gov/regional/zip/gsp/gsp_naics_all_C.zip', tf, quiet = TRUE)
		
		tuz <- gsub('.zip', '', tf, fixed = T)
		
		unzip(tf, exdir=tuz)
		
		indies <- data.table::data.table(read.delim(
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
	
		write.csv2(toGet, localPath, quote = FALSE, row.names = FALSE)
	
		return(toGet)
		},

	finally = {''})
}
