#' Load local merge metadata cache
#' 
#' @return Local cache of merge metadata, as data.table
#' @import data.table
#' @export 

loadLocalMerge <- function(){
#Think we should set validate = TRUE for all requests

	requireNamespace('data.table', quietly = TRUE)

	localPath <- paste0(.libPaths()[1], '/euroStates/rawdata')

	tryCatch({
		localMerge <- data.table::fread(paste0(localPath, '/Merge_Table.csv'))
		return(localMerge)
	}, error = function(e){
		message('[Insert handler for local data csv not being found]')
		localMerge <- NULL
		return(localMerge)
		}, finally = {''})

		
}

