#' Load local structure metadata cache
#' 
#' @keywords internal
#' @return Local cache of structure metadata, as data.table
#' @import data.table
#' @export 

loadLocalStruc <- function(){
#Think we should set validate = TRUE for all requests

	requireNamespace('data.table', quietly = TRUE)

	localPath <- paste0(.libPaths()[1], '/eu.us.openR/rawdata')

	tryCatch({
		localStruc <- data.table::fread(paste0(localPath, '/Structure_Table.csv'));
		return(localStruc);
	}, error = function(e){
		updateLocalStruc();
	}, finally = {
		localStruc <- data.table::fread(paste0(localPath, '/Structure_Table.csv'));
		return(localStruc);
	})
		
}

