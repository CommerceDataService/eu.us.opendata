#' Load local acronym metadata cache
#' 
#' @keywords internal
#' @return Local cache of acronyms 
#' @import data.table
#' @export 

loadLocalAcronyms <- function(){
  
  localPath <- paste0(.libPaths()[1], '/eu.us.opendata/rawdata')
  
  tryCatch({
    localAcronyms <- read.csv(paste0(localPath, '/Acronym_Table.csv'));
    return(localAcronyms);
  })
  
}

