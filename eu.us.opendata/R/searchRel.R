#' Search Metadata Store Relationship Table using text search
#' 
#' @param asHtml  Option to render results in an interactive DT
#' @param term  Search term
#' @param beaKey 	BEA API key (won't be necessary once SPARQL repository has been updated with timestamp)
#' @import DT RCurl
#' @export 

searchRel <- function(term, asHtml = FALSE, beaKey = ''){
	requireNamespace('DT', quietly = TRUE)
	requireNamespace('RCurl', quietly = TRUE)
	
		eu.us.opendata::updateCache(beaKey);

    flag <- c()
    
    #Check if there is any term that is passed into the function
    if(nchar(trimws(term))!= 0){
        split <- unlist(strsplit(gsub("[[:punct:]]","",term),"[[:space:]]"))
        
        #Step one -- canonical, RelTable table will go here -- will loadLocalRel.r run at some point before?
          localRel <- loadLocalRel()
          data <- as.data.frame(localRel[, 1:2, with = FALSE])
          
        #Step two: Scrapped pure regex in this round
          for(i in 0:5){
            for(k in split){
              flag <- c(flag,
                        data[agrep(k, data[,2], max.distance = i, ignore.case = TRUE),2])
            }
          }
    }
      
    #Check if there are any results
    if(length(flag) == 0){
        print("Search: No matches")
        return(data.frame())
      } else {
      
      #recommended relative rankings
        results <- data.frame((table(flag)))
        results$series <- (as.character(results$flag))
        results$Rel_score <- 100*results$Freq/max(results$Freq)
        
        results <- merge(data,results,by.x="Rel_name",by.y="series")
        results <- results[,c("Rel_score","Rel_ID","Rel_name")]
        results <- results[order(-results$Rel_score),]
        
      return(results)
      }
}
