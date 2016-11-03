#' Search Metadata Store Relationship Table using text search
#' 
#' @param asHTML  Option to render results in an interactive DT
#' @param term  Search term
#' @import DT RCurl
#' @export 

searchRel <- function(term, asHTML = FALSE){
	requireNamespace('DT', quietly = TRUE)
	requireNamespace('RCurl', quietly = TRUE)
	
	start = proc.time()[3]
	#	eu.us.opendata::updateCache();

    flag <- c()
    
    #Check if there is any term that is passed into the function
    if(nchar(trimws(term))!= 0){
        
        
        #Step one -- canonical, RelTable table will go here -- will loadLocalRel.r run at some point before?
          localRel <- loadLocalRel()
          data <- as.data.frame(localRel[,, with = FALSE])
          data <- data[!duplicated(data[,2]),]
        
        #Step two: synonym engine
          split <- searchRefine(trimws(term))
          
        #Step three: Searching
          for(i in 0:5){
            for(k in split){
              for(j in 2:13){
                flag <- c(flag,
                          as.vector(
                            data[agrep(k, data[,j], max.distance = i, ignore.case = TRUE),2]
                            )
                          )
                  }
              
            }
          }
    }

    #Check if there are any results
		
    if(length(flag) == 0){
        warning("Search: No matches")
        return(data.frame())
      
      } else if(length(flag)>0 & asHTML==FALSE){
      #recommended relative rankings
        results <- data.frame((table(flag)))
        results$series <- (as.character(results$flag))
        results$Rel_score <- round(100*results$Freq/max(results$Freq),2)
        
        results <- merge(data,results,by.x="Rel_name",by.y="series")
        results <- results[,c("Rel_score","Rel_ID","Rel_name")]
        results <- results[!duplicated(results),]
        results <- results[order(-results$Rel_score),]
        print(paste("Search duration: ",round(proc.time()[3]-start,4),"s",sep=""))
        return(results)
        
      } else if(length(flag)>0 && asHTML==TRUE){
        
        #recommended relative rankings
        results <- data.frame((table(flag)))
        results$series <- (as.character(results$flag))
        results$Rel_score <- round(100*results$Freq/max(results$Freq),2)
        
        results <- merge(data,results,by.x="Rel_name",by.y="series")
        results <- results[,c("Rel_score","Rel_ID","Rel_name")]
        results <- results[!duplicated(results),]
        results <- results[order(-results$Rel_score),]
		    datatable(data.frame(results))
		} 
		
	
}
