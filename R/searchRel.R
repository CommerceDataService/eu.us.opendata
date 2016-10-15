#' Search Metadata Store Relationship Table using text search
#' 
#' @return 
#' @import DT
#' @export 

searchRel <- function(term){
    
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
                        data[agrep(k, data[,2], max = i, ignore.case = TRUE),2])
            }
          }
    }
      
    #Check if there are any results
    if(length(flag)>0){
      #recommended relative rankings
        results <- data.frame((table(flag)))
        results$series <- (as.character(results$flag))
        results <- results[order(-results$Freq),]
          results$Rel_score <- 100*results$Freq/max(results$Freq)
        
      #Print results to console
        
          results <- merge(data,results,by.x="Rel_name",by.y="series")
          results <- results[,c("Rel_score","Rel_ID","Rel_name")]
          print(paste("RESULTS FOR TERM = '", term,"'",sep=""))
          print(results)
          
          #payload results <-- need to add Rel_ID
          return(results)
      } else{
        print("No matches")
      }
  
}

#res <- searchRel("county")
