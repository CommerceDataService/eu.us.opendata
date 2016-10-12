#' Search Metadata Store Relationship Table using text search
#' 
#' @return 
#' @import DT
#' @export 

searchRel <- function(term){
  ##Need to add
  
  a = gsub("[[:punct:]]","",term)
  split <- unlist(strsplit(a,"[[:space:]]"))
  
  #stage one -- canonical, RelTable table will go here -- will loadLocalRel.r run at some point before?
    data <- c("annual gdp","quarterly gdp", "Annual GDP by State/NUTS2", "NUTS3","NUTS2","nothing","yadda")
  
  #independent search, iter max distance from 0:n, search by term, return vector of quasi-matched terms
  #Scrapped pure regex in this round
    flag <- c()
    for(i in 0:5){
    for(k in split){
      flag <- c(flag,data[agrep(k, data, max = i, ignore.case = TRUE)])
    }
    }
  
  #recommended relative rankings
    results <- data.frame((table(flag)))
    results$series <- (as.character(results$flag))
    results <- results[order(-results$Freq),]
    results$rel_score <- 100*results$Freq/max(results$Freq)
    
  #Print results to console
    print(paste("RESULTS FOR TERM = '", term,"'",sep=""))
    print(results[,c("series","rel_score")])
    
  #payload results
    return(results[,c("series","rel_score")])
}

res <- searchRel("ann")
