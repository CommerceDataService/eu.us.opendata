#' This function takes a list of RDF statements (triples) and: 1) divides them into chunks; 2) turns them into a full-fledged SPARQL INSERT query using the query.builder function; and 3) inserts them into the target triple store by executing the SPARQL INSERT query
#' 
#' @param endpoint		the SPARQL endpoint URL of the target triple store
#' @param query.list		a list of RDF statements
#' @param query.builder		the function to be used to build the query
#' @param limit		maximum number of characters in each chunk
#' @return NULL
#' @import 	SPARQL

large.query.handler <- function(endpoint, query.list, query.builder, limit = 5000){
	requireNamespace('SPARQL', quietly = TRUE)
  
  results <- NULL
  i <- 1
  
  while (i < length(query.list)){
    
    begin <- i
    size <- 0
    while (size + nchar(query.list[i]) < limit && i<length(query.list)) {
      i <- i + 1
      size <- size + nchar(query.list[i])
    }
    
    query <- paste(query.list[begin:i], collapse="")
    query <- query.builder(query)
    results <- rbind(results, SPARQL(endpoint,query)$results)
    
  }
  
  return(results)
}

