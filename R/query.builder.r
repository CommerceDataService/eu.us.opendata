#' Creates a well-formed INSERT SPARQL query by adding the INSERT clause and the required namespace prefixes
#' 
#' @param query		the desired content of the INSERT clause
#' @return 	A well-formed SPARQL INSERT query

query.builder <- function(query){
  
  namespace.qb <- "PREFIX qb: <http://purl.org/linked-data/cube#>"
  namespace.dcat <- "PREFIX dcat: <http://www.w3.org/ns/dcat#>"
  namespace.dct <- "PREFIX dct: <http://purl.org/dc/terms/>"
  
  query <- paste("INSERT DATA {", query,  "}")
  query <- paste(namespace.qb, namespace.dcat, namespace.dct, query)
  
  return(query)  
  
}

