#' Inserts an SDMX dataflow into a triple store as an RDF Data Cube Vocabulary dataset. The dataset is complemented with metadata which follows the DCAT ontology.  
#' 
#' @param SPARQL.endpoint		the SPARQL endpoint URL of the target triple store
#' @param dataflow		SDMX dataflow object
#' @returns NULL
#' @import 	SPARQL rsdmx

Dataflow_To_RDF <- function(SPARQL.endpoint, dataflow){
  
	requireNamespace('SPARQL', quietly = TRUE)
	requireNamespace('rsdmx', quietly = TRUE)

  query.list <- NULL
  
  id <- paste("<", dataflow@id, ">", sep="")
  query.list <- c(query.list, paste(id, "rdf:type qb:DataSet, dcat:Dataset ."))
  
  title <- paste("\"", dataflow@id, "\"", sep="")
  query.list <- c(query.list, paste(id, "dct:title", title, "."))
  
  description <- paste("\"", dataflow@Name[["en"]], "\"", sep="")
  query.list <- c(query.list, paste(id, "dct:description", description, "."))
  
  publisher <- paste("<", dataflow@agencyID, ">", sep="")
  query.list <- c(query.list, paste(id, "dct:publisher", publisher, "."))
  
  structure <- paste("<", dataflow@dsdRef, ">", sep="")
  query.list <- c(query.list, paste(id, "qb:structure", structure, "."))
  
  insert.output <- large.query.handler(SPARQL.endpoint, query.list, query.builder)
  
}

