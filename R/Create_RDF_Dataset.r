#' Create RDF dataset
#' 
#' @param dataset.specs			A list of specifications
#' @return A list to be passed to the query 
#' @import SPARQL data.tree
#' @export 

Create_RDF_Dataset <- function(dataset.specs){

  query.list <- NULL

	requireNamespace('SPARQL', quietly = TRUE)
	requireNamespace('data.tree', quietly = TRUE)
  
  
  id <-  paste("<", dataset.specs[["id"]], ">", sep="")
  query.list <- c(query.list, paste(id, "rdf:type qb:DataSet, dcat:Dataset ."))
  
  title <- paste("\"", dataset.specs[["title"]], "\"", sep="")
  query.list <- c(query.list, paste(id, "dct:title", title, "."))
  
  description <- paste("\"", dataset.specs[["description"]], "\"", sep="")
  query.list <- c(query.list, paste(id, "dct:description", description, "."))
  
  publisher <- paste("<", dataset.specs[["publisher"]], ">", sep="")
  query.list <- c(query.list, paste(id, "dct:publisher", publisher, "."))
  
  structure <- paste("<", dataset.specs[["structure"]], ">", sep="")
  query.list <- c(query.list, paste(id, "qb:structure", structure, "."))  
  
  return(query.list)
}

