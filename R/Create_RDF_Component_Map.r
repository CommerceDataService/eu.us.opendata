#' Create RDF component map
#'
#' @param dataset.specs			A list of specifications
#' @return A list to be passed to the query 
#' @import SPARQL data.tree


Create_RDF_Component_Map <- function(component.map.specs){
  
  query.list <- NULL
  
	requireNamespace('SPARQL', quietly = TRUE)

  id <-  paste("<Component_Map_", component.map.specs[["id"]], ">", sep="")
  query.list <- c(query.list, paste(id, "rdf:type struc:ComponentMap ."))
  
  map <-  paste("<Structure_Map_", component.map.specs[["map"]], ">", sep="")
  query.list <- c(query.list, paste(map, "struc:hasComponentMap", id, "."))
  
  source <- paste("<", component.map.specs[["source"]], ">", sep="")
  query.list <- c(query.list, paste(id, "struc:source", source, "."))
  
  target <- paste("<", component.map.specs[["target"]], ">", sep="")
  query.list <- c(query.list, paste(id, "struc:target", target, "."))
  
  return(query.list)
}

