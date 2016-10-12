#' Create RDF structure map
#'
#' @param dataset.specs			A list of specifications
#' @return A list to be passed to the query 
#' @import SPARQL 

Create_RDF_Structure_Map <- function(structure.map.specs){
  
	requireNamespace('SPARQL', quietly = TRUE)

  query.list <- NULL
  
  id <-  paste("<Structure_Map_", structure.map.specs[["id"]], ">", sep="")
  query.list <- c(query.list, paste(id, "rdf:type struc:StructureMap ."))
  
  sourceStructure <- paste("<", structure.map.specs[["sourceStructure"]], ">", sep="")
  query.list <- c(query.list, paste(id, "struc:sourceStructure", sourceStructure, "."))
  
  targetStructure <- paste("<", structure.map.specs[["targetStructure"]], ">", sep="")
  query.list <- c(query.list, paste(id, "struc:targetStructure", targetStructure, "."))
  
  sourceStructureUsage <- paste("<", structure.map.specs[["sourceStructureUsage"]], ">", sep="")
  query.list <- c(query.list, paste(id, "struc:sourceStructureUsage", sourceStructureUsage, "."))
  
  targetStructureUsage <- paste("<", structure.map.specs[["targetStructureUsage"]], ">", sep="")
  query.list <- c(query.list, paste(id, "struc:targetStructureUsage", targetStructureUsage, "."))
  
  return(query.list)
}
