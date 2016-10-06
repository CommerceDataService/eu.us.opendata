Create_RDF_Value_Map <- function(value.map.specs){
  
	requireNamespace('SPARQL', quietly = TRUE)

  query.list <- NULL
  
  id <- paste("<Value_Map_", value.map.specs[["id"]], ">", sep="")
  query.list <- c(query.list, paste(id, "rdf:type struc:ValueMap ."))
  
  map <- paste("<Component_Map_", value.map.specs[["map"]], ">", sep="")
  query.list <- c(query.list, paste(map, "struc:hasRepresentationMapping", id, "."))
  
  source.value <- paste("\"", value.map.specs[["sourceValue"]], "\"", sep="")
  query.list <- c(query.list, paste(id, "struc:sourceValue", source.value, "."))
  
  target.value <- paste("\"", value.map.specs[["targetValue"]], "\"", sep="")
  query.list <- c(query.list, paste(id, "struc:targetValue", target.value, "."))
  
  return(query.list)
}

