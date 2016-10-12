

readMappings <- function(SPARQL.endpoint){
	structure.map.file <- paste0(.libPaths()[1], "/euroStates/rawdata/Structure_Map_Template.csv")
	component.map.file <- paste0(.libPaths()[1], "/euroStates/rawdata/Component_Map_Template.csv")
	value.map.file <- paste0(.libPaths()[1], "/euroStates/rawdata/Value_Map_Template.csv")
	
	structure.map <- read.csv(structure.map.file)
	component.map <- read.csv(component.map.file)
	value.map <- read.csv(value.map.file)
	
	#
	#Check integrity of the files
	#
	
	#Check that there are no duplicate Dataset IDs and that all mandatory elements are present
	if(nrow(structure.map[!duplicated(structure.map[, "Structure.Map.ID"]), ]) != nrow(structure.map)) stop("Duplicate Structure Map IDs!")
	if(nrow(structure.map[complete.cases(structure.map),]) != nrow(structure.map)) stop("Missing mandatory values in structure map descriptions!")
	
	#Check that there are no duplicate Component Map IDs, 
	#that all mandatory elements are present
	if(nrow(component.map[!duplicated(component.map[, "Component.Map.ID"]), ]) != nrow(component.map)) stop("Duplicate Structure Map ID - Component Map ID combinations!")
	if(nrow(component.map[complete.cases(component.map),]) != nrow(component.map)) stop("Missing mandatory values in Component Map descriptions!")
	
	#Check that there are no duplicate Value Maps ID, 
	#that all mandatory elements are present
	if(nrow(value.map[!duplicated(value.map[, "Value.Map.ID"]), ]) != nrow(value.map)) stop("Duplicate Value Maps!")
	if(nrow(value.map[complete.cases(value.map),]) != nrow(value.map)) stop("Missing mandatory values in Value Map descriptions!")
	
	#Check that the Structure Map IDs in the structure map file match the Structure Map IDs in the Component map file and viceversa
	if(nrow(merge(structure.map, component.map, "Structure.Map.ID", all=TRUE)) != nrow(merge(structure.map, component.map, "Structure.Map.ID"))) stop("Mismatches between structure maps and component maps available.")
	
	#Check that the Component Map IDs in the value map file exist in the component map file
	if(nrow(merge(value.map, component.map, "Component.Map.ID")) != nrow(value.map)) stop("Mismatches between value maps and component maps available.")
	
	#
	#Prepare Structure Map RDF statements
	#
	
	structure.map.query.list <- NULL
	
	for (i in 1:nrow(structure.map)) {
	  
	  structure.map.specs  <-  list(
	    'id' = structure.map[i, "Structure.Map.ID"] ,
	    'sourceStructure' = structure.map[i, "Source.DSD"],
	    'targetStructure' = structure.map[i, "Target.DSD"],
	    'sourceStructureUsage' = structure.map[i, "Source.Dataset"],
	    'targetStructureUsage' = structure.map[i, "Target.Dataset"]
	  )
	  
	  structure.map.query.list <- c(structure.map.query.list, Create_RDF_Structure_Map(structure.map.specs))
	  
	}
	
	#
	#Prepare Component Map RDF statements
	#
	
	component.map.query.list <- NULL
	
	for (i in 1:nrow(component.map)) {
	  
	  component.map.specs  <-  list(
	    'id' = component.map[i, "Component.Map.ID"] ,
	    'map'= component.map[i, "Structure.Map.ID"] ,
	    'source' = component.map[i, "Source.Concept"],
	    'target' = component.map[i, "Target.Concept"]
	  )
	  
	  component.map.query.list <- c(component.map.query.list, Create_RDF_Component_Map(component.map.specs))
	  
	}
	
	#
	#Prepare Value Map RDF statements
	#
	
	value.map.query.list <- NULL
	
	for (i in 1:nrow(value.map)) {
	  
	  value.map.specs  <-  list(
	    'id' = value.map[i, "Value.Map.ID"] ,
	    'map'= value.map[i, "Component.Map.ID"] ,
	    'sourceValue' = value.map[i, "Source.Value"],
	    'targetValue' = value.map[i, "Target.Value"]
	  )
	  
	  value.map.query.list <- c(value.map.query.list, Create_RDF_Value_Map(value.map.specs))
	  
	}
	
	#
	#Execute queries
	#
	
	insert.output <- large.query.handler(SPARQL.endpoint, structure.map.query.list, query.builder)
	insert.output <- large.query.handler(SPARQL.endpoint, component.map.query.list, query.builder)
	insert.output <- large.query.handler(SPARQL.endpoint, value.map.query.list, query.builder)
	
}
