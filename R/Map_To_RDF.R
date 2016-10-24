source("RDF_query_helper_functions.R", chdir=T)

Map_To_RDF<-function(MapSpecs){
  
  query.list<-NULL
  
  if(!is.null(MapSpecs$sourceStructure) && !is.null(MapSpecs$targetStructure)){
    id.stem<- paste(MapSpecs$sourceStructure, MapSpecs$targetStructure, sep="#")
    id.stem<-paste("MAP#", id.stem, sep="")
    id<-paste("<", id.stem, ">", sep="")
    query.list<-c(query.list, paste(id, "rdf:type struc:StructureMap ."))
    sourceStructure<-paste("<", MapSpecs$sourceStructure, ">", sep="")
    targetStructure<-paste("<", MapSpecs$targetStructure, ">", sep="")
    query.list<-c(query.list, paste(id, "struc:sourceStructure",  sourceStructure, "."))
    query.list<-c(query.list, paste(id, "struc:targetStructure",  targetStructure, "."))
  }
  
  if(!is.null(MapSpecs$hasComponentMap)){
    
    for(i in 1:length(MapSpecs$hasComponentMap)){
   
      component.id.stem<-paste(id.stem, "#COMP", i, sep="")
      component.id<-paste("<", component.id.stem, ">", sep="")
      query.list<-c(query.list, paste(component.id, "rdf:type struc:ComponentMap ."))
      query.list<-c(query.list, paste(id, "struc:hasComponentMap", component.id, "."))
      
      if(!is.null(MapSpecs$hasComponentMap[[i]]$source)){
        source<-MapSpecs$hasComponentMap[[i]]$source
        query.list<-c(query.list, paste(component.id, "struc:source", source, "."))
      }
      
      if(!is.null(MapSpecs$hasComponentMap[[i]]$target)){
        target<-MapSpecs$hasComponentMap[[i]]$target
        query.list<-c(query.list, paste(component.id, "struc:target", target, "."))
      }
      
      if(!is.null(MapSpecs$hasComponentMap[[i]]$hasRepresentationMapping)){
        
        for(j in 1:length(MapSpecs$hasComponentMap[[i]]$hasRepresentationMapping)){
          
          representation.id<-paste("<", component.id.stem, "#REP", j, ">", sep="")
          type<-paste("struc:", MapSpecs$hasComponentMap[[i]]$hasRepresentationMapping[[j]]$mapType, sep="")
          query.list<-c(query.list, paste(representation.id, "rdf:type", type, "."))
          query.list<-c(query.list, paste(component.id, "struc:hasRepresentationMapping", representation.id, "."))
          
          if(!is.null(MapSpecs$hasComponentMap[[i]]$hasRepresentationMapping[[j]]$source)){
            source<-MapSpecs$hasComponentMap[[i]]$hasRepresentationMapping[[j]]$source
            query.list<-c(query.list, paste(representation.id, "struc:source", source, "."))
          } 
          
          if(!is.null(MapSpecs$hasComponentMap[[i]]$hasRepresentationMapping[[j]]$target)){
            source<-MapSpecs$hasComponentMap[[i]]$hasRepresentationMapping[[j]]$target
            query.list<-c(query.list, paste(representation.id, "struc:target", target, "."))
          }   
        }
      }
    }
  }
  
  return(query.list)
}

Map_Insert<-function(MapSpecs.list, SPARQL.endpoint) {
  
  Map.query.list<-unlist(lapply(MapSpecs.list, Map_To_RDF))
  
  large.query.handler(SPARQL.endpoint, Map.query.list, insert.query.builder)
  
}