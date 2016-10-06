library(SPARQL)

Blank_ID<-function(blank.counter){
  
  return(paste("_:b", blank.counter, sep=""))
  
}

query.builder<-function(query){
  
  namespace.qb<-"PREFIX qb: <http://purl.org/linked-data/cube#>"
  namespace.dcat<-"PREFIX dcat: <http://www.w3.org/ns/dcat#>"
  namespace.dct<-"PREFIX dct: <http://purl.org/dc/terms/>"
  namespace.struc<-"PREFIX struc: <http://www.example.org/struc/>"
  
  query<-paste("INSERT DATA {", query,  "}")
  query<-paste(namespace.qb, namespace.dcat, namespace.dct, namespace.struc, query)
  
  return(query)  
  
}

large.query.handler<-function(endpoint, query.list, query.builder, limit=5000){
  
  results<-NULL
  i<-1
  
  while (i<length(query.list)){
    
    begin<-i
    size<-0
    while (size + nchar(query.list[i]) < limit && i<length(query.list)) {
      i<-i+1
      size<-size+nchar(query.list[i])
    }
    
    query<-paste(query.list[begin:i], collapse="")
    query<-query.builder(query)
    results<-rbind(results, SPARQL(endpoint,query)$results)
    
  }
  
  return(results)
}

Create_RDF_Structure_Map<-function(structure.map.specs){
  
  query.list<-NULL
  
  id<- paste("<Structure_Map_", structure.map.specs[["id"]], ">", sep="")
  query.list<-c(query.list, paste(id, "rdf:type struc:StructureMap ."))
  
  sourceStructure<-paste("<", structure.map.specs[["sourceStructure"]], ">", sep="")
  query.list<-c(query.list, paste(id, "struc:sourceStructure", sourceStructure, "."))
  
  targetStructure<-paste("<", structure.map.specs[["targetStructure"]], ">", sep="")
  query.list<-c(query.list, paste(id, "struc:targetStructure", targetStructure, "."))
  
  sourceStructureUsage<-paste("<", structure.map.specs[["sourceStructureUsage"]], ">", sep="")
  query.list<-c(query.list, paste(id, "struc:sourceStructureUsage", sourceStructureUsage, "."))
  
  targetStructureUsage<-paste("<", structure.map.specs[["targetStructureUsage"]], ">", sep="")
  query.list<-c(query.list, paste(id, "struc:targetStructureUsage", targetStructureUsage, "."))
  
  return(query.list)
}

Create_RDF_Component_Map<-function(component.map.specs){
  
  query.list<-NULL
  
  id<- paste("<Component_Map_", component.map.specs[["id"]], ">", sep="")
  query.list<-c(query.list, paste(id, "rdf:type struc:ComponentMap ."))
  
  map<- paste("<Structure_Map_", component.map.specs[["map"]], ">", sep="")
  query.list<-c(query.list, paste(map, "struc:hasComponentMap", id, "."))
  
  source<-paste("<", component.map.specs[["source"]], ">", sep="")
  query.list<-c(query.list, paste(id, "struc:source", source, "."))
  
  target<-paste("<", component.map.specs[["target"]], ">", sep="")
  query.list<-c(query.list, paste(id, "struc:target", target, "."))
  
  return(query.list)
}

Create_RDF_Value_Map<-function(value.map.specs){
  
  query.list<-NULL
  
  id<- paste("<Value_Map_", value.map.specs[["id"]], ">", sep="")
  query.list<-c(query.list, paste(id, "rdf:type struc:ValueMap ."))
  
  map<- paste("<Component_Map_", value.map.specs[["map"]], ">", sep="")
  query.list<-c(query.list, paste(map, "struc:hasRepresentationMapping", id, "."))
  
  source.value<-paste("\"", value.map.specs[["sourceValue"]], "\"", sep="")
  query.list<-c(query.list, paste(id, "struc:sourceValue", source.value, "."))
  
  target.value<-paste("\"", value.map.specs[["targetValue"]], "\"", sep="")
  query.list<-c(query.list, paste(id, "struc:targetValue", target.value, "."))
  
  return(query.list)
}

SPARQL.endpoint<-"https://dydra.com/luca-gramaglia/eu-us-partnership-metadata-store/sparql"

structure.map.file<-"/home/luca/Desktop/R-SPARQL/EU-US Partnership/Metadata/Mappings/Structure_Map_Template.csv"
component.map.file<-"/home/luca/Desktop/R-SPARQL/EU-US Partnership/Metadata/Mappings/Component_Map_Template.csv"
value.map.file<-"/home/luca/Desktop/R-SPARQL/EU-US Partnership/Metadata/Mappings/Value_Map_Template.csv"

structure.map<-read.csv(structure.map.file)
component.map<-read.csv(component.map.file)
value.map<-read.csv(value.map.file)

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

structure.map.query.list<-NULL

for (i in 1:nrow(structure.map)) {
  
  structure.map.specs <- list(
    'id' = structure.map[i, "Structure.Map.ID"] ,
    'sourceStructure' = structure.map[i, "Source.DSD"],
    'targetStructure' = structure.map[i, "Target.DSD"],
    'sourceStructureUsage' = structure.map[i, "Source.Dataset"],
    'targetStructureUsage' = structure.map[i, "Target.Dataset"]
  )
  
  structure.map.query.list<-c(structure.map.query.list, Create_RDF_Structure_Map(structure.map.specs))
  
}

#
#Prepare Component Map RDF statements
#

component.map.query.list<-NULL

for (i in 1:nrow(component.map)) {
  
  component.map.specs <- list(
    'id' = component.map[i, "Component.Map.ID"] ,
    'map'= component.map[i, "Structure.Map.ID"] ,
    'source' = component.map[i, "Source.Concept"],
    'target' = component.map[i, "Target.Concept"]
  )
  
  component.map.query.list<-c(component.map.query.list, Create_RDF_Component_Map(component.map.specs))
  
}

#
#Prepare Value Map RDF statements
#

value.map.query.list<-NULL

for (i in 1:nrow(value.map)) {
  
  value.map.specs <- list(
    'id' = value.map[i, "Value.Map.ID"] ,
    'map'= value.map[i, "Component.Map.ID"] ,
    'sourceValue' = value.map[i, "Source.Value"],
    'targetValue' = value.map[i, "Target.Value"]
  )
  
  value.map.query.list<-c(value.map.query.list, Create_RDF_Value_Map(value.map.specs))
  
}

#
#Execute queries
#

insert.output<-large.query.handler(SPARQL.endpoint, structure.map.query.list, query.builder)
insert.output<-large.query.handler(SPARQL.endpoint, component.map.query.list, query.builder)
insert.output<-large.query.handler(SPARQL.endpoint, value.map.query.list, query.builder)

