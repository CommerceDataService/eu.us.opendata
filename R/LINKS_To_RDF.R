library(SPARQL)
source("Map_To_RDF.R", chdir = T)
source("RDF_query_helper_functions.R", chdir=T)

#
#SET SPARQL ENDPOINT
#

SPARQL.endpoint<-"https://dydra.com/luca-gramaglia/eu-us-partnership-metadata-store/sparql"

#
#UPLOAD INPUT FILES
#

structure.info.file<-"Metadata/LINKS_To_RDF/Structure_Map_Template.csv"

structure.info<-read.csv(structure.info.file)

#
#CHECK INTEGRITY OF THE FILES
#

#Check that there are no duplicate Dataset IDs/Distribution IDs
if(nrow(structure.info[!duplicated(structure.info),]) != nrow(structure.info)) stop("Duplicate mappings!")

#Check that source and target structures are always different
if(sum(as.character(structure.info$sourceStructure)==as.character(structure.info$targetStructure)) != 0) stop("Same source and target structures!")

#Check that all data structures exist
dataset.list<-c(as.character(structure.info[,"sourceStructure"]), as.character(structure.info[,"targetStructure"]))
dataset.list<-dataset.list[!duplicated(dataset.list)]
dataset.list<-paste("<", dataset.list, ">", sep="")

DSD.select.query<-"SELECT ?x WHERE {?x a dcat:Dataset}"
DSD.select.query<-add.namespace(DSD.select.query)

DSD.select<-SPARQL(SPARQL.endpoint, DSD.select.query)$results

if(sum(dataset.list %in% DSD.select) != length(dataset.list)) stop("Some source/target data structures do not exist")

#Check that all components exist and retrieve IDs

source.list<-structure.info[,c("sourceStructure","sourceComponent")]
names(source.list)<-c("Structure", "Component")

target.list<-structure.info[,c("targetStructure","targetComponent")]
names(target.list)<-c("Structure", "Component")

combined.list<-rbind(source.list, target.list)
combined.list<-combined.list[!duplicated(combined.list),]

values<-cbind(paste("<", combined.list[,"Structure"], ">", sep=""), paste("\"", combined.list[,"Component"], "\"", sep=""))
values<-apply(values, 1, function(x) paste("(", paste(x, collapse=" "), ")", sep=""))
values<-paste(values, collapse=" ")

component.select.query<-paste("SELECT ?x WHERE { VALUES (?dsd ?label) {", values,  "} ?dsd qb:component ?y . ?x rdfs:label ?label . ?y ?p ?x }")
component.select.query<-add.namespace(component.select.query)

component.select<-as.character(SPARQL(SPARQL.endpoint, component.select.query)$results)

if(length(component.select)!=nrow(combined.list)) stop("Some source/target components do not exist")

#
#PREPARE STRUCTURE INFO
#

#Split by structure and component

MapSpecs.list<-split(structure.info, list(structure.info$sourceStructure, structure.info$targetStructure))
names(MapSpecs.list)<-NULL

MapSpecs.list<-lapply(MapSpecs.list, function(x) {
  
  l<-list()
  
  index.source<-which(names(x)=="sourceStructure")
  index.target<-which(names(x)=="targetStructure")
  index<-c(index.source, index.target)
  
  l$sourceStructure<-as.character(x[,index.source])[1]
  l$targetStructure<-as.character(x[,index.target])[1]
  
  drop<-x[,-index]
  l$hasComponentMap<-split(drop, list(drop$sourceComponent, drop$targetComponent), drop=TRUE)
  
  return(l)
})

#Split by map

MapSpecs.list<-lapply(MapSpecs.list, function(x){
  
  names(x$hasComponentMap)<-NULL
  
  x$hasComponentMap<-lapply(x$hasComponentMap, function(y){
    
    l<-list()
    
    index.source<-which(names(y)=="sourceComponent")
    index.target<-which(names(y)=="targetComponent")
    index<-c(index.source, index.target)
    
    l$source<-as.character(y[,index.source])[1]
    l$target<-as.character(y[,index.target])[1]
    
    drop<-y[,-index]
    l$hasRepresentationMapping<-split(drop, list(drop$mapType), drop=TRUE)
    
    return(l)
    
  })
  
  return(x)
})

#Tidy up

MapSpecs.list<-lapply(MapSpecs.list, function(x){
  
  x$hasComponentMap<-lapply(x$hasComponentMap, function(y){
    
    index.source<-which(as.character(combined.list$Structure) == x$sourceStructure & as.character(combined.list$Component) == y$source)
    index.target<-which(as.character(combined.list$Structure) == x$targetStructure & as.character(combined.list$Component) == y$target)
    
    y$source<-component.select[index.source]
    y$target<-component.select[index.target]
    
    names(y$hasRepresentationMapping)<-NULL
    
    y$hasRepresentationMapping<-lapply(y$hasRepresentationMapping, function(z){
      
      l<-list()
      
      index.source<-which(names(z)=="sourceValue")
      index.target<-which(names(z)=="targetValue")
      index.map<-which(names(z)=="mapType")
      
      l$source<-paste("\"", as.character(z[,index.source])[1], "\"", sep="")
      l$target<-paste("\"", as.character(z[,index.target])[1], "\"", sep="")
      l$mapType<-as.character(z[,index.map])[1]
      
      return(l)
    })
    
    return(y)
  })
  
  return(x)
})

#Map.query.list<-unlist(lapply(MapSpecs.list, Map_To_RDF))
Map_Insert(MapSpecs.list, SPARQL.endpoint)
