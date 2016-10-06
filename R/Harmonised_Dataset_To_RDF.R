library(SPARQL)
library(data.tree)

Blank_ID<-function(blank.counter){
  
  return(paste("_:b", blank.counter, sep=""))
  
}

query.builder<-function(query){
  
  namespace.qb<-"PREFIX qb: <http://purl.org/linked-data/cube#>"
  namespace.dcat<-"PREFIX dcat: <http://www.w3.org/ns/dcat#>"
  namespace.dct<-"PREFIX dct: <http://purl.org/dc/terms/>"
  
  query<-paste("INSERT DATA {", query,  "}")
  query<-paste(namespace.qb, namespace.dcat, namespace.dct, query)
  
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

Create_RDF_Dataset<-function(dataset.specs){
  
  query.list<-NULL
  
  id<- paste("<", dataset.specs[["id"]], ">", sep="")
  query.list<-c(query.list, paste(id, "rdf:type qb:DataSet, dcat:Dataset ."))
  
  title<-paste("\"", dataset.specs[["title"]], "\"", sep="")
  query.list<-c(query.list, paste(id, "dct:title", title, "."))
  
  description<-paste("\"", dataset.specs[["description"]], "\"", sep="")
  query.list<-c(query.list, paste(id, "dct:description", description, "."))
  
  publisher<-paste("<", dataset.specs[["publisher"]], ">", sep="")
  query.list<-c(query.list, paste(id, "dct:publisher", publisher, "."))
  
  structure<-paste("<", dataset.specs[["structure"]], ">", sep="")
  query.list<-c(query.list, paste(id, "qb:structure", structure, "."))  
  
  return(query.list)
}

Create_RDF_DSD<-function(DSD.tree) {
  
  query.dsd <- NULL
  blank.counter<-0
  
  for (i in 1:length(DSD.tree$children)){
    
    dsd.id<-paste("<", DSD.tree$children[[i]]$name, ">", sep="")
    query.dsd<-c(query.dsd, paste(dsd.id, "rdf:type qb:DataStructureDefinition ."))
    
    dsd.label<-paste("\"", DSD.tree$children[[i]]$DSD.label, "\"", sep="")
    query.dsd<-c(query.dsd, paste(dsd.id, "rdfs:label", dsd.label, "."))
    
    for (j in 1:length(DSD.tree$children[[i]]$children)){
      
      component.id<-paste("<", DSD.tree$children[[i]]$children[[j]]$name, ">", sep="")
      component.property<-paste("qb:", DSD.tree$children[[i]]$children[[j]]$Component.role, "Property", sep="")
      query.dsd<-c(query.dsd, paste(component.id, "rdf:type", component.property, "."))
      
      blank.id<-Blank_ID(blank.counter)
      
      query.dsd<-c(query.dsd, paste(dsd.id, "qb:component", blank.id, "."))
      query.dsd<-c(query.dsd, paste(blank.id, "rdf:type qb:ComponentSpecification ."))
      component.role<-paste("qb:", tolower(DSD.tree$children[[i]]$children[[j]]$Component.role), sep="")
      query.dsd<-c(query.dsd, paste(blank.id, component.role , component.id, "."))      
      
      blank.counter<-blank.counter+1
      
      label<-paste("\"", DSD.tree$children[[i]]$children[[j]]$Component.label, "\"", sep="")
      query.dsd<-c(query.dsd, paste(component.id, "rdfs:label", label, "."))
      
      if(DSD.tree$children[[i]]$children[[j]]$Coded){
        
        query.dsd<-c(query.dsd, paste(component.id, "rdf:type qb:CodedProperty ."))
        
        code.list<-paste("<", DSD.tree$children[[i]]$children[[j]]$Codelist.ID, ">", sep="")
        query.dsd<-c(query.dsd, paste(component.id, "qb:codeList", code.list, "."))
      }      
      
    }
  }
  
  return(query.dsd)
  
}

SPARQL.endpoint<-"https://dydra.com/luca-gramaglia/eu-us-partnership-metadata-store/sparql"

dataset.file<-"/home/luca/Desktop/R-SPARQL/EU-US Partnership/Metadata/Dataset-DSD/Dataset_Template.csv"
DSD.file<-"/home/luca/Desktop/R-SPARQL/EU-US Partnership/Metadata/Dataset-DSD/DSD_Template.csv"

dataset<-read.csv(dataset.file)
DSD<-read.csv(DSD.file)

#
#Check integrity of the files
#

#Check that there are no duplicate Dataset IDs and that all mandatory elements are present
if(nrow(dataset[!duplicated(dataset[, "Dataset.ID"]), ]) != nrow(dataset)) stop("Duplicate dataset IDs!")
if(nrow(dataset[complete.cases(dataset),]) != nrow(dataset)) stop("Missing mandatory values in dataset descriptions!")

#Check that there are no duplicate DSD - Component combinations, 
#that all mandatory elements are present
#and that a given DSD ID implies a given DSD label
if(nrow(DSD[!duplicated(DSD[, c("DSD.ID", "Component.ID")]), ]) != nrow(DSD)) stop("Duplicate DSD ID - Component ID combinations!")
if(nrow(DSD[complete.cases(subset(DSD, select=-c(Codelist.ID))),]) != nrow(DSD)) stop("Missing mandatory values in DSD descriptions!")
if(!all(duplicated(DSD[, c("DSD.ID")]) == duplicated(DSD[, c("DSD.label")]))) stop("Different DSD labels for same DSD ID!")

#Check that the DSD IDs in the dataset file match the DSD IDs in the DSD file and viceversa
if(nrow(merge(dataset, DSD, "DSD.ID", all=TRUE)) != nrow(merge(dataset, DSD, "DSD.ID"))) stop("Mismatches between datasets and DSDs available.")

#
#Prepare dataset RDF statements
#

dataset.query.list<-NULL

for (i in 1:nrow(dataset)) {
  
  dataset.specs <- list(
    'id' = dataset[i, "Dataset.ID"] ,
    'title' = dataset[i, "Dataset.title"],
    'description' = dataset[i, "Dataset.description"],
    'publisher' = dataset[i, "Dataset.publisher"],
    'structure' = dataset[i, "DSD.ID"]
  )
  
  dataset.query.list<-c(dataset.query.list, Create_RDF_Dataset(dataset.specs))
  
}


#
#Prepare DSD RDF statements
#

DSD$pathString<-paste("All", DSD$DSD.ID, DSD$Component.ID, sep="/")
DSD.tree<-FromDataFrameTable(DSD, colLevels = list(NULL, "DSD.label", c("Component.label", "Component.role", "Coded", "Codelist.ID")))

dsd.query.list<-Create_RDF_DSD(DSD.tree)

#
#Execute queries
#

insert.output<-large.query.handler(SPARQL.endpoint, dataset.query.list, query.builder)
insert.output<-large.query.handler(SPARQL.endpoint, dsd.query.list, query.builder)









