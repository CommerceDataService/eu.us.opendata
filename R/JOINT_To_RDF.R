library(SPARQL)
source("Dataset_To_RDF.R", chdir=T)

#
#SET SPARQL ENDPOINT
#

SPARQL.endpoint<-"https://dydra.com/luca-gramaglia/eu-us-partnership-metadata-store/sparql"

#
#UPLOAD INPUT FILES
#

dataset.info.file<-"Metadata/JOINT_To_RDF/Dataset_Template.csv"
component.info.file<-"Metadata/JOINT_To_RDF/Component_Template.csv"

dataset.info<-read.csv(dataset.info.file)
component.info<-read.csv(component.info.file)

#
#CHECK INTEGRITY OF THE FILES
#

#Check that there are no duplicate Dataset IDs/Distribution IDs
if(nrow(dataset.info[!duplicated(dataset.info[, "Dataset.ID"]), ]) != nrow(dataset.info)) stop("Duplicate Dataset IDs!")

#Check that the Dataset IDs in the dataset file match the Dataset IDs in the distribution file and viceversa
if(nrow(merge(dataset.info, component.info, "Dataset.ID", all=TRUE)) != nrow(merge(dataset.info, component.info, "Dataset.ID"))) stop("Mismatches between datasets and component info available.")

#
#RETRIEVE CATALOG INFO
#

catalog.query<-"SELECT ?x WHERE {?x a dcat:Catalog}"
catalog.query<-add.namespace(catalog.query)

catalog<-as.character(SPARQL(SPARQL.endpoint, catalog.query)$results)

if(length(catalog)!=1) stop("No catalog found in metadata store")

#
#INCLUDE COMPONENTS IN DATASET SPECS
#

DatasetSpecs.list <- apply(dataset.info, 1, function(x){
  
  l<-as.list(x[!is.na(x)])
  l$component<-list()
  l$catalog<-catalog
  
  return(l)
  })

ComponentSpecs.list <- apply(component.info, 1, function(x){
  
  l<-as.list(x[!is.na(x)])
  l$Dataset.ID <- which(dataset.info$Dataset.ID==x["Dataset.ID"])
  return(l)
  
  })

for(i in 1:length(ComponentSpecs.list)){
  
  index<-ComponentSpecs.list[[i]]$Dataset.ID
  slot<-length(DatasetSpecs.list[[index]]$component)
  
  ComponentSpecs.list[[i]]$Dataset.ID<-NULL
  DatasetSpecs.list[[index]]$component[[slot+1]]<-ComponentSpecs.list[[i]]
  
}

Dataset_Insert(DatasetSpecs.list, SPARQL.endpoint)

