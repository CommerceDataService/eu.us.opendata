source("Dataset_To_RDF.R", chdir=T)

#
#SET SPARQL ENDPOINT
#

SPARQL.endpoint<-"https://dydra.com/luca-gramaglia/eu-us-partnership-metadata-store/sparql"

#
#UPLOAD INPUT FILES
#

dataset.info.file<-"/Metadata/JOINT_To_RDF/Dataset_Template.csv"
component.info.file<-"/Metadata/JOINT_To_RDF/Component_Template.csv"

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
#INCLUDE COMPONENTS IN DATASET SPECS
#

DatasetSpecs.list <- apply(dataset.info, 1, function(x){
  
  l<-as.list(x[!is.na(x)])
  l$component<-list()
  
  return(l)
  })

ComponentSpecs.list <- apply(component.info, 1, function(x){
  
  l<-as.list(x[!is.na(x)])
  l$Dataset.ID <- which(dataset.info$Dataset.ID=="JOINT#GDP_A_2")
  return(l)
  
  })

for(i in 1:length(ComponentSpecs.list)){
  
  index<-ComponentSpecs.list[[i]]$Dataset.ID
  slot<-length(DatasetSpecs.list[[index]]$component)
  
  ComponentSpecs.list[[i]]$Dataset.ID<-NULL
  DatasetSpecs.list[[index]]$component[[slot+1]]<-ComponentSpecs.list[[i]]
  
}

Dataset_Insert(DatasetSpecs.list, SPARQL.endpoint)

