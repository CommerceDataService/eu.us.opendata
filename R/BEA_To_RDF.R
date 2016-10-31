library(SPARQL)
library(RCurl)
library(rjson)
library(reshape2)
source("Dataset_To_RDF.R", chdir=T)
source("shortlist.R", chdir=T)

#
#SET SPARQL ENDPOINT
#

SPARQL.endpoint<-"https://dydra.com/luca-gramaglia/eu-us-partnership-metadata-store/sparql"

#
# SET BEA API KEY AND URL
#

bea.api.key <- "CD09B666-8E09-4B62-B5FA-D1E830084579"
bea.api.url <- "http://www.bea.gov/api/data/"

#
#UPLOAD INPUT FILES
#

dataset.info.file<-"Metadata/BEA_To_RDF/Dataset_Template.csv"
BEAAPI.info.file<-"Metadata/BEA_To_RDF/API_Template.csv"

dataset.info<-read.csv(dataset.info.file)
BEAAPI.info<-read.csv(BEAAPI.info.file)

#
#CHECK INTEGRITY OF THE FILES
#

#Check that there are no duplicate Dataset IDs/Distribution IDs
if(nrow(dataset.info[!duplicated(dataset.info[, "Dataset.ID"]), ]) != nrow(dataset.info)) stop("Duplicate Dataset IDs!")
if(nrow(BEAAPI.info[!duplicated(BEAAPI.info[, "Dataset.ID"]), ]) != nrow(BEAAPI.info)) stop("Duplicate Dataset IDs in BEA API info!")

#Check that the Dataset IDs in the dataset file match the Dataset IDs in the distribution file and viceversa
if(nrow(merge(dataset.info, BEAAPI.info, "Dataset.ID", all=TRUE)) != nrow(merge(dataset.info, BEAAPI.info, "Dataset.ID"))) stop("Mismatches between datasets and BEA API info available.")

#
#SET UP RDF INPUT AND BEA API CALLS 
#

DatasetSpecs.list <- apply(dataset.info, 1, function(x) as.list(x[!is.na(x)]))
BEAAPISpecs.list <- apply(BEAAPI.info, 1, function(x) as.list(x[!is.na(x)]))

BEAAPISpecs.list<-lapply(BEAAPISpecs.list, function(x) {
  
  l<-list()
  
  x$Dataset.ID <- NULL
  x$UserID<-bea.api.key
  
  if (x$datasetname == "regionaldata"){
    names(x)[which(names(x)=="table.name")]<-"keycode"
    l[[1]]<-x
  }
  
  else if (x$datasetname == "regionalproduct"){
    
    names(x)[which(names(x)=="table.name")]<-"component"
    
    parameter.input<-list(UserID=x$UserID, datasetname="regionalproduct", method="GetParameterValuesFiltered", ResultFormat=x$ResultFormat, TargetParameter="IndustryId", Component=x$component)
    parameter.url<-paste0(bea.api.url, "?", paste(names(parameter.input), parameter.input, sep="=", collapse = "&"))
    parameter<-fromJSON(getURL(parameter.url))$BEAAPI$Results$ParamValue
    
    NAICS.map<-as.character(shortlist()[["IndustryID"]])
    
    for (i in 1:length(parameter)) {
      if(parameter[[i]]$Key %in% NAICS.map) {
        x$IndustryId<-parameter[[i]]$Key
        l[[length(l)+1]]<-x
      }
    }  
  }
  
  else if (x$datasetname == "regionalincome"){
    
    names(x)[which(names(x)=="table.name")]<-"TableName"
    
    parameter.input<-list(UserID=x$UserID, datasetname="regionalincome", method="GetParameterValuesFiltered", ResultFormat=x$ResultFormat, TargetParameter="LineCode", TableName=x$TableName)
    parameter.url<-paste0(bea.api.url, "?", paste(names(parameter.input), parameter.input, sep="=", collapse = "&"))
    parameter<-fromJSON(getURL(parameter.url))$BEAAPI$Results$ParamValue
    
    NAICS.map<-as.character(shortlist()[["IndustryID"]])
    
    for (i in 1:length(parameter)) {
      if(parameter[[i]]$Key %in% NAICS.map) {
        x$LineCode<-parameter[[i]]$Key
        l[[length(l)+1]]<-x
      }
    }  
  }
  
  else stop("Unknown Dataset name")

  return(l)
  
})

DistributionSpecs.list<-lapply(BEAAPISpecs.list, function(x){
  
  l<-list()
  
  l$accessURL <- unlist (lapply(x, function(y){
    y$UserID<-"XXX"
    return(paste0(bea.api.url, "?", paste(names(y), y, sep="=", collapse = "&")))
  }))
  
  return(l)
  
})

for (i in 1:length(DatasetSpecs.list)){
  DatasetSpecs.list[[i]]$distribution<-list()
  DatasetSpecs.list[[i]]$distribution[[1]]<-DistributionSpecs.list[[i]]
}

#
#RETRIEVING DATA AND METADATA TO COMPLETE INFO
#(only looks at first accessURL)
#


for(i in 1:length(BEAAPISpecs.list)) {
  
  #Retrieve data and metadata
  
  DataSpecs<-BEAAPISpecs.list[[i]][[1]]
  data.url<-gsub("XXX", bea.api.key, DistributionSpecs.list[[i]]$accessURL)
  
  MetadataSpecs<-list(UserID=bea.api.key, method="GetData", datasetname="APIDatasetMetaData", ResultFormat="json", dataset=DataSpecs$datasetname)
  metadata.url<-paste0(bea.api.url, "?", paste(names(MetadataSpecs), MetadataSpecs, sep="=", collapse = "&"))

  data.info<-fromJSON(getURL(data.url))
  metadata.info<-fromJSON(getURL(metadata.url))
  data<-beaR::beaGet(DataSpecs, asWide = FALSE)
  
  #Retrieve dcat:Dataset properties
  
  if(DataSpecs$datasetname=="regionaldata") {
    title.list<-lapply(metadata.info$BEAAPI$Datasets[[1]]$Parameters[[1]]$Keycode$ParamValue, function(x) x$Desc)
    names(title.list)<-lapply(metadata.info$BEAAPI$Datasets[[1]]$Parameters[[1]]$Keycode$ParamValue, function(x) x$Key)
    DatasetSpecs.list[[i]]$title<-title.list[[DataSpecs$keycode]]
  }
  
  else if(DataSpecs$datasetname=="regionalproduct") {
    title.list<-lapply(metadata.info$BEAAPI$Datasets[[1]]$Parameters[[1]]$Component$ParamValue, function(x) x$Desc)
    names(title.list)<-lapply(metadata.info$BEAAPI$Datasets[[1]]$Parameters[[1]]$Component$ParamValue, function(x) x$Key)
    DatasetSpecs.list[[i]]$title<-title.list[[DataSpecs$component]]
  }
  
  else if(DataSpecs$datasetname=="regionalincome") {
    title.list<-lapply(metadata.info$BEAAPI$Datasets[[1]]$Parameters[[1]]$TableName$ParamValue, function(x) x$Desc)
    names(title.list)<-lapply(metadata.info$BEAAPI$Datasets[[1]]$Parameters[[1]]$TableName$ParamValue, function(x) x$Key)
    DatasetSpecs.list[[i]]$title<-title.list[[DataSpecs$TableName]]
  }
  
  DatasetSpecs.list[[i]]$statMeasure<-data.info$BEAAPI$Results$UnitOfMeasure
  DatasetSpecs.list[[i]]$temporal<-paste(min(data$TimePeriod), "-", max(data$TimePeriod))
  
  #Retrieve structure components
  
  Component.list<-lapply(data.info$BEAAPI$Results$Dimensions, function(x) {
    
    l<-list()
    
    l$type<-"component"
    l$order<-x$Ordinal
    l$label<-x$Name
    l$id<-x$Name
    
    return(l)
  })
  
  DatasetSpecs.list[[i]]$component<-Component.list
  
}

Dataset_Insert(DatasetSpecs.list, SPARQL.endpoint)
