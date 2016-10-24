library(XML)
library(httr)
library(RJSDMX)
source("Dataset_To_RDF.R", chdir=T)

Form_ComponentSpecs<-function(component.node) {
  
  l<-list()
  attrs<-names(xmlAttrs(component.node))
  
  if(xmlName(component.node)=="str:Dimension" || xmlName(component.node)=="str:TimeDimension") l$type<-"dimension"
  else if(xmlName(component.node)=="str:Attribute") l$type<-"attribute"
  else if(xmlName(component.node)=="str:PrimaryMeasure") l$type<-"measure"
  
  l$id<-xmlGetAttr(component.node, "id")
  l$label<-xmlGetAttr(component.node, "id")
  
  if("position" %in% attrs) l$order<-xmlGetAttr(component.node, "position")
  
  if("assignmentStatus" %in% attrs){
    if(xmlGetAttr(component.node, "assignmentStatus")=="Conditional") l$componentRequired<-"false"
    else if(xmlGetAttr(component.node, "assignmentStatus")=="Mandatory") l$componentRequired<-"true"
  }
  
  if(!is.null(component.node[["str:LocalRepresentation"]][["str:Enumeration"]][["Ref"]])) l$codeList<-xmlGetAttr(component.node[["str:LocalRepresentation"]][["str:Enumeration"]][["Ref"]], "id")
  
  return(l)
}

Form_CodelistSpecs<-function(codelist.node){
  
  l<-list()
 
  l$id<-xmlGetAttr(codelist.node, "id")
  l$label<-xmlValue(codelist.node[["com:Name"]])
  
  children<-xmlChildren(codelist.node)
  l$code<-lapply(children[names(children)=="str:Code"], function(x){
    
    m<-list()
    
    m$id<-xmlGetAttr(x, "id")
    m$notation<-xmlGetAttr(x, "id")
    m$label<-xmlValue(x[["com:Name"]])
    
    return(m)
    
  })
  
  names(l$code)<-NULL
  
  return(l)
}

#
#SET SPARQL ENDPOINT
#

SPARQL.endpoint<-"https://dydra.com/luca-gramaglia/eu-us-partnership-metadata-store/sparql"

#
#UPLOAD INPUT FILES
#

dataset.info.file<-"/home/luca/Desktop/R-SPARQL/EU-US Partnership/Metadata/ESTAT_To_RDF/Dataset_Template.csv"
API.info.file<-"/home/luca/Desktop/R-SPARQL/EU-US Partnership/Metadata/ESTAT_To_RDF/API_Template.csv"

dataset.info<-read.csv(dataset.info.file)
API.info<-read.csv(API.info.file)

#
#CHECK INTEGRITY OF THE FILES
#

#Check that there are no duplicate Dataset IDs/Distribution IDs
if(nrow(dataset.info[!duplicated(dataset.info[, "Dataset.ID"]), ]) != nrow(dataset.info)) stop("Duplicate Dataset IDs!")
if(nrow(API.info[!duplicated(API.info[, "Dataset.ID"]), ]) != nrow(API.info)) stop("Duplicate Dataset IDs in API info!")

#Check that the Dataset IDs in the dataset file match the Dataset IDs in the distribution file and viceversa
if(nrow(merge(dataset.info, API.info, "Dataset.ID", all=TRUE)) != nrow(merge(dataset.info, API.info, "Dataset.ID"))) stop("Mismatches between datasets and API info available.")

#
#SET DATASET AND DISTRIBUTION SPECS
#

DatasetSpecs.list <- apply(dataset.info, 1, function(x) as.list(x[!is.na(x)]))
APISpecs.list <- apply(API.info, 1, function(x) as.list(x[!is.na(x)]))
DistributionSpecs.list<-lapply(APISpecs.list, function(x){
  l<-list()
  l$accessURL <- paste("http://ec.europa.eu/eurostat/SDMX/diss-web/rest/data", x$Dataflow.ID, x$Subset, "all", sep="/" )
  return(l)
})

for (i in 1:length(DatasetSpecs.list)){
  DatasetSpecs.list[[i]]$distribution<-list()
  DatasetSpecs.list[[i]]$distribution[[1]]<-DistributionSpecs.list[[i]]
}

#
#RETRIEVE DSD AND SET SPECS
#

for (i in 1:length(APISpecs.list)){
  
  #Retrieve dataflow
  
  dataflow.id<-APISpecs.list[[i]]$Dataflow.ID
  dataflow.url<-paste("http://www.ec.europa.eu/eurostat/SDMX/diss-web/rest/dataflow/ESTAT/", dataflow.id, "/latest", sep="")
  dataflow<-xmlRoot(xmlTreeParse(content(GET(dataflow.url), "text")))
  
  #Set Dataset title
  
  DatasetSpecs.list[[i]]$title<-xmlValue(getNodeSet(dataflow, "//str:Dataflow/com:Name[@lang='en']")[[1]])
  
  #Retrieve DSD
  
  dsd.id<-xmlGetAttr(getNodeSet(dataflow, "//mes:Structures/str:Dataflows/str:Dataflow/str:Structure/Ref")[[1]], "id")
  dsd.url<-paste("http://ec.europa.eu/eurostat/SDMX/diss-web/rest/datastructure/ESTAT/", dsd.id, sep="")
  dsd<-xmlRoot(xmlTreeParse(content(GET(dsd.url), "text")))
  
  #Set Dataset components
  
  ComponentSpecs.list<-list()
  ComponentSpecs.list<-c(ComponentSpecs.list, lapply(getNodeSet(dsd, "//str:Dimension"), Form_ComponentSpecs))
  ComponentSpecs.list<-c(ComponentSpecs.list, lapply(getNodeSet(dsd, "//str:TimeDimension"), Form_ComponentSpecs))
  ComponentSpecs.list<-c(ComponentSpecs.list, lapply(getNodeSet(dsd, "//str:Attribute"), Form_ComponentSpecs))
  ComponentSpecs.list<-c(ComponentSpecs.list, lapply(getNodeSet(dsd, "//str:MeasureList/str:PrimaryMeasure"), Form_ComponentSpecs))
  
  DatasetSpecs.list[[i]]$component<-ComponentSpecs.list
  
  #Set codelists
  
  CodelistSpecs.list<-lapply(getNodeSet(dsd, "//str:Codelists/str:Codelist"), Form_CodelistSpecs)
  DatasetSpecs.list[[i]]$codelist<-CodelistSpecs.list
  
  #Set temporal range (hard-coded to TIME_PERIOD...)
  
  data<-getTimeSeriesTable(provider="EUROSTAT", id=paste(APISpecs.list[[i]]$Dataflow.ID, APISpecs.list[[i]]$Subset, sep="/"))
  DatasetSpecs.list[[i]]$temporal<-paste(min(as.numeric(as.character(data$TIME_PERIOD))), "-", max(as.numeric(as.character(data$TIME_PERIOD))))
  
}

Dataset_Insert(DatasetSpecs.list, SPARQL.endpoint)
