source("Catalog_To_RDF.R", chdir=T)

#
#SET SPARQL ENDPOINT
#

SPARQL.endpoint<-"https://dydra.com/luca-gramaglia/eu-us-partnership-metadata-store/sparql"

#
#UPLOAD INPUT FILES
#

catalog.info.file<-"Metadata/CATALOG_To_RDF/Catalog_Template.csv"

catalog.info<-read.csv(catalog.info.file)

#
#CHECK INTEGRITY OF THE FILES
#

#Check that there is only one catalogue
if(nrow(catalog.info)!=1) stop("One and only one catalog must be defined")

#
#UPLOAD CATALOG INFO
#

CatalogSpecs.list <- apply(catalog.info, 1, function(x){
  
  l<-as.list(x[!is.na(x)])
  
  return(l)
})

Catalog_Insert(CatalogSpecs.list, SPARQL.endpoint)
