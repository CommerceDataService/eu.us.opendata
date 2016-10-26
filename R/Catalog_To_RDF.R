source("RDF_query_helper_functions.R", chdir=T)

Catalog_To_RDF<-function(CatalogSpecs){
  
  query.list<-NULL
  
  if(!is.null(CatalogSpecs$Catalog.ID)){
    id<- paste("<", CatalogSpecs$Catalog.ID, ">", sep="")
    query.list<-c(query.list, paste(id, "rdf:type dcat:Catalog ."))
  }
  
  if(!is.null(CatalogSpecs$title)){
    title<-paste("\"", CatalogSpecs$title, "\"", sep="")
    query.list<-c(query.list, paste(id, "dct:title", title, "."))
  }
  
  if(!is.null(CatalogSpecs$description)){
    description<-paste("\"", CatalogSpecs$description, "\"", sep="")
    query.list<-c(query.list, paste(id, "dct:description", description, "."))
  }
  
  if(!is.null(CatalogSpecs$publisher)){
    publisher<-paste("<", CatalogSpecs$publisher, ">", sep="")
    query.list<-c(query.list, paste(id, "dct:publisher", publisher, "."))
  }
  
  time<-paste("\"", as.POSIXlt(Sys.time(), "GMT"), "\"", sep="")
  query.list<-c(query.list, paste(id, "pav:lastUpdateOn", time, "."))

  return(query.list)
  
}

Catalog_Insert<-function(CatalogSpecs.list, SPARQL.endpoint) {
  
  Catalog.query.list<-unlist(lapply(CatalogSpecs.list, Catalog_To_RDF))
  
  large.query.handler(SPARQL.endpoint, Catalog.query.list, insert.query.builder)
  
  insert.catalog.last.update(as.POSIXlt(Sys.time(), "GMT"), SPARQL.endpoint)
}