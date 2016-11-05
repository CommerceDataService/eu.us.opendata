#' Add query namespaces
#' 
#' @param query	A SPARQL query string that you want namespaces for
#' @keywords internal
#' @return Query string with namespaces
#' @export 



add.namespace<-function(query){
  
  namespace.qb<-"PREFIX qb: <http://purl.org/linked-data/cube#>"
  namespace.dcat<-"PREFIX dcat: <http://www.w3.org/ns/dcat#>"
  namespace.dct<-"PREFIX dct: <http://purl.org/dc/terms/>"
  namespace.sdmx<-"PREFIX struc: <http://example.org/struc/>"
  namespace.stat<-"PREFIX stat: <http://example.org/stat/>"
  namespace.pav<-"PREFIX pav: <http://purl.org/pav/>"
  
  query<-paste(namespace.qb, namespace.dcat, namespace.dct, namespace.sdmx, namespace.stat, namespace.pav, query)
  
  return(query)  
  
}
