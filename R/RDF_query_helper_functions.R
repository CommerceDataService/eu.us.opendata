library(SPARQL)

insert.query.builder<-function(query){
  
  namespace.qb<-"PREFIX qb: <http://purl.org/linked-data/cube#>"
  namespace.dcat<-"PREFIX dcat: <http://www.w3.org/ns/dcat#>"
  namespace.dct<-"PREFIX dct: <http://purl.org/dc/terms/>"
  namespace.sdmx<-"PREFIX struc: <http://example.org/struc/>"
  namespace.stat<-"PREFIX stat: <http://example.org/stat/>"
  
  query<-paste("INSERT DATA {", query,  "}")
  query<-paste(namespace.qb, namespace.dcat, namespace.dct, namespace.sdmx, namespace.stat, query)
  
  return(query)  
  
}

add.namespace<-function(query){
  
  namespace.qb<-"PREFIX qb: <http://purl.org/linked-data/cube#>"
  namespace.dcat<-"PREFIX dcat: <http://www.w3.org/ns/dcat#>"
  namespace.dct<-"PREFIX dct: <http://purl.org/dc/terms/>"
  namespace.sdmx<-"PREFIX struc: <http://example.org/struc/>"
  namespace.stat<-"PREFIX stat: <http://example.org/stat/>"
  
  query<-paste(namespace.qb, namespace.dcat, namespace.dct, namespace.sdmx, namespace.stat, query)
  
  return(query)  
  
}

large.query.handler<-function(SPARQL.endpoint, query.list, query.builder, limit=5000){
  
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
    results<-rbind(results, SPARQL(SPARQL.endpoint,query)$results)
    
  }
  
  return(results)
}

