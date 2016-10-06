library(SPARQL)
library(rsdmx)
library(eurostat)

SPARQL.endpoint<-"https://dydra.com/luca-gramaglia/eu-us-partnership-metadata-store/sparql"

dataset.name<-"GDP_A_2"

Get_Dataset<-function(SPARQL.endpoint, dataset.name){
  
  #Get Source Dataset name
  
  dataset.title<-paste("\"", dataset.name, "\"", sep="")
  
  query.source.dataset <- paste("
    PREFIX qb: <http://purl.org/linked-data/cube#>
    PREFIX dcat: <http://www.w3.org/ns/dcat#>
    PREFIX dct: <http://purl.org/dc/terms/>
    PREFIX struc: <http://www.example.org/struc/>
  
    SELECT ?EUSource WHERE { 
      ?x rdf:type dcat:Dataset ; 
         dct:publisher <EU_US_Partnership> ; 
         dct:title", dataset.title, ". 
      ?s struc:sourceStructureUsage ?EUSource; 
         struc:targetStructureUsage ?x
    }") 
  
  source.dataset.id<- SPARQL(SPARQL.endpoint, query.source.dataset)$results                             
  
  source.dataset.id<-source.dataset.id[1,1]
  source.dataset.name<-gsub("<", "", source.dataset.id)
  source.dataset.name<-gsub(">", "", source.dataset.name)
 
  #Retrieve source data
  
  #Neither rsdmx nor eurostat package work, need to find workaround.
  #source.dataset<- as.data.frame(readSDMX(providerId = "ESTAT", resource = "data", flowRef=source.dataset.name, key = list("", "", "", "")))  
  #source.dataset<-get_eurostat(source.dataset.name, time_format = "num")
  
  #Retrieve mappings
  
  query.component.map<-paste("
    PREFIX qb: <http://purl.org/linked-data/cube#>
    PREFIX dcat: <http://www.w3.org/ns/dcat#>
    PREFIX dct: <http://purl.org/dc/terms/>
    PREFIX struc: <http://www.example.org/struc/>
                   
    SELECT ?CompID ?CompS ?CompT WHERE { 
       ?s struc:sourceStructureUsage", source.dataset.id, ".
       ?s struc:targetStructureUsage ?x.
       ?x dct:title", dataset.title, ".
       ?s struc:hasComponentMap ?CompID .
       ?CompID struc:source ?z.
       ?z rdfs:label ?CompS.
       ?CompID struc:target ?w.
       ?w rdfs:label ?CompT.
    }")
  
  query.value.map<-paste("
    PREFIX qb: <http://purl.org/linked-data/cube#>
    PREFIX dcat: <http://www.w3.org/ns/dcat#>
    PREFIX dct: <http://purl.org/dc/terms/>
    PREFIX struc: <http://www.example.org/struc/>
    
    SELECT ?CompID ?ValS ?ValT WHERE { 
        ?s struc:sourceStructureUsage", source.dataset.id, ".
        ?s struc:targetStructureUsage ?x.
        ?x dct:title", dataset.title, ".
        ?s struc:hasComponentMap ?CompID .
        ?CompID struc:hasRepresentationMapping ?r.
        ?r struc:sourceValue ?ValS ;
           struc:targetValue ?ValT.
      }")
                        
  component.map<- SPARQL(SPARQL.endpoint, query.component.map)$results
  value.map<-SPARQL(SPARQL.endpoint, query.value.map)$results
  
}