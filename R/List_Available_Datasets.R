library(SPARQL)

SPARQL.endpoint<-"https://dydra.com/luca-gramaglia/eu-us-partnership-metadata-store/sparql"

List_Available_Datasets<-function(SPARQL.endpoint){

query <- "
PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX dcat: <http://www.w3.org/ns/dcat#>
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX struc: <http://www.example.org/struc/>
  
SELECT ?Title ?Description ?EUSource WHERE { 
  ?x rdf:type dcat:Dataset ; 
     dct:publisher <EU_US_Partnership> ; 
     dct:title ?Title ; 
     dct:description ?Description . 
  ?s struc:sourceStructureUsage ?EUSource; 
     struc:targetStructureUsage ?x
}" 

return(SPARQL(SPARQL.endpoint, query)$results)

}

