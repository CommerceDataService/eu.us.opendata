library(SPARQL)
library(data.table)
source("RDF_query_helper_functions.R", chdir=T)

SPARQL.endpoint<-"https://dydra.com/luca-gramaglia/eu-us-partnership-metadata-store/sparql"

MergeID.query<-"SELECT ?Merge_ID ?Source_Component ?Target_Component ?Map_Type ?Source_Value ?Target_Value WHERE { 

?Merge_ID a struc:StructureMap .
?Merge_ID struc:hasComponentMap ?CompID .

?CompID struc:source ?x .
?x rdfs:label ?Source_Component .
?CompID struc:target ?y .
?y rdfs:label ?Target_Component .
?CompID struc:hasRepresentationMapping ?MapID .

?MapID rdf:type ?Map_Type .
?MapID struc:source ?Source_Value .
?MapID struc:target ?Target_Value .
}
"

MergeID.query<-add.namespace(MergeID.query)

MergeID<-SPARQL(SPARQL.endpoint, MergeID.query)$results

PropertyMap<-MergeID[grepl("PropertyMap", MergeID$Map_Type),]
MergeID<-MergeID[!grepl("PropertyMap", MergeID$Map_Type),]
             
ValueMap<-rbindlist(apply(PropertyMap, 1, function(x){

  property.map.query<-paste("SELECT ?source ?target {", x["Merge_ID"], "struc:sourceStructure ?ds .
  ?ds qb:component/(qb:componentProperty|qb:dimension|qb:measure|qb:attribute) ?Comp .
  ?Comp rdfs:label", paste("\"", x["Source_Component"], "\"", sep=""), ".
  ?Comp qb:codeList/skos:hasTopConcept ?x .
  ?x", x["Source_Value"], "?source .
  ?x", x["Target_Value"], "?target . }")
  
  property.map.query<-add.namespace(property.map.query)
  
  ValueMap<-SPARQL(SPARQL.endpoint, property.map.query)$results
  ValueMap<-cbind(x[1], x[2], x[3], x[4], ValueMap, row.names=NULL)
  colnames(ValueMap) <-c("Merge_ID", "Source_Component", "Target_Component", "Map_Type", "Source_Value", "Target_Value")
  
  return(ValueMap)
}))

ValueMap$Map_Type<-gsub("PropertyMap", "ValueMap", ValueMap$Map_Type)

MergeID<-rbind(MergeID, ValueMap)
