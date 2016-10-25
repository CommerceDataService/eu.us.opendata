#' Update local merge metadata cache
#' 
#' @return Local cache of merge metadata, as data.table
#' @import SPARQL RCurl
#' @export 

updateLocalMerge <- function(){

	requireNamespace('SPARQL')
	requireNamespace('RCurl')

endpt <- 'http://dydra.com/luca-gramaglia/eu-us-partnership-metadata-store/sparql'

quer <- 'PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX dcat: <http://www.w3.org/ns/dcat#>
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX struc: <http://example.org/struc/>

SELECT ?Merge_ID ?Source_Component ?Target_Component ?Map_Type ?Source_Value ?Target_Value WHERE { 
  ?Merge_ID a struc:StructureMap .
  
  ?s struc:hasComponentMap ?CompID .
  ?CompID struc:source ?x .
  ?x rdfs:label ?Source_Component .
  ?CompID struc:target ?y .
  ?y rdfs:label ?Target_Component .

  ?CompID struc:hasRepresentationMapping ?MapID .
  ?MapID rdf:type ?Map_Type .
  ?MapID struc:source ?Source_Value .
  ?MapID struc:source ?Target_Value .
}';

qd <- SPARQL(endpt,quer)

localPath <- paste0(.libPaths()[1], '/euroStates/rawdata/Merge_Table.csv')

write.csv2(qd$results, localPath, quote = FALSE, row.names = FALSE)

		
}

