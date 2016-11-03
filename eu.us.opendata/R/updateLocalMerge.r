#' Update local merge metadata cache
#' 
#' @keywords internal
#' @return Local cache of merge metadata, as data.table
#' @import SPARQL RCurl
#' @export 

updateLocalMerge <- function(){

	requireNamespace('SPARQL', quietly = TRUE)
	requireNamespace('RCurl', quietly = TRUE)

endpt <- 'http://dydra.com/luca-gramaglia/eu-us-partnership-metadata-store/sparql'

quer <- 'PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX dcat: <http://www.w3.org/ns/dcat#>
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX struc: <http://example.org/struc/>

SELECT ?Merge_ID ?Source_Component ?Target_Component ?Map_Type ?Source_Value ?Target_Value WHERE { 
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
}';

qd <- SPARQL(endpt,quer)

localPath <- paste0(.libPaths()[1], '/eu.us.opendata/rawdata')
dir.create(localPath, showWarnings = FALSE, recursive = TRUE)

utils::write.csv2(qd$results, paste0(localPath, '/Merge_Table.csv'), quote = FALSE, row.names = FALSE)

		
}

