#' Update local structure metadata cache
#' 
#' @return Local cache of structure metadata, as data.table
#' @import data.table
#' @export 

updateLocalStruc <- function(){


	requireNamespace('SPARQL')
	requireNamespace('RCurl')

endpt <- 'http://dydra.com/luca-gramaglia/eu-us-partnership-metadata-store/sparql'

quer <- '		
PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX dcat: <http://www.w3.org/ns/dcat#>
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX struc: <http://example.org/struc/>

SELECT ?Structure_ID ?Component WHERE { 
  ?Structure_ID a dcat:Dataset; dct:publisher <JOINT> .
  
  ?Structure_ID qb:component ?x .
  ?x qb:dimension|qb:attribute|qb:measure|qb:componentProperty ?CompID.
  ?CompID rdfs:label ?Component .

}';

	qd <- SPARQL(endpt,quer)

	localPath <- paste0(.libPaths()[1], '/euroStates/rawdata/Structure_Table.csv')

	write.csv2(qd$results, localPath, quote = FALSE, row.names = FALSE)


}

