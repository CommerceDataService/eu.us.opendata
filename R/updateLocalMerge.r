#' Update local merge metadata cache
#' 
#' @return Local cache of merge metadata, as data.table
#' @import SPARQL RCurl
#' @export 

updateLocalMerge <- function(uid, pwd){

	requireNamespace('SPARQL')
	requireNamespace('RCurl')

quer <- 'PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX dcat: <http://www.w3.org/ns/dcat#>
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX struc: <http://www.example.org/struc/>
PREFIX stat: <http://example.org/stat/>

SELECT ?MapType_MergeID ?CompID ?Source_Component ?Target_Component WHERE { 
	?MapType_MergeID struc:sourceStructureUsage <nama_10r_2gdp>.
	?MapType_MergeID struc:targetStructureUsage ?x.
	?x dct:title "GDP_A_2".
	?MapType_MergeID struc:hasComponentMap ?CompID .
	?CompID struc:source ?s.
	?s rdfs:label ?Source_Component.
	?CompID struc:target ?w.
	?w rdfs:label ?Target_Component.
}';

qd <- SPARQL(endpt,quer,curl_args=c('userpwd'=paste0(uid,':',pwd)))



		
}

