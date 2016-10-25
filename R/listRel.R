#' List all datasets available
#' 
#' @param asHtml  Option to render results in an interactive DT
#' @import SPARQL DT
#' @export 

listRel <- function(asHtml = TRUE){

	requireNamespace('SPARQL', quietly = TRUE)
	requireNamespace('DT', quietly = TRUE)

	endpt <- 'http://dydra.com/luca-gramaglia/eu-us-partnership-metadata-store/sparql'
	
	quer <- "PREFIX dcat: <http://www.w3.org/ns/dcat#>
		PREFIX qb: <http://purl.org/linked-data/cube#> 
		PREFIX dct: <http://purl.org/dc/terms/>
		
		SELECT ?x WHERE {?x a dcat:Dataset}";
	
	relDT <- data.table::data.table(t(SPARQL::SPARQL(endpt, quer)$results))
	data.table::setnames(relDT, 'V1', 'Dataset')
	
	if(asHtml == TRUE){
		DT::datatable(relDT)
	} else {
		return(relDT)
	}
}

