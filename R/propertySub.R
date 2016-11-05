#' Quick query to Dydra repo for properties to be used as labels and substitution from filtered localMerge in getRel
#' 
#' @param localMerge	Local Merge metadata, typically passed by getRel
#' @param filterIDs	Character vector of IDs to filter property map, typically passed by getRel
#' @keywords internal
#' @return Value map to be used in getRel
#' @import data.table SPARQL
#' @export 


propertySub <- function(localMrg = loadLocalMerge(), filterIDs = c()){
	Map_Type			<- NULL
	Merge_ID			<- NULL
	Target_Value	<- NULL
	
	requireNamespace('SPARQL', silent = TRUE)
	requireNamespace('data.table', silent = TRUE)
	
	PropertyMap <- localMrg[grep("PropertyMap", Map_Type)][Merge_ID %in% filterIDs]
	MergeID <- localMrg[!grep("PropertyMap", Map_Type),]

	
	endpt <- 'http://dydra.com/luca-gramaglia/eu-us-partnership-metadata-store/sparql'
	
	ValueMap <- rbindlist(apply(PropertyMap, 1, function(x){

		property.map.query <- paste("SELECT ?source ?target {", x["Merge_ID"], "struc:sourceStructure ?ds .
		?ds qb:component/(qb:componentProperty|qb:dimension|qb:measure|qb:attribute) ?Comp .
		?Comp rdfs:label", paste("\"", x["Source_Component"], "\"", sep=""), ".
		?Comp qb:codeList/skos:hasTopConcept ?x .
		?x", x["Source_Value"], "?source .
		?x", x["Target_Value"], "?target . }")
		
		property.map.query<-add.namespace(property.map.query)
		
		ValueMap <- SPARQL(endpt, property.map.query)$results
		ValueMap <- cbind(x[1], x[2], x[3], x[4], ValueMap, row.names=NULL)
		colnames(ValueMap) <- c("Merge_ID", "Source_Component", "Target_Component", "Map_Type", "Source_Value", "Target_Value")
		
		return(ValueMap)
	}))

	#ValueMap$Map_Type<-gsub("PropertyMap", "ValueMap", ValueMap$Map_Type)

	#MergeID<-rbind(MergeID, ValueMap)
	ValueMap[, Target_Value := iconv(Target_Value, from="UTF-8", to = "UTF-8")]
	
	return(ValueMap)
}