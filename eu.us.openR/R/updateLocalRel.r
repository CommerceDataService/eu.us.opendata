#' Update local relationship metadata cache
#' 
#' @keywords internal
#' @return Local cache of relationship metadata, as data.table
#' @import SPARQL RCurl
#' @export 

updateLocalRel <- function(){

	requireNamespace('SPARQL', quietly = TRUE)
	requireNamespace('RCurl', quietly = TRUE)

endpt <- 'http://dydra.com/luca-gramaglia/eu-us-partnership-metadata-store/sparql'

quer <- 'PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX dcat: <http://www.w3.org/ns/dcat#>
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX struc: <http://example.org/struc/>
PREFIX stat: <http://example.org/stat/>

SELECT 
?Rel_ID ?Rel_name ?BEA_ID ?EU_ID ?BEA_Merge_ID ?EU_Merge_ID
?BEA_Geo ?EU_Geo ?BEA_Period ?EU_Period ?Freq 
?BEA_Unit ?EU_Unit 

WHERE{

?Rel_ID a dcat:Dataset; 
  	dct:publisher <JOINT> .
?BEA_Dset a dcat:Dataset; 
		dct:publisher <BEA> .
?EU_Dset a dcat:Dataset; 
		dct:publisher <ESTAT> .

?BEA_Merge_ID a struc:StructureMap; 
		struc:sourceStructure ?BEA_Dset; 
		struc:targetStructure ?Rel_ID .

?EU_Merge_ID a struc:StructureMap; 
		struc:sourceStructure ?EU_Dset; 
		struc:targetStructure ?Rel_ID .

?Rel_ID dct:title ?Rel_name;
		dct:accrualPeriodicity ?Freq .    

?BEA_Dset dct:spatial ?BEA_Geo;
		dct:temporal ?BEA_Period;
		stat:statMeasure ?BEA_Unit;
    dcat:distribution/dcat:accessURL ?BEA_ID.

?EU_Dset dct:spatial ?EU_Geo;
		dct:temporal ?EU_Period;
		stat:statMeasure ?EU_Unit;
    dcat:distribution/dcat:accessURL ?EU_ID.

}';

qd <- SPARQL(endpt,quer)

localPath <- paste0(.libPaths()[1], '/eu.us.openR/rawdata')
dir.create(localPath, showWarnings = FALSE, recursive = TRUE)

utils::write.csv2(qd$results, paste0(localPath, '/Relationship_Table.csv'), quote = FALSE, row.names = FALSE)

		
}

