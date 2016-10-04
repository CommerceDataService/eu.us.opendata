#' Inserts an SDMX codelist into a triple store as an RDF SKOS ConceptScheme.   
#'
#' @param SPARQL.endpoint		the SPARQL endpoint URL of the target triple store
#' @param code.lists		a list of SDMX codelist objects
#' @return NULL
#' @import 	SPARQL rsdmx

CodeList_To_RDF <- function(SPARQL.endpoint, code.lists){
  
	requireNamespace('SPARQL', quietly = TRUE)
	requireNamespace('rsdmx', quietly = TRUE)

  for (i in 1:length(code.lists)){
    
    query.scheme <- NULL
    query.concept <- NULL
    
    #Set concept scheme RDF statements
    
    scheme.name <- code.lists[[i]]@id
    print(paste("\t Codelist", scheme.name))
    
    scheme.id <- paste("<", code.lists[[i]]@id, ">", sep="")
    query.scheme <- c(query.scheme, paste(scheme.id, "rdf:type skos:ConceptScheme ."))
    
    scheme.label <- paste("\"", code.lists[[i]]@Name[["en"]], "\"", sep="")
    query.scheme <- c(query.scheme, paste(scheme.id, "skos:prefLabel", scheme.label, "."))
    
    codes <- code.lists[[i]]@Code
    
    #Set RDF statements for each individual code
    
    for(j in 1:length(codes)){
      
      code.id <- paste("<", scheme.name, "_", codes[[j]]@id, ">", sep="")
      query.concept <- c(query.concept, paste(code.id, "rdf:type skos:Concept ."))
      query.concept <- c(query.concept, paste(code.id, "skos:inScheme", scheme.id, "."))
      query.scheme <- c(query.scheme, paste(scheme.id, "skos:hasTopConcept", code.id, "."))
      
      code.notation <- paste("\"", codes[[j]]@id, "\"", sep="")
      query.concept <- c(query.concept, paste(code.id, "skos:notation", code.notation, "."))
      
      code.label <- paste("\"", codes[[j]]@label[["en"]], "\"", sep="")
      query.concept <- c(query.concept, paste(code.id, "skos:prefLabel", code.label, "."))
      
    }
    
    #Insert RDF statements about the codes and about the concept scheme
    
    insert.output <- large.query.handler(SPARQL.endpoint, query.scheme, query.builder)
    insert.output <- large.query.handler(SPARQL.endpoint, query.concept, query.builder)
    
  }
  
}

