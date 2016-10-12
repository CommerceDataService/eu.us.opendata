#' Create RDF from DSD data tree
#' 
#' @param DSD.tree 	 Hierarchical DSD 
#' @return A vector containing 
#' @import SPARQL data.tree
#' @export 


Create_RDF_DSD <- function(DSD.tree) {
  
  query.dsd  <-  NULL
  blank.counter <- 0
  
	requireNamespace('SPARQL', quietly = TRUE)
	requireNamespace('data.tree', quietly = TRUE)
  
  for (i in 1:length(DSD.tree$children)){
    
    dsd.id <- paste("<", DSD.tree$children[[i]]$name, ">", sep="")
    query.dsd <- c(query.dsd, paste(dsd.id, "rdf:type qb:DataStructureDefinition ."))
    
    dsd.label <- paste("\"", DSD.tree$children[[i]]$DSD.label, "\"", sep="")
    query.dsd <- c(query.dsd, paste(dsd.id, "rdfs:label", dsd.label, "."))
    
    for (j in 1:length(DSD.tree$children[[i]]$children)){
      
      component.id <- paste("<", DSD.tree$children[[i]]$children[[j]]$name, ">", sep="")
      component.property <- paste("qb:", DSD.tree$children[[i]]$children[[j]]$Component.role, "Property", sep="")
      query.dsd <- c(query.dsd, paste(component.id, "rdf:type", component.property, "."))
      
      blank.id <- Blank_ID(blank.counter)
      
      query.dsd <- c(query.dsd, paste(dsd.id, "qb:component", blank.id, "."))
      query.dsd <- c(query.dsd, paste(blank.id, "rdf:type qb:ComponentSpecification ."))
      component.role <- paste("qb:", tolower(DSD.tree$children[[i]]$children[[j]]$Component.role), sep="")
      query.dsd <- c(query.dsd, paste(blank.id, component.role , component.id, "."))      
      
      blank.counter <- blank.counter+1
      
      label <- paste("\"", DSD.tree$children[[i]]$children[[j]]$Component.label, "\"", sep="")
      query.dsd <- c(query.dsd, paste(component.id, "rdfs:label", label, "."))
      
      if(DSD.tree$children[[i]]$children[[j]]$Coded){
        
        query.dsd <- c(query.dsd, paste(component.id, "rdf:type qb:CodedProperty ."))
        
        code.list <- paste("<", DSD.tree$children[[i]]$children[[j]]$Codelist.ID, ">", sep="")
        query.dsd <- c(query.dsd, paste(component.id, "qb:codeList", code.list, "."))
      }      
      
    }
  }
  
  return(query.dsd)
  
}

