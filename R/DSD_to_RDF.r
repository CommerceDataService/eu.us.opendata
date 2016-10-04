#' Inserts an SDMX DSD into a triple store as an RDF Data Cube Vocabulary DataStructureDefinition.   
#' 
#' @param SPARQL.endpoint		the SPARQL endpoint URL of the target triple store
#' @param data.structure		SDMX DSD object
#' @return NULL
#' @import 	SPARQL rsdmx

DSD_To_RDF <- function(SPARQL.endpoint, data.structure) {
  
  requireNamespace('SPARQL', quietly = TRUE)
	requireNamespace('rsdmx', quietly = TRUE)


  dimension.list <- data.structure@Components@Dimensions
  attribute.list <- data.structure@Components@Attributes
  primary.measure <- data.structure@Components@PrimaryMeasure
  time.dimension <- data.structure@Components@TimeDimension
  
  query.dsd  <-  NULL
  query.component <- NULL
  blank.counter <- 0
  
  #Set RDF statements for DataStructureDefinition
  
  dsd.id <- paste("<", data.structure@id, ">", sep="")
  query.dsd <- c(query.dsd, paste(dsd.id, "rdf:type qb:DataStructureDefinition ."))
  
  dsd.label <- paste("\"", data.structure@id, "\"", sep="")
  query.dsd <- c(query.dsd, paste(dsd.id, "rdfs:label", dsd.label, "."))
  
  #Set RDF statements for dimension components
  
  for (i in 1:length(dimension.list)){
    
    id <- paste("<", dimension.list[[i]]@conceptRef, ">", sep="")
    query.component <- c(query.component, paste(id, "rdf:type qb:DimensionProperty ."))
    
    blank.id <- Blank_ID(blank.counter)
    
    query.dsd <- c(query.dsd, paste(dsd.id, "qb:component", blank.id, "."))
    query.dsd <- c(query.dsd, paste(blank.id, "rdf:type qb:ComponentSpecification ."))
    query.dsd <- c(query.dsd, paste(blank.id, "qb:dimension" , id, "."))
    
    blank.counter <- blank.counter+1
    
    label <- paste("\"", dimension.list[[i]]@conceptRef, "\"", sep="")
    query.component <- c(query.component, paste(id, "rdfs:label", label, "."))
    
    if(!is.na(dimension.list[[i]]@codelist)){
      
      query.component <- c(query.component, paste(id, "rdf:type qb:CodedProperty ."))
      
      code.list <- paste("<", dimension.list[[i]]@codelist, ">", sep="")
      query.component <- c(query.component, paste(id, "qb:codeList", code.list, "."))
    }
  }

  #Set RDF statements for attribute components
  
  for (i in 1:length(attribute.list)){
    
    id <- paste("<", attribute.list[[i]]@conceptRef, ">", sep="")
    query.component <- c(query.component, paste(id, "rdf:type qb:AttributeProperty ."))

    blank.id <- Blank_ID(blank.counter)
    
    query.dsd <- c(query.dsd, paste(dsd.id, "qb:component", blank.id, "."))
    query.dsd <- c(query.dsd, paste(blank.id, "rdf:type qb:ComponentSpecification ."))
    query.dsd <- c(query.dsd, paste(blank.id, "qb:attribute" , id, "."))
    
    blank.counter <- blank.counter+1
    
    label <- paste("\"", attribute.list[[i]]@conceptRef, "\"", sep="")
    query.component <- c(query.component, paste(id, "rdfs:label", label, "."))
    
    if(!is.na(attribute.list[[i]]@codelist)){
      
      query.component <- c(query.component, paste(id, "rdf:type qb:CodedProperty ."))
      
      code.list <- paste("<", attribute.list[[i]]@codelist, ">", sep="")
      query.component <- c(query.component, paste(id, "qb:codeList", code.list, "."))
    }
  }

  #Set RDF statements for time dimension component
  
  id <- paste("<", time.dimension@conceptRef, ">", sep="")
  query.component <- c(query.component, paste(id, "rdf:type qb:DimensionProperty ."))

  blank.id <- Blank_ID(blank.counter)
  
  query.dsd <- c(query.dsd, paste(dsd.id, "qb:component", blank.id, "."))
  query.dsd <- c(query.dsd, paste(blank.id, "rdf:type qb:ComponentSpecification ."))
  query.dsd <- c(query.dsd, paste(blank.id, "qb:dimension" , id, "."))
  
  blank.counter <- blank.counter+1
  
  label <- paste("\"", time.dimension@conceptRef, "\"", sep="")
  query.component <- c(query.component, paste(id, "rdfs:label", label, "."))
  
  if(!is.na(time.dimension@codelist)){
    
    query.component <- c(query.component, paste(id, "rdf:type qb:CodedProperty ."))
    
    code.list <- paste("<", time.dimension@codelist, ">", sep="")
    query.component <- c(query.component, paste(id, "qb:codeList", code.list, "."))
  }

  #Set RDF statements for primary measure components
  
  id <- paste("<", primary.measure@conceptRef, ">", sep="")
  query.component <- c(query.component, paste(id, "rdf:type qb:MeasureProperty ."))

  blank.id <- Blank_ID(blank.counter)
  
  query.dsd <- c(query.dsd, paste(dsd.id, "qb:component", blank.id, "."))
  query.dsd <- c(query.dsd, paste(blank.id, "rdf:type qb:ComponentSpecification ."))
  query.dsd <- c(query.dsd, paste(blank.id, "qb:measure" , id, "."))
  
  blank.counter <- blank.counter+1
  
  label <- paste("\"", primary.measure@conceptRef, "\"", sep="")
  query.component <- c(query.component, paste(id, "rdfs:label", label, "."))
  
  if(!is.na(primary.measure@codelist)){
    
    query.component <- c(query.component, paste(id, "rdf:type qb:CodedProperty ."))
    
    code.list <- paste("<", primary.measure@codelist, ">", sep="")
    query.component <- c(query.component, paste(id, "qb:codeList", code.list, "."))
  }
    
  insert.output <- large.query.handler(SPARQL.endpoint, query.dsd, query.builder)
  insert.output <- large.query.handler(SPARQL.endpoint, query.component, query.builder)
  
}

