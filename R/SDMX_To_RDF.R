library(SPARQL)
library(rsdmx)

# query.builder: creates a well-formed INSERT SPARQL query by adding the INSERT clause and the required namespace prefixes
# returns: a well-formed SPARQL INSERT query
# @query: the desired content of the INSERT clause

query.builder<-function(query){
  
  namespace.qb<-"PREFIX qb: <http://purl.org/linked-data/cube#>"
  namespace.dcat<-"PREFIX dcat: <http://www.w3.org/ns/dcat#>"
  namespace.dct<-"PREFIX dct: <http://purl.org/dc/terms/>"
  
  query<-paste("INSERT DATA {", query,  "}")
  query<-paste(namespace.qb, namespace.dcat, namespace.dct, query)
  
  return(query)  
  
}

# large.query.handler: this function takes a list of RDF statements (triples) and:
# - divides them into chunks
# - turns them into a full-fledged SPARQL INSERT query using the query.builder function
# - inserts them into the target triple store by executing the SPARQL INSERT query
# returns: NA
# @endpoint: the SPARQL endpoint URL of the target triple store
# @query.list: a list of RDF statements
# @query.builder: the function to be used to build the query
# @limit: maximum number of characters in each chunk

large.query.handler<-function(endpoint, query.list, query.builder, limit=5000){
  
  results<-NULL
  i<-1
  
  while (i<length(query.list)){
    
    begin<-i
    size<-0
    while (size + nchar(query.list[i]) < limit && i<length(query.list)) {
      i<-i+1
      size<-size+nchar(query.list[i])
    }
    
    query<-paste(query.list[begin:i], collapse="")
    query<-query.builder(query)
    results<-rbind(results, SPARQL(endpoint,query)$results)
    
  }
  
  return(results)
}

# Blank_ID: utility function to create a blank node ID
# returns: a blank node ID
# @blank.counter: counter distinguishing the blank nodes

Blank_ID<-function(blank.counter){
  
  return(paste("_:b", blank.counter, sep=""))
  
}

# Dataflow_To_RDF: inserts an SDMX dataflow into a triple store as an RDF Data Cube Vocabulary dataset. The dataset is complemented with metadata 
# which follows the DCAT ontology.  
# returns: NA
# @SPARQL.endpoint: the SPARQL endpoint URL of the target triple store
# @dataflow: SDMX dataflow object

Dataflow_To_RDF<-function(SPARQL.endpoint, dataflow){
  
  query.list<-NULL
  
  id<- paste("<", dataflow@id, ">", sep="")
  query.list<-c(query.list, paste(id, "rdf:type qb:DataSet, dcat:Dataset ."))
  
  title<-paste("\"", dataflow@id, "\"", sep="")
  query.list<-c(query.list, paste(id, "dct:title", title, "."))
  
  description<-paste("\"", dataflow@Name[["en"]], "\"", sep="")
  query.list<-c(query.list, paste(id, "dct:description", description, "."))
  
  publisher<-paste("<", dataflow@agencyID, ">", sep="")
  query.list<-c(query.list, paste(id, "dct:publisher", publisher, "."))
  
  structure<-paste("<", dataflow@dsdRef, ">", sep="")
  query.list<-c(query.list, paste(id, "qb:structure", structure, "."))
  
  insert.output<-large.query.handler(SPARQL.endpoint, query.list, query.builder)
  
}

# DSD_To_RDF: inserts an SDMX DSD into a triple store as an RDF Data Cube Vocabulary DataStructureDefinition.   
# returns: NA
# @SPARQL.endpoint: the SPARQL endpoint URL of the target triple store
# @data.structure: SDMX DSD object

DSD_To_RDF<-function(SPARQL.endpoint, data.structure) {
  
  
  dimension.list<-data.structure@Components@Dimensions
  attribute.list<-data.structure@Components@Attributes
  primary.measure<-data.structure@Components@PrimaryMeasure
  time.dimension<-data.structure@Components@TimeDimension
  
  query.dsd <- NULL
  query.component<-NULL
  blank.counter<-0
  
  #Set RDF statements for DataStructureDefinition
  
  dsd.id<-paste("<", data.structure@id, ">", sep="")
  query.dsd<-c(query.dsd, paste(dsd.id, "rdf:type qb:DataStructureDefinition ."))
  
  dsd.label<-paste("\"", data.structure@id, "\"", sep="")
  query.dsd<-c(query.dsd, paste(dsd.id, "rdfs:label", dsd.label, "."))
  
  #Set RDF statements for dimension components
  
  for (i in 1:length(dimension.list)){
    
    id<-paste("<", dimension.list[[i]]@conceptRef, ">", sep="")
    query.component<-c(query.component, paste(id, "rdf:type qb:DimensionProperty ."))
    
    blank.id<-Blank_ID(blank.counter)
    
    query.dsd<-c(query.dsd, paste(dsd.id, "qb:component", blank.id, "."))
    query.dsd<-c(query.dsd, paste(blank.id, "rdf:type qb:ComponentSpecification ."))
    query.dsd<-c(query.dsd, paste(blank.id, "qb:dimension" , id, "."))
    
    blank.counter<-blank.counter+1
    
    label<-paste("\"", dimension.list[[i]]@conceptRef, "\"", sep="")
    query.component<-c(query.component, paste(id, "rdfs:label", label, "."))
    
    if(!is.na(dimension.list[[i]]@codelist)){
      
      query.component<-c(query.component, paste(id, "rdf:type qb:CodedProperty ."))
      
      code.list<-paste("<", dimension.list[[i]]@codelist, ">", sep="")
      query.component<-c(query.component, paste(id, "qb:codeList", code.list, "."))
    }
  }

  #Set RDF statements for attribute components
  
  for (i in 1:length(attribute.list)){
    
    id<-paste("<", attribute.list[[i]]@conceptRef, ">", sep="")
    query.component<-c(query.component, paste(id, "rdf:type qb:AttributeProperty ."))

    blank.id<-Blank_ID(blank.counter)
    
    query.dsd<-c(query.dsd, paste(dsd.id, "qb:component", blank.id, "."))
    query.dsd<-c(query.dsd, paste(blank.id, "rdf:type qb:ComponentSpecification ."))
    query.dsd<-c(query.dsd, paste(blank.id, "qb:attribute" , id, "."))
    
    blank.counter<-blank.counter+1
    
    label<-paste("\"", attribute.list[[i]]@conceptRef, "\"", sep="")
    query.component<-c(query.component, paste(id, "rdfs:label", label, "."))
    
    if(!is.na(attribute.list[[i]]@codelist)){
      
      query.component<-c(query.component, paste(id, "rdf:type qb:CodedProperty ."))
      
      code.list<-paste("<", attribute.list[[i]]@codelist, ">", sep="")
      query.component<-c(query.component, paste(id, "qb:codeList", code.list, "."))
    }
  }

  #Set RDF statements for time dimension component
  
  id<-paste("<", time.dimension@conceptRef, ">", sep="")
  query.component<-c(query.component, paste(id, "rdf:type qb:DimensionProperty ."))

  blank.id<-Blank_ID(blank.counter)
  
  query.dsd<-c(query.dsd, paste(dsd.id, "qb:component", blank.id, "."))
  query.dsd<-c(query.dsd, paste(blank.id, "rdf:type qb:ComponentSpecification ."))
  query.dsd<-c(query.dsd, paste(blank.id, "qb:dimension" , id, "."))
  
  blank.counter<-blank.counter+1
  
  label<-paste("\"", time.dimension@conceptRef, "\"", sep="")
  query.component<-c(query.component, paste(id, "rdfs:label", label, "."))
  
  if(!is.na(time.dimension@codelist)){
    
    query.component<-c(query.component, paste(id, "rdf:type qb:CodedProperty ."))
    
    code.list<-paste("<", time.dimension@codelist, ">", sep="")
    query.component<-c(query.component, paste(id, "qb:codeList", code.list, "."))
  }

  #Set RDF statements for primary measure components
  
  id<-paste("<", primary.measure@conceptRef, ">", sep="")
  query.component<-c(query.component, paste(id, "rdf:type qb:MeasureProperty ."))

  blank.id<-Blank_ID(blank.counter)
  
  query.dsd<-c(query.dsd, paste(dsd.id, "qb:component", blank.id, "."))
  query.dsd<-c(query.dsd, paste(blank.id, "rdf:type qb:ComponentSpecification ."))
  query.dsd<-c(query.dsd, paste(blank.id, "qb:measure" , id, "."))
  
  blank.counter<-blank.counter+1
  
  label<-paste("\"", primary.measure@conceptRef, "\"", sep="")
  query.component<-c(query.component, paste(id, "rdfs:label", label, "."))
  
  if(!is.na(primary.measure@codelist)){
    
    query.component<-c(query.component, paste(id, "rdf:type qb:CodedProperty ."))
    
    code.list<-paste("<", primary.measure@codelist, ">", sep="")
    query.component<-c(query.component, paste(id, "qb:codeList", code.list, "."))
  }
    
  insert.output<-large.query.handler(SPARQL.endpoint, query.dsd, query.builder)
  insert.output<-large.query.handler(SPARQL.endpoint, query.component, query.builder)
  
}

# CodeList_To_RDF: inserts an SDMX codelist into a triple store as an RDF SKOS ConceptScheme.   
# returns: NA
# @SPARQL.endpoint: the SPARQL endpoint URL of the target triple store
# @code.lists: a list of SDMX codelist objects

CodeList_To_RDF<-function(SPARQL.endpoint, code.lists){
  
  for (i in 1:length(code.lists)){
    
    query.scheme<-NULL
    query.concept<-NULL
    
    #Set concept scheme RDF statements
    
    scheme.name<-code.lists[[i]]@id
    print(paste("\t Codelist", scheme.name))
    
    scheme.id<-paste("<", code.lists[[i]]@id, ">", sep="")
    query.scheme<-c(query.scheme, paste(scheme.id, "rdf:type skos:ConceptScheme ."))
    
    scheme.label<-paste("\"", code.lists[[i]]@Name[["en"]], "\"", sep="")
    query.scheme<-c(query.scheme, paste(scheme.id, "skos:prefLabel", scheme.label, "."))
    
    codes<-code.lists[[i]]@Code
    
    #Set RDF statements for each individual code
    
    for(j in 1:length(codes)){
      
      code.id<-paste("<", scheme.name, "_", codes[[j]]@id, ">", sep="")
      query.concept<-c(query.concept, paste(code.id, "rdf:type skos:Concept ."))
      query.concept<-c(query.concept, paste(code.id, "skos:inScheme", scheme.id, "."))
      query.scheme<-c(query.scheme, paste(scheme.id, "skos:hasTopConcept", code.id, "."))
      
      code.notation<-paste("\"", codes[[j]]@id, "\"", sep="")
      query.concept<-c(query.concept, paste(code.id, "skos:notation", code.notation, "."))
      
      code.label<-paste("\"", codes[[j]]@label[["en"]], "\"", sep="")
      query.concept<-c(query.concept, paste(code.id, "skos:prefLabel", code.label, "."))
      
    }
    
    #Insert RDF statements about the codes and about the concept scheme
    
    insert.output<-large.query.handler(SPARQL.endpoint, query.scheme, query.builder)
    insert.output<-large.query.handler(SPARQL.endpoint, query.concept, query.builder)
    
  }
  
}

SPARQL.endpoint<-"https://dydra.com/luca-gramaglia/eu-us-partnership-metadata-store/sparql"

#Declare dataset name

dataset.name<-"nama_10r_2gdp"

#Retrieve dataflow and DSD

dataflow <- readSDMX(providerId = "ESTAT", agencyId="ESTAT", resource = "dataflow", resourceId=dataset.name)@dataflows[[1]]

dsd.name<- dataflow@dsdRef
dsd <- readSDMX(providerId = "ESTAT", agencyId="ESTAT", resource = "datastructure", resourceId=dsd.name)
data.structure<-dsd@datastructures@datastructures[[1]]

code.lists<-dsd@codelists@codelists

print("Handling Dataflow...")
Dataflow_To_RDF(SPARQL.endpoint, dataflow)

print("Handling DSD...")
DSD_To_RDF(SPARQL.endpoint, data.structure)

print("Handling Codelists...")
CodeList_To_RDF(SPARQL.endpoint, code.lists)

