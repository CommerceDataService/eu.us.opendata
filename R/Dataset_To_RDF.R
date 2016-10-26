source("RDF_query_helper_functions.R", chdir=T)

Dataset_To_RDF<-function(DatasetSpecs){
  
  query.list<-NULL
  
  if(!is.null(DatasetSpecs$Dataset.ID)){
    id<- paste("<", DatasetSpecs$Dataset.ID, ">", sep="")
    query.list<-c(query.list, paste(id, "rdf:type qb:DataStructureDefinition, dcat:Dataset ."))
  }
  
  if(!is.null(DatasetSpecs$catalog)){
    catalog.id<- DatasetSpecs$catalog
    query.list<-c(query.list, paste(catalog.id, "dcat:dataset", id))
  }
  
  if(!is.null(DatasetSpecs$title)){
    title<-paste("\"", DatasetSpecs$title, "\"", sep="")
    query.list<-c(query.list, paste(id, "dct:title", title, "."))
  }
  
  if(!is.null(DatasetSpecs$description)){
    description<-paste("\"", DatasetSpecs$description, "\"", sep="")
    query.list<-c(query.list, paste(id, "dct:description", description, "."))
  }
  
  if(!is.null(DatasetSpecs$publisher)){
    publisher<-paste("<", DatasetSpecs$publisher, ">", sep="")
    query.list<-c(query.list, paste(id, "dct:publisher", publisher, "."))
  }
  
  if(!is.null(DatasetSpecs$accrualPeriodicity)){
    accrualPeriodicity<-paste("\"", DatasetSpecs$accrualPeriodicity, "\"", sep="")
    query.list<-c(query.list, paste(id, "dct:accrualPeriodicity", accrualPeriodicity, "."))
  }
  
  if(!is.null(DatasetSpecs$spatial)){
    spatial<-paste("\"", DatasetSpecs$spatial, "\"", sep="")
    query.list<-c(query.list, paste(id, "dct:spatial", spatial, "."))
  }
  
  if(!is.null(DatasetSpecs$temporal)){
    temporal<-paste("\"", DatasetSpecs$temporal, "\"", sep="")
    query.list<-c(query.list, paste(id, "dct:temporal", temporal, "."))
  }
  
  if(!is.null(DatasetSpecs$statMeasure)){
    statMeasure<-paste("\"", DatasetSpecs$statMeasure, "\"", sep="")
    query.list<-c(query.list, paste(id, "stat:statMeasure", statMeasure, "."))
  }
  
  if(!is.null(DatasetSpecs$distribution)){
    
    for(i in 1:length(DatasetSpecs$distribution)){
      
      distribution.id<-paste("<", DatasetSpecs$Dataset.ID, "#DISTRO", i, ">", sep="")
      query.list<-c(query.list, paste(distribution.id, "rdf:type qb:DataSet, dcat:Distribution ."))
      query.list<-c(query.list, paste(id, "dcat:distribution", distribution.id, "."))
      query.list<-c(query.list, paste(distribution.id, "qb:structure", id, "."))
      
      for (j in 1:length(DatasetSpecs$distribution[[i]]$accessURL)){
        
        accessURL<-paste("\"", DatasetSpecs$distribution[[i]]$accessURL[j], "\"", sep="")
        query.list<-c(query.list, paste(distribution.id, "dcat:accessURL", accessURL, "."))
        
      }
      
    }
  }
  
  if(!is.null(DatasetSpecs$component)){
    
    for(i in 1:length(DatasetSpecs$component)){
      
      component.specification.id<- paste("<", DatasetSpecs$Dataset.ID, "#COMP", i, ">", sep="")
      query.list<-c(query.list, paste(component.specification.id, "rdf:type qb:ComponentSpecification ."))
      query.list<-c(query.list, paste(id, "qb:component", component.specification.id, "."))
      
      if(DatasetSpecs$component[[i]]$type=="component"){
        component.property<-"qb:componentProperty"
        component.property.class<-"qb:ComponentProperty"
      }
      else if (DatasetSpecs$component[[i]]$type=="dimension"){
        component.property<-"qb:dimension"
        component.property.class<-"qb:DimensionProperty"
      }
      else if (DatasetSpecs$component[[i]]$type=="attribute"){
        component.property<-"qb:attribute"
        component.property.class<-"qb:AttributeProperty"
      }
      else if (DatasetSpecs$component[[i]]$type=="measure"){
        component.property<-"qb:measure"
        component.property.class<-"qb:MeasureProperty"
      }
      else stop("Component type not recognised")
      
      component.property.id<- paste("<", DatasetSpecs$Dataset.ID, "#COMP", i, "#", DatasetSpecs$component[[i]]$id, ">", sep="")
      query.list<-c(query.list, paste(component.property.id, "rdf:type",  component.property.class ,"."))
      query.list<-c(query.list, paste(component.specification.id, component.property, component.property.id, "."))
      
      if(!is.null(DatasetSpecs$component[[i]]$order)){
        order<-DatasetSpecs$component[[i]]$order
        query.list<-c(query.list, paste(component.specification.id, "qb:order", order, "."))
      }
      
      if(!is.null(DatasetSpecs$component[[i]]$componentRequired)){
        componentRequired<-paste("\"", DatasetSpecs$component[[i]]$componentRequired, "\"", sep="")
        query.list<-c(query.list, paste(component.specification.id, "qb:componentRequired", componentRequired, "."))
      }
      
      if(!is.null(DatasetSpecs$component[[i]]$label)){
        label<-paste("\"", DatasetSpecs$component[[i]]$label, "\"", sep="")
        query.list<-c(query.list, paste(component.property.id, "rdfs:label", label, "."))
      }
      
      if(!is.null(DatasetSpecs$component[[i]]$codeList)){
        query.list<-c(query.list, paste(component.property.id, "rdf:type qb:CodedProperty ."))
        codeList<-paste("<", DatasetSpecs$Dataset.ID, "#CODELIST#", DatasetSpecs$component[[i]]$codeList, ">", sep="")
        query.list<-c(query.list, paste(component.property.id, "qb:codeList", codeList, "."))
      }
      
    }
  }
  
  if(!is.null(DatasetSpecs$codelist)){
    
    for(i in 1:length(DatasetSpecs$codelist)){
      
      codelist.id<-paste("<", DatasetSpecs$Dataset.ID, "#CODELIST#", DatasetSpecs$codelist[[i]]$id, ">", sep="")
      query.list<-c(query.list, paste(codelist.id, "rdf:type skos:ConceptScheme ."))
      
      if(!is.null(DatasetSpecs$codelist[[i]]$label)){
        label<-paste("\"", DatasetSpecs$codelist[[i]]$label, "\"", sep="")
        query.list<-c(query.list, paste(codelist.id, "skos:prefLabel", label, "."))
      }
      
      for(j in 1:length(DatasetSpecs$codelist[[i]]$code)){
        
        code.id<-paste("<", DatasetSpecs$Dataset.ID, "#CODELIST#", DatasetSpecs$codelist[[i]]$id, "#", DatasetSpecs$codelist[[i]]$code[[j]]$id, ">", sep="")
        query.list<-c(query.list, paste(code.id, "rdf:type skos:Concept ."))
        query.list<-c(query.list, paste(code.id, "skos:inScheme", codelist.id , "."))
        query.list<-c(query.list, paste(codelist.id, "skos:hasTopConcept", code.id , "."))

        if(!is.null(DatasetSpecs$codelist[[i]]$code[[j]]$label)){
          label<-paste("\"", DatasetSpecs$codelist[[i]]$code[[j]]$label, "\"", sep="")
          query.list<-c(query.list, paste(code.id, "skos:prefLabel", label, "."))
        } 
        
        if(!is.null(DatasetSpecs$codelist[[i]]$code[[j]]$notation)){
          notation<-paste("\"", DatasetSpecs$codelist[[i]]$code[[j]]$notation, "\"", sep="")
          query.list<-c(query.list, paste(code.id, "skos:notation", notation, "."))
        } 
        
      }
      
    }
  }
  
  return(query.list)
  
}

Dataset_Insert<-function(DatasetSpecs.list, SPARQL.endpoint) {
  
  Dataset.query.list<-unlist(lapply(DatasetSpecs.list, Dataset_To_RDF))
  
  large.query.handler(SPARQL.endpoint, Dataset.query.list, insert.query.builder)
  
  insert.catalog.last.update(as.POSIXlt(Sys.time(), "GMT"), SPARQL.endpoint)
}