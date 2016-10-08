

	requireNamespace('SPARQL', quietly = TRUE)
	requireNamespace('data.tree', quietly = TRUE)

	SPARQL.endpoint <- "https://dydra.com/luca-gramaglia/eu-us-partnership-metadata-store/sparql"
	
	dataset.file <- paste0(.libPaths()[1], "/euroStates/rawdata/Dataset_Template.csv")
	DSD.file <- paste0(.libPaths()[1], "/euroStates/rawdata/DSD_Template.csv")
	
	dataset <- read.csv(dataset.file)
	DSD <- read.csv(DSD.file)
	
	#
	#Check integrity of the files
	#
	
	#Check that there are no duplicate Dataset IDs and that all mandatory elements are present
	if(nrow(dataset[!duplicated(dataset[, "Dataset.ID"]), ]) != nrow(dataset)) stop("Duplicate dataset IDs!")
	if(nrow(dataset[complete.cases(dataset),]) != nrow(dataset)) stop("Missing mandatory values in dataset descriptions!")
	
	#Check that there are no duplicate DSD - Component combinations, 
	#that all mandatory elements are present
	#and that a given DSD ID implies a given DSD label
	if(nrow(DSD[!duplicated(DSD[, c("DSD.ID", "Component.ID")]), ]) != nrow(DSD)) stop("Duplicate DSD ID - Component ID combinations!")
	if(nrow(DSD[complete.cases(subset(DSD, select=-c(Codelist.ID))),]) != nrow(DSD)) stop("Missing mandatory values in DSD descriptions!")
	if(!all(duplicated(DSD[, c("DSD.ID")]) == duplicated(DSD[, c("DSD.label")]))) stop("Different DSD labels for same DSD ID!")
	
	#Check that the DSD IDs in the dataset file match the DSD IDs in the DSD file and viceversa
	if(nrow(merge(dataset, DSD, "DSD.ID", all=TRUE)) != nrow(merge(dataset, DSD, "DSD.ID"))) stop("Mismatches between datasets and DSDs available.")
	
	#
	#Prepare dataset RDF statements
	#
	
	dataset.query.list <- NULL
	
	for (i in 1:nrow(dataset)) {
	  
	  dataset.specs  <-  list(
	    'id' = dataset[i, "Dataset.ID"] ,
	    'title' = dataset[i, "Dataset.title"],
	    'description' = dataset[i, "Dataset.description"],
	    'publisher' = dataset[i, "Dataset.publisher"],
	    'structure' = dataset[i, "DSD.ID"]
	  )
	  
	  dataset.query.list <- c(dataset.query.list, Create_RDF_Dataset(dataset.specs))
	  
	}
	
	
	#
	#Prepare DSD RDF statements
	#
	
	DSD$pathString <- paste("All", DSD$DSD.ID, DSD$Component.ID, sep="/")
	DSD.tree <- data.tree::FromDataFrameTable(DSD, colLevels = list(NULL, "DSD.label", c("Component.label", "Component.role", "Coded", "Codelist.ID")))
	
	dsd.query.list <- Create_RDF_DSD(DSD.tree)
	
	#
	#Execute queries
	#
	
	insert.output <- large.query.handler(SPARQL.endpoint, dataset.query.list, query.builder)
	insert.output <- large.query.handler(SPARQL.endpoint, dsd.query.list, query.builder)
	
	
	
	
	
	
	
	
	
