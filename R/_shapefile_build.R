library(rgdal)
library(rgeos)
library(maptools)

##EU GEO
  setwd("/Users/sigmamonstr/Google Drive/DOC/055-Project-EU-US/geo/NUTS_2013_10M_SH/data")
  
  #Reading
    shape <- readOGR(dsn = ".", layer = "NUTS_RG_10M_2013")
    shape@data$GEO_NAME <- shape@data$NUTS_ID
    shape@data <- shape@data[,c(1,5,2,3)]
    colnames(shape@data)<-c("GEO","GEO_NAME","LEVEL","SHAPE_AREA")
    shape@data$GEO <- as.character(shape@data$GEO)
    shape@data$GEO_NAME <- as.character(shape@data$GEO_NAME)
    shape@data$LEVEL <- as.character(shape@data$LEVEL)
    shape@data$ID <- paste0("eu_",1:nrow(shape@data))
    
    NUTS1 <- shape[shape@data$LEVEL==1,]
    NUTS2 <- shape[shape@data$LEVEL==2,]
    NUTS3 <- shape[shape@data$LEVEL==3,]
    
    NUTS1 <- spTransform(NUTS1, CRS("+init=epsg:4326"))
    NUTS2 <- spTransform(NUTS2, CRS("+init=epsg:4326"))
    NUTS3 <- spTransform(NUTS3, CRS("+init=epsg:4326"))
    
    #Metro formation
    metro_code <- read.csv("/Users/sigmamonstr/Google Drive/DOC/055-Project-EU-US/geo/Metropolitan-region-typology-NUTS-2013-1.csv")
    metro <- merge(NUTS3,metro_code,by.x = "GEO", by.y="NUTS3")
    metro <- subset(metro,!is.na(metro@data$Metro))
    
   df_new <-  data.frame()
   poly_new <- subset(metro,is.na(metro@data$Metro))
   metro@data$Metro <- as.character(metro@data$Metro)
    for(k in unique(metro@data$Metro)){
      temp <- subset(metro,metro@data$Metro==k)
      df_new <- data.frame(GEO = k,
                                 GEO_NAME = temp@data$Metro_name[1],
                                 LEVEL = NA,
                                 SHAPE_AREA = sum(temp@data$SHAPE_AREA),
                                 ID = k)
      temp_shp <- unionSpatialPolygons(temp, temp@data$Metro==k)
      row.names(temp_shp)<-k
      rownames(df_new) <- k
      temp = SpatialPolygonsDataFrame(Sr=temp_shp, data=df_new,FALSE)
      poly_new <- spRbind(poly_new,temp)
      print(k)
    }
    
    metro <- poly_new
    
    setwd("/Users/sigmamonstr/Google Drive/DOC/055-Project-EU-US/geo/shapes")
    writeOGR(NUTS1, ".", "NUTS1", driver="ESRI Shapefile",overwrite_layer = TRUE)
    writeOGR(NUTS2, ".", "NUTS2", driver="ESRI Shapefile",overwrite_layer = TRUE)
    writeOGR(NUTS3, ".", "NUTS3", driver="ESRI Shapefile",overwrite_layer = TRUE)
    writeOGR(metro, ".", "NUTMetro", driver="ESRI Shapefile",overwrite_layer = TRUE)
  #

##US GEO
  
  setwd("/Users/sigmamonstr/Google Drive/DOC/055-Project-EU-US/geo/cb_2015_us_county_20m")
  county <- readOGR(dsn = ".", layer = "cb_2015_us_county_20m")
  county <- spTransform(county, CRS("+init=epsg:4326"))
  county@data$LEVEL <- "county"
  county@data$SHAPE_AREA <- county@data$ALAND + county@data$AWATER
  county@data$GEO <- as.character(county@data$GEOID)
  county@data$GEO_NAME <- as.character(county@data$NAME)
  county@data$ID <- paste0("us_",1:nrow(county@data))
  county@data <- county@data[,c("GEOID","NAME","LEVEL","SHAPE_AREA")]
  colnames(county@data) <- c("GEO","GEO_NAME","LEVEL","SHAPE_AREA")
  USCounty <- county
  setwd("/Users/sigmamonstr/Google Drive/DOC/055-Project-EU-US/geo/shapes")
  writeOGR(USCounty, ".", "USCounty", driver="ESRI Shapefile",overwrite_layer = TRUE)
  
  setwd("/Users/sigmamonstr/Google Drive/DOC/055-Project-EU-US/geo/cb_2015_us_state_20m")
  state <- readOGR(dsn = ".", layer = "cb_2015_us_state_20m")
  state <- spTransform(state, CRS("+init=epsg:4326"))
  state@data$LEVEL <- "state"
  state@data$SHAPE_AREA <- state@data$ALAND + state@data$AWATER
  state@data$GEO <- as.character(state@data$GEOID)
  state@data$GEO_NAME <- as.character(state@data$NAME)
  state@data <- state@data[,c("GEO","GEO_NAME","LEVEL","SHAPE_AREA")]
  state@data$ID <- paste0("us_",1:nrow(state@data))
  USState <- state
  setwd("/Users/sigmamonstr/Google Drive/DOC/055-Project-EU-US/geo/shapes")
  writeOGR(USState, ".", "USState", driver="ESRI Shapefile",overwrite_layer = TRUE)
  
  
  setwd("/Users/sigmamonstr/Google Drive/DOC/055-Project-EU-US/geo/cb_2015_us_cbsa_20m")
  msa <- readOGR(dsn = ".", layer = "cb_2015_us_cbsa_20m")
  msa <- spTransform(msa, CRS("+init=epsg:4326"))
  msa@data$LEVEL <- "msa"
  msa@data$SHAPE_AREA <- msa@data$ALAND + msa@data$AWATER
  msa@data$GEO <- as.character(msa@data$GEOID)
  msa@data$GEO_NAME <- as.character(msa@data$NAME)
  msa@data <- msa@data[,c("GEO","GEO_NAME","LEVEL","SHAPE_AREA")]
  msa@data$ID <- paste0("us_",1:nrow(msa@data))
  USMSA <- msa
  setwd("/Users/sigmamonstr/Google Drive/DOC/055-Project-EU-US/geo/shapes")
  writeOGR(USMSA, ".", "USMSA", driver="ESRI Shapefile",overwrite_layer = TRUE)
  
  
##MERGE
  library(maptools)
  
  NUTS1 <- spChFIDs(NUTS1, paste("NUTS1", row.names(NUTS1), sep="."))
  temp <- spRbind(state,NUTS1)

