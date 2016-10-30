#' create interactive map of dataset for a given year
#' 
#' @param data  data.table object from getRel()
#' @param year selected year for map
#' @param asSHP controls output as a leaflet map or a merged SHP
#' @keywords internal
#' @return Shapefile of relevant geographic units
#' @import rgdal maptools leaflet
#' @export 
#' 
#' 

#EXAMPLE -- shp <- geoDetect(statenuts2)
geoMap <- function(dataset, year, asSHP = FALSE){
  
  requireNamespace('leaflet', quietly = TRUE)
  
  #Get shapefile
    shp<-geoDetect(dataset)
    
  #Subset data
    one_year <- dataset[dataset$TIME==year,,]
    
  #Prep data for merge
    shp@data$GEO <- as.character(shp@data$GEO)
    shp@data$GEO_NAME <- NULL
    one_year$GEO <- as.character(one_year$GEO)
    one_year$GEO_NAME <- as.character(one_year$GEO_NAME)
    one_year$GEO_NAME[is.na(one_year$GEO_NAME)] <- one_year$GEO[is.na(one_year$GEO_NAME)]
    one_year$GEO[one_year$SOURCE=="bea"] <- substr(one_year$GEO,1,2)[one_year$SOURCE=="bea"]
    
  #Merge Records
    test <- merge(shp,one_year,by="GEO")
    
  #Set up scaled values
    test$LOG_VALUE <- log(test$OBS_VALUE)
    test$LOG_VALUE[is.na(test$LOG_VALUE)] <- 0
    test <- test[!is.na(test$OBS_VALUE),]
    
    meta = as.data.frame(attr(dataset,"Description"))
    test$UNIT <- meta$BEA_Unit
    test$UNIT[test$SOURCE=="eurostat"]<- meta$EU_Unit
  
  ##IF
    if(!asSHP){
      #Popup
      content_all <- paste("<h2>",test$GEO_NAME,"<br>",
                           "</h2>Observed Value in ",test$UNIT,":",test$OBS_VALUE
                           )
      
      #Color ramp
      qpal <- colorQuantile("Blues", test$LOG_VALUE, n = 30)
      
      #Map
      leaflet(test) %>% 
        setView(lat = 34.452218, lng = -38.232422,2) %>%
        addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
                 attribution = "BEA + Eurostat") %>%
        addPolygons(
          stroke = FALSE, fillOpacity = 0.8, smoothFactor = 0.5,
          color = ~qpal(test$LOG_VALUE), popup = content_all
        )
    } else {
      return(test)
    }
}
