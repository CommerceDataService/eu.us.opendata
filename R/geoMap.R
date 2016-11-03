#' create interactive map of dataset for a given year
#' 
#' @param data  data.table object from getRel()
#' @param year selected year for map, "all" returns a shapefile with all years of data in wide form
#' @param asSHP controls output as a leaflet map or a merged SHP
#' @keywords internal
#' @return Shapefile of relevant geographic units
#' @import  leaflet
#' @export 
#' 
#' 

#EXAMPLE -- shp <- geoDetect(statenuts2)
geoMap <- function(dataset, year, asSHP = FALSE, tile_provider = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
                   fillOpacity = 0.8, colorPal="Blues", colorSteps=30){
  
  requireNamespace('leaflet', quietly = TRUE)
  
  #Initial check -- if YES, then progress to process
  if(tolower(year) == "all" || year %in% unique(dataset$TIME)){
    
    #Subset data
    if(year == "all"){
      one_year <- wide(dataset, timeWide=TRUE)
      
    } else if(is.numeric(year) && 2014 %in% unique(dataset$TIME)){
      one_year <- dataset[dataset$TIME==year,,]
    }
    
    #Get shapefile
      print("Identifying geographic levels")
#	Shapefiles read with rgdal from shp are now in .sysdata
      shp<-geoDetect(dataset)

#TESTING: Temporarily ignore MSA/Metro-level for mapping
   if(class(shp) == 'character') {
   	return(shp)
   } else {
    #Prep data for merge
      shp@data$GEO <- as.character(shp@data$GEO)
      shp@data$GEO_NAME <- NULL
      one_year$GEO <- as.character(one_year$GEO)
      one_year$GEO_NAME <- as.character(one_year$GEO_NAME)
      one_year$GEO_NAME[is.na(one_year$GEO_NAME) | one_year$GEO_NAME==""] <- one_year$GEO[is.na(one_year$GEO_NAME) | one_year$GEO_NAME==""]
      
    #Adjustments to names 
      if(attr(dataset,"Description")$BEA_Geo=="State"){
        one_year$GEO[one_year$SOURCE=="bea"] <- substr(one_year$GEO,1,2)[one_year$SOURCE=="bea"]
      }
     
    #Merge Records
      test <- merge(shp,one_year,by="GEO")
     
    ##IF FOR LEAFLET
      if(!asSHP && year !="all"){
        
        #Scale values
          test@data$LOG_VALUE <- log(test@data$OBS_VALUE)
          test@data$LOG_VALUE[is.na(test@data$LOG_VALUE)] <- 0
          test <- test[!is.na(test@data$OBS_VALUE),]
          meta = as.data.frame(attr(dataset,"Description"))
          test@data$UNIT <- meta$BEA_Unit
          test@data$UNIT[test@data$SOURCE=="eurostat"]<- meta$EU_Unit
        
        #Popup
        content_all <- paste("<h2>",test$GEO_NAME,"</h2><p><strong>",meta$Rel_name[1],"</strong><br>",
                             "<u>Observed Value (",test$UNIT,"</u>):",test$OBS_VALUE,"</p>")
        
        #Color ramp
        qpal <- colorQuantile(colorPal, test$LOG_VALUE, n = colorSteps)
        
        #Map
        leaflet(test) %>% 
          setView(lat = 34.452218, lng = -38.232422,2) %>%
          addTiles(tile_provider,
                   attribution = "BEA + Eurostat") %>%
          addPolygons(
            stroke = FALSE, fillOpacity = fillOpacity, smoothFactor = 0.5,
            color = ~qpal(test$LOG_VALUE), popup = content_all
          )
      } else {
        return(test)
      }
   }
  } else {
    warning("Period selected is not available")
  }
}
