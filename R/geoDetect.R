#' detect, extract, and merge shapefiles
#' 
#' @param data  data.table object from getRel()
#' @keywords internal
#' @return Shapefile of relevant geographic units
#' @import rgdal maptools
#' @export 
#' 
#' 

#EXAMPLE -- shp <- geoDetect(statenuts2)

geoDetect<- function(data){

  requireNamespace('rgdal', quietly = TRUE)
  requireNamespace('maptools', quietly = TRUE)
  
  #localPath <- paste0(.libPaths()[1], '/eu.us.opendata/rawdata')
  localPath <- "/Users/sigmamonstr/Github/project-eu-us/rawdata/shp"

  #Pull geo levels from unified dataset
    meta = as.data.frame(attr(data,"Description"))
    meta = meta[,grep("*_Geo",colnames(meta))]
  
  #Go through each geo levels
  for(k in 1:ncol(meta)){
    if(colnames(meta)[k]=="BEA_Geo"){
          us_shp <- switch( meta[1,k],
                            County = readOGR(dsn =localPath, layer = "USCounty.shp"),
                            State =  readOGR(dsn =localPath, layer = "USState.shp"),
                            MSA = readOGR(dsn =localPath, layer = "USMSA.shp"))
          
    } else if(colnames(meta)[k]=="EU_Geo"){
          eu_shp <- switch(substr(meta[1,k],1,5),
                        NUTS1 = readOGR(dsn =localPath, layer = "NUTS1.shp"),
                        NUTS2 = readOGR(dsn =localPath, layer = "NUTS2.shp"),
                        NUTS3 = readOGR(dsn =localPath, layer = "NUTS3.shp"),
                        Metro = )
    }
    
  }
  #Merge and return
    us_shp <- spChFIDs(us_shp, paste("us_shp", row.names(us_shp), sep="."))
    temp <- spRbind(us_shp,eu_shp)
    return(temp)
  
}


