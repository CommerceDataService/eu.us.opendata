#' detect, extract, and merge shapefiles
#' 
#' @param data  data.table object from getRel()
#' @keywords internal
#' @return Shapefile of relevant geographic units
#' @import rgdal maptools sp
#' @export 
#' 
#' 

#EXAMPLE -- shp <- geoDetect(statenuts2)

geoDetect<- function(data){

  requireNamespace('rgdal', quietly = TRUE)
  requireNamespace('maptools', quietly = TRUE)
  requireNamespace('sp', quietly = TRUE)

  localPath <- paste0(.libPaths()[1], '/eu.us.opendata/extdata/shp')
 
  #Pull geo levels from unified dataset and .sysdata shapefiles
    meta = as.data.frame(attr(data,"Description"))
    print(paste("Dataset: ", meta$Rel_name[1]))
    meta = meta[,grep("*_Geo",colnames(meta))]
  for(k in 1:ncol(meta)){
    if(colnames(meta)[k]=="BEA_Geo"){
          us_shp <- switch( meta[1,k],
                            County = eu.us.od.County,
                            State = eu.us.od.State,
                            MSA = eu.us.od.MSA
                       		)
          print(paste("US Geography: ",meta[1,k]))
          
    } else if(colnames(meta)[k]=="EU_Geo"){
          eu_shp <- switch(substr(meta[1,k],1,5),
                        NUTS1 = eu.us.od.NUTS1,
                        NUTS2 = eu.us.od.NUTS2,
                        NUTS3 = eu.us.od.NUTS3,
                        Metro = eu.us.od.Metro 
                       )
          print(paste("EU Geography: ",meta[1,k]))
    }
    
  }

  #Merge and return
    us_shp <- sp::spChFIDs(us_shp, paste("us_shp", row.names(us_shp), sep="."))
    temp <- maptools::spRbind(us_shp,eu_shp)
    return(temp)
    
  
}


