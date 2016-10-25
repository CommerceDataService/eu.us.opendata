#' Given a relationship ID, return description of mapped relationship allowing users to see common overlap and/or differences between datasets
#' 
#' @param term 	 ID of the statistic requested 
#' @import data.table formattable
#' @export 

describeRel <- function(term) { 
	requireNamespace(data.table)
	requireNamespace(formattable)

	eu.us.openR::updateCache();
	
  localRel <- as.data.frame(loadLocalRel())
  loc = localRel[localRel$Rel_ID==term,]
  
  if(nrow(loc)>0){
  temp = data.frame(field = c("EU US Rel_ID","Description","freq", "period","unit","geo", "url"),
                 EU_data = c(loc$Rel_ID,loc$Rel_name,loc$Freq,loc$EU_Period,loc$EU_Unit,loc$EU_Geo,loc$EU_ID),
                 US_data = c("","",loc$Freq,loc$BEA_Period,loc$BEA_Unit,loc$BEA_Geo,loc$BEA_ID))
  colnames(a) <- c("Field", "EU Data","US Data")
  formattable(a, align="l")
  } else{
    print("No result")
  }
}

#Example
#describeRel("JOINT#GDP_A_2") #matched case
#describeRel("gdp") #error case
