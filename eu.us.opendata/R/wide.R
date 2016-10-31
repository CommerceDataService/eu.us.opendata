#' Reshape datasets into wide form
#' Auto detect if just GEO x TIME or GEO x IND x TIME
#' 
#' @keywords internal
#' @param df 	 data.frame to be reshaped to wide format 
#' @param timeWide 	 data.frame to be reshaped to wide format 
#' @importFrom stats reshape 
#' @export 

wide <- function(df, timeWide=FALSE){
  if(timeWide==FALSE){
    if(!"IND" %in% colnames(df)){
      warning("Converting from long to wide GEO")
      input <- df[,c("GEO","TIME","OBS_VALUE"),with=FALSE]
      out <- reshape(input, 
                     timevar = "GEO",
                     idvar = c("TIME"),
                     direction = "wide")
      out$TIME <- as.numeric(as.character(out$TIME))
      return(out)
    } else if(sum(c("GEO","IND") %in% colnames(df))==2) {
      warning("Converting from long to wide GEO x INDUSTRY")
      input <- df[,c("GEO","TIME","IND","OBS_VALUE"),with=FALSE]
      out <- reshape(input, 
                     timevar = c("GEO","IND"),
                     idvar = c("TIME"),
                     direction = "wide")
      out$TIME <-  as.numeric(as.character(out$TIME))
      return(out)
    }
  } else if(timeWide==TRUE){
    if(!"IND" %in% colnames(df)){
      warning("Converting from long to wide TIME")
      input <- df[,c("GEO","GEO_NAME","TIME","OBS_VALUE", "SOURCE"),with=FALSE]
      input$GEO_NAME[is.na(input$GEO_NAME)]<-""
      out <- reshape(input, 
                     timevar = "TIME",
                     idvar = c("GEO","GEO_NAME", "SOURCE"),
                     direction = "wide")
      
      return(out)
    } else if(sum(c("GEO","IND") %in% colnames(df))==2) {
      warning("Converting from long to wide TIME")
      input <- df[,c("GEO","GEO_NAME","TIME","IND","OBS_VALUE", "SOURCE"),with=FALSE]
      input$GEO_NAME[is.na(input$GEO_NAME)]<-""
      out <- reshape(input, 
                     timevar = c("TIME"),
                     idvar = c("GEO","GEO_NAME","IND", "SOURCE"),
                     direction = "wide")
      
      return(out)
    }
  }
}

