#' Reshape datasets into wide form
#' Auto detect if just GEO x TIME or GEO x IND x TIME
#' 
#' @keywords internal
#' @param df 	 data.frame to be reshaped to wide format 
#' @importFrom stats reshape 
#' @export 

wide <- function(df){
  
  if(!"IND" %in% colnames(df)){
    print("Converting from long to wide GEO")
    input <- df[,c("GEO_NAME","TIME","OBS_VALUE"),with=FALSE]
    out <- reshape(input, 
                   timevar = "GEO_NAME",
                   idvar = c("TIME"),
                   direction = "wide")
    out$TIME <- as.numeric(as.character(out$TIME))
    return(out)
  } else if(sum(c("GEO","IND") %in% colnames(df))==2) {
    print("Converting from long to wide GEO x INDUSTRY")
    input <- df[,c("GEO_NAME","TIME","IND","OBS_VALUE"),with=FALSE]
    out <- reshape(input, 
                   timevar = c("GEO_NAME","IND"),
                   idvar = c("TIME"),
                   direction = "wide")
    out$TIME <-  as.numeric(as.character(out$TIME))
    return(out)
  }
}

