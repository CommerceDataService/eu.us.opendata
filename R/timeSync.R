#' Provides user easier access to extracts the latest overlapping year of data or all overlapping years.
#' 
#' @param dataset 	data.frame or data.table 
#' @param sync 	Numeric value representing period, with options: 1 = latest period, 2 = overlapping period
#' @export 


timeSync <- function(dataset, sync){
#attr(dataset,"Description")
  meta0 = attr(dataset,"Description")
  meta1 = as.data.frame(meta0)
  meta = meta1[,grep("*_Period",colnames(meta1))]
  temp = data.frame()
  for(k in 1:length(colnames(meta))){
    t = as.numeric(trimws(unlist(strsplit(meta[1,k],"-"))))
    temp <- rbind(temp, data.frame(start = t[1],end=t[2]))
  } 
  
  if(sync ==1){
    #latest
    dataset$TIME <- as.numeric(as.character(dataset$TIME))
    dataset <- dataset[dataset$TIME==min(temp$end),,]
    attr(dataset,"Description") <- meta0
    return(dataset)
  }else if(sync==2){
    #overlap
    dataset$TIME <- as.numeric(as.character(dataset$TIME))
    dataset <- dataset[which(dataset$TIME<=min(temp$end) & dataset$TIME>=max(temp$start)),,]
    attr(dataset,"Description") <- meta0
    return(dataset)
    
  } else if(sync != 1 && sync != 2){
    warning("Input correct sync option: 1 = latest period, 2 = overlapping period")
  }
}
