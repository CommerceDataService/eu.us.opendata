
attr(dataset,"Description")

timeSync <- function(dataset, sync){
  meta = as.data.frame(attr(dataset,"Description"))
  meta = meta[,grep("*_Period",colnames(meta))]
  temp = data.frame()
  for(k in 1:length(colnames(meta))){
    t = as.numeric(trimws(unlist(strsplit(meta[1,k],"-"))))
    temp <- rbind(temp, data.frame(start = t[1],end=t[2]))
  } 
  
  if(sync ==1){
    #latest
    dataset$TIME <- as.numeric(as.character(dataset$TIME))
    dataset <- dataset[dataset$TIME==min(temp$end),,]
    return(dataset)
  }else if(sync==2){
    #overlap
    dataset$TIME <- as.numeric(as.character(dataset$TIME))
    dataset <- dataset[which(dataset$TIME<=min(temp$end) & dataset$TIME>=max(temp$start)),,]
    return(dataset)
    
  } else if(sync != 1 && sync != 2){
    warning("Input correct sync option: 1 = latest period, 2 = overlapping period")
  }
}
