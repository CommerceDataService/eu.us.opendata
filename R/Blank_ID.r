#' Utility function to create a blank node ID
#'
#' @param blank.counter		counter distinguishing the blank nodes
#' @return a blank node ID

Blank_ID <- function(blank.counter){
  
  return(paste("_:b", blank.counter, sep=""))
  
}

