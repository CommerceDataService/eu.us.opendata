#' Refines search query by spellchecking and standardizing acronyms in query string
#' @param rec is a character string with the terms to be searched
#' @import hunspell
#' @export 


searchRefine <- function(rec){
  
  ##Load master acronyms
  master <- eu.us.opendata::loadLocalAcronyms()
  
  ##Parse query
  combos <- searchParse(rec)
  
  ##Run all n-grams to match
  matches <- data.frame()
  for(k in combos){
    a <- master[agrep(k, master[,2],ignore.case=TRUE ),]
    if(nrow(a)>0){
      a$ngram <- k
      matches <- rbind(matches, a)
    }
  }
  
  ##Replace parts of query
  if(nrow(matches)>0){
    for(k in 1:nrow(matches)){
      rec <- gsub(matches$ngram[k], matches$acronym[k],rec)
    }
  }
  
  return(rec)
  
}

