#' Parses, spell checks, and prepares a search query for standardization
#' by creating n-grams
#' @param   Option to render results in an interactive DT
#' @param spellCheck  Is true to force spellcheck
#' @import hunspell
#' @export 


searchParse <- function(rec, spellCheck = TRUE){
  requireNamespace('hunspell', quietly = TRUE)
  
    splits <-  unlist(strsplit(rec," "))
    
    ##Spell check using hunspell
    if(spellCheck == TRUE){
      for(k in 1:length(splits)){
        if(length(unlist(hunspell_find(splits[k])))>0){
          splits[k] <- unlist(hunspell_suggest(splits[k]))[1]
        }
      }
    }
    
    ##2 to 4 n-grams
    term_test <- c()
    
    if(length(splits)== 1){
      
    } else if(length(splits)==2){
      term_test <- rec
      
    } else if(length(splits)>2){
      for(k in 3:length(splits)){

        term_test <- c(term_test, paste(splits[k-2],splits[k-1], splits[k]))
        if(k >3){
          term_test <- c(term_test, paste(splits[k-3],splits[k-2],splits[k-1], splits[k]))
        }
      }
    }
    
    #Return result
    return(term_test)
  }