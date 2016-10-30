###BEA Main Terms
#readLines
setwd("/Users/sigmamonstr/Github/project-eu-us/rawdata")
a = readLines("bea_terms")
master = data.frame(acronym=NA, term=NA)
for(k in 1:length(a)){

  val  =regexpr(">(.*)\\<", a[k])
  
  if(val[1]>-1){
    temp = substr(a[k], val+1, val+attr(val,"match.length")-3)
    print(temp)
    val2  =regexpr("\\((.*)\\)", temp)
    if(val2[1] == -1){
      temp2 = ""
    } else {
      temp2 = substr(temp, val2+1, val2+attr(val2, "match.length")-2)
      temp = substr(temp  , 1, regexpr("\\(",temp )-2)
    }
    master = rbind(master, data.frame(acronym = temp2, term =temp ))
  }
}
master$source <- "BEA"


##EU
b = read.csv("eurostate_terms.csv",stringsAsFactors = FALSE , fileEncoding="latin1" )
b = b[nchar(b[,2])<50,]
b$source = "EU"

##Combine
master = rbind(master,b)
master <- master[!duplicated(master$acronym),]
master <- master[nchar(master$acronym)>2 & nchar(master$acronym)<=4,]
write.csv(master,"Acronym_Table.csv",row.names=FALSE)

##

rec <-  "the gross domestic product gross national product"

##Parse terms into 2 to 4 n-grams
parse_query <- function(rec){

  splits <-  unlist(strsplit(rec," "))
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
  
  return(term_test)
}






consistent_query <- function(rec){
  
  ##Parse query
  combos <- parse_query(rec)
  
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


words <- c("beer", "wiskey", "wine")
hunspell_check(words)
bad_words <- hunspell_find("supplemen incom")
hunspell_suggest(bad_words[[1]])
