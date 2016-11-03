###Build Acronym List

##US BEA Terms
  setwd("")
  
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



