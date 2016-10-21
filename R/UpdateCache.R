#' Update local metadata cache
#' 
#' @param uid 	User's email for account with access to EU-US metadata Dydra repository (hope to deprecate soon); character string
#' @param pwd 	User's password for account with access to EU-US metadata Dydra repository (hope to deprecate soon); character string
#' @return Nothing, but updates local CSV 
#' @import 
#' @export 

updateCache <- function(uid, pwd, beaKey){
	beaMetaCheck(beaKey)


}