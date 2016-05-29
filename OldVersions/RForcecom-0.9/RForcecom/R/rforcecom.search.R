#' @export
rforcecom.search <-
function(session, queryString){

 # Retrieve XML via REST API
 h <- basicHeaderGatherer()
 t <- basicTextGatherer()
 endpointPath <- rforcecom.api.getSoslEndpoint(session['apiVersion'])
 queryString <- curlEscape(paste("FIND {", queryString, "}", sep=""))
 URL <- paste(session['instanceURL'], endpointPath, queryString, sep="")
 OAuthString <- paste("Bearer", session['sessionID'])
 httpHeader <- c("Authorization"=OAuthString, "Accept"="application/xml")
 resultSet <- curlPerform(url=URL, httpheader=httpHeader, headerfunction = h$update, writefunction = t$update, ssl.verifypeer=F)
 
 # BEGIN DEBUG
 if(exists("rforcecom.debug") && rforcecom.debug){ message(URL) }
 if(exists("rforcecom.debug") && rforcecom.debug){ message(t$value()) }
 # END DEBUG
 
 x.root <- xmlRoot(xmlTreeParse(t$value(), asText=T))
 
 # Check whether it success or not
 errorcode <- NA
 errormessage <- NA
 try(errorcode <- iconv(xmlValue(x.root[['Error']][['errorCode']]), from="UTF-8", to=""), TRUE)
 try(errormessage <- iconv(xmlValue(x.root[['Error']][['message']]), from="UTF-8", to=""), TRUE)
 if(!anyNA(errorcode,errormessage)){
   stop(paste(errorcode, errormessage, sep=": "))
 }
 
 # Parse XML
 xns <- getNodeSet(xmlParse(t$value()),'//SearchResults/SearchResult')
 xls <- sapply(xns, function(xn){ list(Id=xmlValue(xn), type=xmlGetAttr(xn, "type"), url=xmlGetAttr(xn, "url")) })
 if(length(xls)>0){
   xdf <- data.frame(t(xls))
 } else {
   xdf <- NULL
 }
 
 return(xdf)
}

