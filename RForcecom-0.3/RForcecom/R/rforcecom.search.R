rforcecom.search <-
function(session, queryString){
 # Load packages
 require(XML)
 require(RCurl)
 
 # Retrieve XML via REST API
 h <- basicHeaderGatherer()
 t <- basicTextGatherer()
 endpointPath <- rforcecom.api.getSoslEndpoint(session['apiVersion'])
 queryString <- curlEscape(paste("FIND {", queryString, "}", sep=""))
 URL <- paste(session['instanceURL'], endpointPath, queryString, sep="")
 OAuthString <- paste("OAuth", session['sessionID'])
 httpHeader <- c("Authorization"=OAuthString, "Accept"="application/xml")
 resultSet <- curlPerform(url=URL, httpheader=httpHeader, headerfunction = h$update, writefunction = t$update, ssl.verifypeer=F)
 
 # BEGIN DEBUG
 if(exists("rforcecom.debug") && rforcecom.debug){ message(URL) }
 if(exists("rforcecom.debug") && rforcecom.debug){ message(t$value()) }
 # END DEBUG
 
 # Parse XML
 xns <- getNodeSet(xmlParse(t$value()),'//SearchResults/SearchResult')
 xls <- sapply(xns, function(xn){ list(Id=xmlValue(xn), type=xmlGetAttr(xn, "type"), url=xmlGetAttr(xn, "url")) })
 xdf <- data.frame(t(xls))
 
 # Parse XML
 if(t$value() != ""){
  x.root <- xmlRoot(xmlTreeParse(t$value(), asText=T))
  
  # Check whether it success
  errorcode <- NA
  errormessage <- NA
  try(errorcode <- iconv(xmlValue(x.root[['Error']][['errorCode']]), from="UTF-8", to=""), TRUE)
  try(errormessage <- iconv(xmlValue(x.root[['Error']][['message']]), from="UTF-8", to=""), TRUE)
  if(!is.na(errorcode) && !is.na(errormessage)){
   stop(paste(errorcode, errormessage, sep=": "))
  }
 }
 
 return(xdf)
}

