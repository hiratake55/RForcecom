rforcecom.search <-
function(session, queryString){
 # Retrieve XML via REST API
 h <- basicTextGatherer()
 endpointPath <- rforcecom.api.getSoslEndpoint(session['apiVersion'])
 queryString <- curlEscape(paste("FIND {", queryString, "}", sep=""))
 URL <- paste(session['instanceURL'], endpointPath, queryString, sep="")
 OAuthString <- paste("OAuth", session['sessionID'])
 httpHeader <- c("Authorization"=OAuthString, "Accept"="application/xml")
 curlPerform(url=URL, httpheader=httpHeader, writefunction = h$update, ssl.verifypeer=F)
 
 # Parse XML
 xns <- getNodeSet(xmlParse(h$value()),'//SearchResults/SearchResult')
 xls <- sapply(xns, function(xn){ list(Id=xmlValue(xn), type=xmlGetAttr(xn, "type"), url=xmlGetAttr(xn, "url")) })
 xdf <- data.frame(t(xls))
 
 return(xdf)
}

