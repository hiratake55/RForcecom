#' @export
rforcecom.search <-
function(session, queryString){

 # Retrieve XML via REST API
 endpointPath <- rforcecom.api.getSoslEndpoint(session['apiVersion'])
 queryString <- RCurl::curlEscape(paste("FIND {", queryString, "}", sep=""))
 URL <- paste(session['instanceURL'], endpointPath, queryString, sep="")
 OAuthString <- paste("Bearer", session['sessionID'])
 httpHeader <- httr::add_headers("Authorization"=OAuthString, "Accept"="application/xml")
 res <- httr::GET(url=URL, config=httpHeader)
 res.content = httr::content(res, as='text', encoding='UTF-8')
 
 # BEGIN DEBUG
 if(exists("rforcecom.debug") && rforcecom.debug){ message(URL) }
 if(exists("rforcecom.debug") && rforcecom.debug){ message(res.content) }
 # END DEBUG
 
 x.root <- xmlRoot(xmlTreeParse(res.content, asText=T))
 
 # Check whether it success or not
 errorcode <- NA
 errormessage <- NA
 try(errorcode <- iconv(xmlValue(x.root[['Error']][['errorCode']]), from="UTF-8", to=""), TRUE)
 try(errormessage <- iconv(xmlValue(x.root[['Error']][['message']]), from="UTF-8", to=""), TRUE)
 if(!anyNA(errorcode,errormessage)){
   stop(paste(errorcode, errormessage, sep=": "))
 }
 
 # Parse XML
 xns <- getNodeSet(xmlParse(res.content),'//SearchResults/SearchResult')
 xls <- sapply(xns, function(xn){ list(Id=xmlValue(xn), type=xmlGetAttr(xn, "type"), url=xmlGetAttr(xn, "url")) })
 if(length(xls)>0){
   xdf <- data.frame(t(xls))
 } else {
   xdf <- NULL
 }
 
 return(xdf)
}

