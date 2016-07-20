#' @export
rforcecom.getObjectList <-
function(session){

 # Send a query
 endpointPath <- rforcecom.api.getObjectListEndpoint(session['apiVersion'])
 URL <- paste(session['instanceURL'], endpointPath, sep="")
 OAuthString <- paste("Bearer", session['sessionID'])
 httpHeader <- httr::add_headers("Authorization"=OAuthString, "Accept"="application/xml", 'Content-Type'="application/xml")
 res <- httr::GET(url=URL, config=httpHeader)
 res.content = httr::content(res, as='text', encoding='UTF-8')
 
 # BEGIN DEBUG
 if(exists("rforcecom.debug") && rforcecom.debug){ message(URL) }
 if(exists("rforcecom.debug") && rforcecom.debug){ message(res.content) }
 # END DEBUG
 
 # Parse XML
 x.root <- xmlRoot(xmlTreeParse(res.content, asText=T))
 
 # Check whether it success or not
 errorcode <- NA
 errormessage <- NA
 try(errorcode <- iconv(xmlValue(x.root[['Error']][['errorCode']]), from="UTF-8", to=""), TRUE)
 try(errormessage <- iconv(xmlValue(x.root[['Error']][['message']]), from="UTF-8", to=""), TRUE)
 if(!is.na(errorcode) && !is.na(errormessage)){
  stop(paste(errorcode, errormessage, sep=": "))
 }
 
 # Parse XML
 xdf <- xmlToDataFrame(getNodeSet(xmlParse(res.content),'//sobjects'))
 return(xdf)
 
}

