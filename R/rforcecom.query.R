#' @export
rforcecom.query <-
function(session, soqlQuery, queryAll=FALSE){

 # Retrieve XML via REST API
 if(queryAll){
   endpointPath <- rforcecom.api.getSoqlAllEndpoint(session['apiVersion'])
 } else {
   endpointPath <- rforcecom.api.getSoqlEndpoint(session['apiVersion'])
 }
 URL <- paste(session['instanceURL'], endpointPath, RCurl::curlEscape(soqlQuery), sep="")
 OAuthString <- paste("Bearer", session['sessionID'])
 httpHeader <- httr::add_headers("Authorization"=OAuthString, "Accept"="application/xml")
 res <- httr::GET(url=URL, config=httpHeader)
 res.content = httr::content(res, as='text', encoding='UTF-8')
 
 # BEGIN DEBUG
 if(exists("rforcecom.debug") && rforcecom.debug){ message(URL) }
 if(exists("rforcecom.debug") && rforcecom.debug){ message(res.content) }
 # END DEBUG
 
 # Parse XML
 x.root <- xmlRoot(xmlParse(res.content, asText=T))
 
 # Check whether it success or not
 errorcode <- NA
 errormessage <- NA
 try(errorcode <- iconv(xmlValue(x.root[['Error']][['errorCode']]), from="UTF-8", to=""), TRUE)
 try(errormessage <- iconv(xmlValue(x.root[['Error']][['message']]), from="UTF-8", to=""), TRUE)
 if(!is.na(errorcode) && !is.na(errormessage)){
   stop(paste(errorcode, errormessage, sep=": "))
 }
 
 resultset <- query_parser(x.root)
 
 # Check whether it has next record
 try(nextRecordsUrl <- iconv(xmlValue(x.root[['nextRecordsUrl']]), from="UTF-8", to=""), TRUE)
 if(!is.na(nextRecordsUrl)){
   nextRecords <- rforcecom.queryMore(session, nextRecordsUrl)
   resultset <- rbind.fill(resultset, nextRecords)
 }
 
 return(resultset)
}

