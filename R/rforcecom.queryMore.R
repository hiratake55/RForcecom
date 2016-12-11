#' @export
rforcecom.queryMore <-
function(session, nextRecordsUrl){

 # Trim the first slash
 nextRecordsUrl <- sub("^/", "", nextRecordsUrl)

 # Retrieve XML via REST API
 endpointPath <- rforcecom.api.getSoqlEndpoint(session['apiVersion'])
 URL <- paste(session['instanceURL'], nextRecordsUrl, sep="")
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
 
 resultset <- query_parser(httr::content(res, as='parsed', encoding='UTF-8'))
 
 # Check whether it has next record
 try(nextRecordsUrl <- iconv(xmlValue(x.root[['nextRecordsUrl']]), from="UTF-8", to=""), TRUE)
 if(!is.na(nextRecordsUrl)){
   nextRecords <- rforcecom.queryMore(session, nextRecordsUrl)
   resultset <- rbind.fill(resultset, nextRecords)
 }
 
 return(resultset)
}

