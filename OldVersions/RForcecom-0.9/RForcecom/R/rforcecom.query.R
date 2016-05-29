#' @export
rforcecom.query <-
function(session, soqlQuery, queryAll=FALSE){

 # Retrieve XML via REST API
 h <- basicHeaderGatherer()
 t <- basicTextGatherer()
 if(queryAll){
   endpointPath <- rforcecom.api.getSoqlAllEndpoint(session['apiVersion'])
 } else {
   endpointPath <- rforcecom.api.getSoqlEndpoint(session['apiVersion'])
 }
 URL <- paste(session['instanceURL'], endpointPath, curlEscape(soqlQuery), sep="")
 OAuthString <- paste("Bearer", session['sessionID'])
 httpHeader <- c("Authorization"=OAuthString, "Accept"="application/xml")
 curlPerform(url=URL, httpheader=httpHeader, headerfunction = h$update, writefunction = t$update, ssl.verifypeer=F)
 
 # BEGIN DEBUG
 if(exists("rforcecom.debug") && rforcecom.debug){ message(URL) }
 if(exists("rforcecom.debug") && rforcecom.debug){ message(t$value()) }
 # END DEBUG
 
 # Parse XML
 x.root <- xmlRoot(xmlParse(t$value(), asText=T))
 
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

