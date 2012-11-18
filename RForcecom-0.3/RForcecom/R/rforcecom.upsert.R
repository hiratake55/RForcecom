rforcecom.upsert <-
function(session, objectName, externalIdField, externalId, fields){
 # Load packages
 require(XML)
 require(RCurl)
 
 # Create XML
 xmlElem <- ""
 for(i in 1:length(fields)){
  fieldValue <- iconv(fields[i], from="", to="UTF-8")
  xmlElem <- paste(xmlElem, "<", names(fields[i]), ">",fields[i] ,"</", names(fields[i]), ">",sep="")
 }
 xmlBody <- paste("<?xml version=\"1.0\" encoding=\"UTF-8\"?><root>", xmlElem, "</root>", sep="")
 
 # Send records
 h <- basicHeaderGatherer()
 t <- basicTextGatherer()
 endpointPath <- rforcecom.api.getExternalIdFieldEndpoint(session['apiVersion'], objectName, externalIdField, externalId)
 URL <- paste(session['instanceURL'], endpointPath, sep="")
 OAuthString <- paste("OAuth", session['sessionID'])
 httpHeader <- c("Authorization"=OAuthString, "Accept"="application/xml", 'Content-Type'="application/xml")
 resultSet <- curlPerform(url=URL, httpheader=httpHeader, headerfunction = h$update, writefunction = t$update, ssl.verifypeer=F, postfields=xmlBody, customrequest="PATCH")
 
 # BEGIN DEBUG
 if(exists("rforcecom.debug") && rforcecom.debug){ message(URL) }
 if(exists("rforcecom.debug") && rforcecom.debug){ message(t$value()) }
 # END DEBUG
 
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
  
  # Parse XML
  xdf <- xmlToDataFrame(getNodeSet(xmlParse(t$value()),'//Result'))
  return(xdf)
 }
 
}

