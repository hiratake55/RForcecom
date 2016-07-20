#' @export
rforcecom.upsert <-
function(session, objectName, externalIdField, externalId, fields){

 # Create XML
 xmlElem <- ""
 for(i in 1:length(fields)){
  fieldValue <- iconv(fields[i], from="", to="UTF-8")
  xmlElem <- paste(xmlElem, "<", names(fields[i]), ">",fields[i] ,"</", names(fields[i]), ">",sep="")
 }
 xmlBody <- paste("<?xml version=\"1.0\" encoding=\"UTF-8\"?><root>", xmlElem, "</root>", sep="")
 
 # Send records
 endpointPath <- rforcecom.api.getExternalIdFieldEndpoint(session['apiVersion'], objectName, externalIdField, externalId)
 URL <- paste(session['instanceURL'], endpointPath, sep="")
 OAuthString <- paste("Bearer", session['sessionID'])
 httpHeader <- httr::add_headers("Authorization"=OAuthString, "Accept"="application/xml", 'Content-Type'="application/xml")
 res <- httr::PATCH(url=URL, config=httpHeader, body=xmlBody)
 res.content = httr::content(res, as='text', encoding='UTF-8')
 
 # BEGIN DEBUG
 if(exists("rforcecom.debug") && rforcecom.debug){ message(URL) }
 if(exists("rforcecom.debug") && rforcecom.debug){ message(res.content) }
 # END DEBUG
 
 # Parse XML
 if(res.content != ""){
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
  xdf <- xmlToDataFrame(getNodeSet(xmlParse(res.content),'//Result'))
  return(xdf)
 }
 
}

