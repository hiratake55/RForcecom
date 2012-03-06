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
 h <- basicTextGatherer()
 endpointPath <- rforcecom.api.getExternalIdFieldEndpoint(session['apiVersion'], objectName, externalIdField, externalId)
 URL <- paste(session['instanceURL'], endpointPath, sep="")
 OAuthString <- paste("OAuth", session['sessionID'])
 httpHeader <- c("Authorization"=OAuthString, "Accept"="application/xml", 'Content-Type'="application/xml")
 resultSet <- curlPerform(url=URL, httpheader=httpHeader, writefunction = h$update, ssl.verifypeer=F, postfields=xmlBody, customrequest="PATCH")
 return(resultSet)
}

