rforcecom.delete <-
function(session, objectName, id){
 # Send records
 h <- basicTextGatherer()
 endpointPath <- rforcecom.api.getRecordEndpoint(session['apiVersion'], objectName, id)
 URL <- paste(session['instanceURL'], endpointPath, sep="")
 OAuthString <- paste("OAuth", session['sessionID'])
 httpHeader <- c("Authorization"=OAuthString, "Accept"="application/xml", 'Content-Type'="application/xml")
 resultSet <- httpResponse <- curlPerform(url=URL, httpheader=httpHeader, writefunction = h$update, ssl.verifypeer=F, customrequest="DELETE")
 return(resultSet)
}

