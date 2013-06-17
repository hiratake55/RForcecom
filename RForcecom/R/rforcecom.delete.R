rforcecom.delete <-
function(session, objectName, id){
 # Load packages
 if(!require(XML)){ install.packages("XML"); stop(!require(XML)) }
 if(!require(RCurl)){ install.packages("RCurl"); stop(!require(RCurl)) }
 
 # Send records
 h <- basicHeaderGatherer()
 t <- basicTextGatherer()
 endpointPath <- rforcecom.api.getRecordEndpoint(session['apiVersion'], objectName, id)
 URL <- paste(session['instanceURL'], endpointPath, sep="")
 OAuthString <- paste("Bearer", session['sessionID'])
 httpHeader <- c("Authorization"=OAuthString, "Accept"="application/xml", 'Content-Type'="application/xml")
 if(!exists("rforcecom.settings.curlopts")){ rforcecom.settings.curlopts <- list() }
 httpResponse <- curlPerform(url=URL, httpheader=httpHeader, headerfunction = h$update, writefunction = t$update, ssl.verifypeer=F, customrequest="DELETE", .opts=rforcecom.settings.curlopts)
 
 # BEGIN DEBUG
 if(exists("rforcecom.debug") && rforcecom.debug){ message(URL) }
 if(exists("rforcecom.debug") && rforcecom.debug){ message(t$value()) }
 # END DEBUG
  
 # Parse XML
 if(t$value() != ""){
  x.root <- xmlRoot(xmlTreeParse(t$value(), asText=T))
  
  # Check whether it success or not
  errorcode <- NA
  errormessage <- NA
  try(errorcode <- iconv(xmlValue(x.root[['Error']][['errorCode']]), from="UTF-8", to=""), TRUE)
  try(errormessage <- iconv(xmlValue(x.root[['Error']][['message']]), from="UTF-8", to=""), TRUE)
  if(!is.na(errorcode) && !is.na(errormessage)){
   stop(paste(errorcode, errormessage, sep=": "))
  }
 }
 
}

