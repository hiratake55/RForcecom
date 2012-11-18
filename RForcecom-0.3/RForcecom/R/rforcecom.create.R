rforcecom.create <- 
function(session, objectName, fields){
 # Load packages
 require(XML)
 require(RCurl)
 
 # Create XML node set
 rforcecom.create.createXmlNodeSet <- function(nodelist){
  xmlNodeSet <- ""
  for(i in 1:length(nodelist)){
   if(!is.null(names(nodelist[i]))){
    nodeValue <- iconv(nodelist[i], from="", to="UTF-8")
    xmlNodeSet <- paste(xmlNodeSet, "<", names(nodelist[i]), ">", nodeValue ,"</", names(nodelist[i]), ">",sep="")
   }
  }
  return(xmlNodeSet)
 }
 
 # Create an XML
 xmlElem <- ""
 if(is.data.frame(fields)){
  xmlElem <- apply(fields, 2, rforcecom.create.createXmlNodeSet)
 }
 else if(is.vector(fields)){
  xmlElem <- rforcecom.create.createXmlNodeSet(fields)
 }
 
 xmlBody <- paste("<?xml version=\"1.0\" encoding=\"UTF-8\"?><root>", xmlElem, "</root>", sep="")
 
 # Send records
 h <- basicHeaderGatherer()
 t <- basicTextGatherer()
 endpointPath <- rforcecom.api.getObjectEndpoint(session['apiVersion'], objectName)
 URL <- paste(session['instanceURL'], endpointPath, sep="")
 OAuthString <- paste("OAuth", session['sessionID'])
 httpHeader <- c("Authorization"=OAuthString, "Accept"="application/xml", 'Content-Type'="application/xml")
 curlPerform(url=URL, httpheader=httpHeader, headerfunction = h$update, writefunction = t$update, ssl.verifypeer=F, postfields=xmlBody)
 
 # BEGIN DEBUG
 if(exists("rforcecom.debug") && rforcecom.debug){ message(URL) }
 if(exists("rforcecom.debug") && rforcecom.debug){ message(xmlBody) }
 if(exists("rforcecom.debug") && rforcecom.debug){ message(t$value()) }
 # END DEBUG
 
 # Parse XML
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

