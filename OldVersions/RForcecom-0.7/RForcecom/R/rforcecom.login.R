rforcecom.login <-
function(username, password, instanceURL, apiVersion){
 # Load packages
 #if(!require(XML)){ install.packages("XML"); stop(!require(XML)) }
 #if(!require(RCurl)){ install.packages("RCurl"); stop(!require(RCurl)) }
 #if(!require(plyr)){ install.packages("plyr"); stop(!require(plyr)) }
 
 # Soap Body
 soapBody <- paste('<?xml version="1.0" encoding="utf-8" ?> \
<env:Envelope xmlns:xsd="http://www.w3.org/2001/XMLSchema" \
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" \
    xmlns:env="http://schemas.xmlsoap.org/soap/envelope/"> \
  <env:Body> \
    <n1:login xmlns:n1="urn:partner.soap.sforce.com"> \
      <n1:username>', username, '</n1:username> \
      <n1:password>', password, '</n1:password> \
    </n1:login> \
  </env:Body> \
</env:Envelope>\n\n', sep="")
 
 # HTTP POST
 h <- basicHeaderGatherer()
 t <- basicTextGatherer()
 URL <- paste(instanceURL, rforcecom.api.getSoapEndpoint(apiVersion), sep="")
 httpHeader <- c("SOAPAction"="login","Content-Type"="text/xml")
 curlPerform(url=URL, httpheader=httpHeader, postfields=soapBody, headerfunction = h$update, writefunction = t$update, ssl.verifypeer=F)

 # BEGIN DEBUG
 if(exists("rforcecom.debug") && rforcecom.debug){ message(URL) }
 if(exists("rforcecom.debug") && rforcecom.debug){ message(t$value()) }
 # END DEBUG
 
 # Parse XML
 x.root <- xmlRoot(xmlTreeParse(t$value(), asText=T))
 
 # Check whether it success or not
 faultstring <- NA
 try(faultstring <- iconv(xmlValue(x.root[['Body']][['Fault']][['faultstring']]), from="UTF-8", to=""), TRUE)
 if(!is.na(faultstring)){
  stop(faultstring)
 }
 
 # Retrieve sessionID from XML
 sessionID <- xmlValue(x.root[['Body']][['loginResponse']][['result']][['sessionId']])
 instanceURL <- sub('(https://[^/]+/).*', '\\1', xmlValue(x.root[['Body']][['loginResponse']][['result']][['serverUrl']]))
 return(c(sessionID=sessionID, instanceURL=instanceURL, apiVersion=apiVersion))
}

