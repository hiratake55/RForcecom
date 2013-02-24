rforcecom.login <-
function(username, password, instanceURL, apiVersion){
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
 h <- basicTextGatherer()
 URL <- paste(instanceURL, rforcecom.api.getSoapEndpoint(apiVersion), sep="")
 httpHeader <- c("SOAPAction"="login","Content-Type"="text/xml")
 curlPerform(url=URL, httpheader=httpHeader, postfields=soapBody, writefunction = h$update, ssl.verifypeer=F)
 
 # Parse XML
 x.root <- xmlRoot(xmlTreeParse(h$value(), asText=T))
 sessionID <- xmlValue(x.root[['Body']][['loginResponse']][['result']][['sessionId']])
 return(c(sessionID=sessionID, instanceURL=instanceURL, apiVersion=apiVersion))
}

