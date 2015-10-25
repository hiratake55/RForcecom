#' @export
rforcecom.login <-
function(username, password, instanceURL, apiVersion){

 if(as.numeric(apiVersion) < 20) stop("the earliest supported API version is 20.0")
 
 # Soap Body
 soapBody <- paste0('<?xml version="1.0" encoding="utf-8" ?> \
                    <env:Envelope xmlns:xsd="http://www.w3.org/2001/XMLSchema" \
                        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" \
                        xmlns:env="http://schemas.xmlsoap.org/soap/envelope/"> \
                      <env:Body> \
                        <n1:login xmlns:n1="urn:partner.soap.sforce.com">\n',
                          as(newXMLNode("username", username), "character"),
                          as(newXMLNode("password", password), "character"),
                        '</n1:login> \
                      </env:Body> \
                    </env:Envelope>\n\n')
 
 # HTTP POST
 h <- basicHeaderGatherer()
 t <- basicTextGatherer()
 URL <- paste("https://login.salesforce.com/", rforcecom.api.getSoapEndpoint(apiVersion), sep="")
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

