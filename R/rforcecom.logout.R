#' @export
rforcecom.logout <-
function(session){

# Soap Body
soapBody <- paste0('<?xml version="1.0" encoding="utf-8" ?> \
                      <env:Envelope xmlns:xsd="http://www.w3.org/2001/XMLSchema" \
                          xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" \
                            xmlns:env="http://schemas.xmlsoap.org/soap/envelope/"> \
                      <env:Header>
                        <SessionHeader xmlns="urn:partner.soap.sforce.com">
                          <sessionId>', session['sessionID'], '</sessionId> \
                        </SessionHeader>
                      </env:Header>
                      <env:Body> \
                        <n1:logout xmlns:n1="urn:partner.soap.sforce.com"> \
                        </n1:logout> \
                      </env:Body> \
                      </env:Envelope>\n\n')
 
 # HTTP POST
 h <- basicHeaderGatherer()
 t <- basicTextGatherer()
 URL <- paste(session['instanceURL'], rforcecom.api.getSoapEndpoint(session['apiVersion']), sep="")
 httpHeader <- c("SOAPAction"="logout", "Content-Type"="text/xml", "Accept"="text/xml")
 curlPerform(url=URL, postfields=soapBody, httpheader=httpHeader, headerfunction = h$update, writefunction = t$update, ssl.verifypeer=F)
 
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
 
 # Check response from XML
 response <- xmlValue(x.root[['Body']][['logoutResponse']])
 
 if (length(response)==0){
   success <- TRUE
 } else {
   success <- FALSE
 }
 
 return(success)
}

