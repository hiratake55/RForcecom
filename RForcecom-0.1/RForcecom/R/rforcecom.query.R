rforcecom.query <-
function(session, soqlQuery){
 # Retrieve XML via REST API
 h <- basicTextGatherer()
 endpointPath <- rforcecom.api.getSoqlEndpoint(session['apiVersion'])
 URL <- paste(session['instanceURL'], endpointPath, curlEscape(soqlQuery), sep="")
 OAuthString <- paste("OAuth", session['sessionID'])
 httpHeader <- c("Authorization"=OAuthString, "Accept"="application/xml")
 curlPerform(url=URL, httpheader=httpHeader, writefunction = h$update, ssl.verifypeer=F)
 
 # Parse XML
 xns <- getNodeSet(xmlParse(h$value()),'//records')
 xdf <- xmlToDataFrame(xns)
 xdf.iconv <- data.frame(lapply(xdf,iconv, from="UTF-8", to=""))
 return(data.frame(xdf.iconv))
}

