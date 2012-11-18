rforcecom.getServerTimestamp <-
function(session){
 # Load packages
 require(XML)
 require(RCurl)
 
 # Send records
 h <- basicHeaderGatherer()
 t <- basicTextGatherer()
 endpointPath <- rforcecom.api.getRestEndpoint(session['apiVersion'])
 URL <- paste(session['instanceURL'], endpointPath, "/", sep="")
 OAuthString <- paste("OAuth", session['sessionID'])
 httpHeader <- c("Authorization"=OAuthString, "Accept"="application/xml", "X-PrettyPrint"="1")
 curlPerform(url=URL, httpheader=httpHeader, headerfunction = h$update, writefunction = t$update, ssl.verifypeer=F)
 
 # BEGIN DEBUG
 if(exists("rforcecom.debug") && rforcecom.debug){ message(URL) }
 if(exists("rforcecom.debug") && rforcecom.debug){ message(h$value()) }
 if(exists("rforcecom.debug") && rforcecom.debug){ message(t$value()) }
 # END DEBUG
 
 # Format as a date
 timeval <- unname(h$value()['Date'])
 tzval <- substr(timeval, nchar(timeval)-2, nchar(timeval))
 localtimeval <- substr(timeval, 0, nchar(timeval)-4)
 lct <- Sys.getlocale("LC_TIME")
 Sys.setlocale("LC_TIME", "C")
 posixtime <- as.POSIXlt(localtimeval, format="%a, %d %b %Y %X", tz=tzval)
 Sys.setlocale("LC_TIME", lct)
 
 return(posixtime)
}

