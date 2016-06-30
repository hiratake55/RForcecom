#' @export
rforcecom.getServerTimestamp <-
function(session){
  
 # Send records
 endpointPath <- rforcecom.api.getRestEndpoint(session['apiVersion'])
 URL <- paste(session['instanceURL'], endpointPath, "/", sep="")
 OAuthString <- paste("Bearer", session['sessionID'])
 httpHeader <- httr::add_headers("Authorization"=OAuthString, "Accept"="application/xml", "X-PrettyPrint"="1")
 res <- httr::GET(url=URL, config=httpHeader)
 res.content = httr::content(res, as='text', encoding='UTF-8')
 res.header = httr::headers(res)
 
 # BEGIN DEBUG
 if(exists("rforcecom.debug") && rforcecom.debug){ message(URL) }
 if(exists("rforcecom.debug") && rforcecom.debug){ message(res.content) }
 # END DEBUG
 
 # Format as a date
 timeval <- unname(res.header['Date'])
 tzval <- substr(timeval, nchar(timeval)-2, nchar(timeval))
 localtimeval <- substr(timeval, 0, nchar(timeval)-4)
 lct <- Sys.getlocale("LC_TIME")
 Sys.setlocale("LC_TIME", "C")
 posixtime <- as.POSIXlt(localtimeval, format="%a, %d %b %Y %X", tz=tzval)
 Sys.setlocale("LC_TIME", lct)
 
 return(posixtime)
}

