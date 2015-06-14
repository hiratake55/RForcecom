rforcecom.getBatchDetails <- 
  function(session, jobId, batchId){
    
    # Send request
    h <- basicHeaderGatherer() 
    t <- basicTextGatherer()
    endpointPath <- rforcecom.api.getBulkEndpoint(session['apiVersion'])
    URL <- paste(session['instanceURL'], endpointPath, '/job/', jobId, '/batch/', batchId, '/result', sep="")
    OAuthString <- unname(session['sessionID'])
    httpHeader <- c("X-SFDC-Session"=OAuthString, "Accept"="application/xml", 'Content-Type'="application/xml")
    curlPerform(url=URL, httpheader=httpHeader, headerfunction = h$update, writefunction = t$update, ssl.verifypeer=F)
    
    # BEGIN DEBUG
    if(exists("rforcecom.debug") && rforcecom.debug){ message(URL) }
    if(exists("rforcecom.debug") && rforcecom.debug){ message(t$value()) }
    # END DEBUG
    
    is_error <- tryCatch({xmlRoot(xmlTreeParse(t$value(), asText=T)); TRUE}, error=function(e){return(FALSE)})
    # Check whether it success or not
    errorcode <- NA
    errormessage <- NA    
    if (is_error) {
      x.root <- xmlRoot(xmlTreeParse(t$value(), asText=T))
      try(errorcode <- iconv(xmlValue(x.root[['exceptionCode']]), from="UTF-8", to=""), TRUE)
      try(errormessage <- iconv(xmlValue(x.root[['exceptionMessage']]), from="UTF-8", to=""), TRUE)
    }
    if(!is.na(errorcode) && !is.na(errormessage))
      stop(paste(errorcode, errormessage, sep=": "))
    
    con <- textConnection(t$value())
    res_data <- read.csv(con)
    return(res_data)
    
  }