#' Abort Bulk API Job 
#' 
#' This function aborts a Job in the Salesforce Bulk API
#'
#' @usage rforcecom.abortBulkJob(session, jobId)
#' @concept bulk job salesforce api
#' @references \url{https://developer.salesforce.com/docs/atlas.en-us.api_asynch.meta/api_asynch/}
#' @param session a named character vector defining parameters of the api connection as returned by \link{rforcecom.login}
#' @param jobId a character string defining the salesforce id assigned to a submitted job as returned by \link{rforcecom.createBulkJob}
#' @return A \code{list} of parameters defining the now aborted job
#' @examples
#' \dontrun{
#' job_abort_info <- rforcecom.abortBulkJob(session, jobId=job_info$id)
#' }
#' @export
rforcecom.abortBulkJob <-
  function(session, jobId){
    
    # XML Body
    xmlBody <- '<?xml version="1.0" encoding="UTF-8"?>
    <jobInfo xmlns="http://www.force.com/2009/06/asyncapi/dataload">
    <state>Aborted</state>
    </jobInfo>'
    
    # Send request
    h <- basicHeaderGatherer() 
    t <- basicTextGatherer()
    endpointPath <- rforcecom.api.getBulkEndpoint(session['apiVersion'])
    URL <- paste(session['instanceURL'], endpointPath, '/job/', jobId, sep="")
    OAuthString <- unname(session['sessionID'])
    httpHeader <- c("X-SFDC-Session"=OAuthString, "Accept"="application/xml", 'Content-Type'="application/xml")
    curlPerform(url=URL, httpheader=httpHeader, headerfunction = h$update, writefunction = t$update, ssl.verifypeer=F, postfields=xmlBody)
    
    # BEGIN DEBUG
    if(exists("rforcecom.debug") && rforcecom.debug){ message(URL) }
    if(exists("rforcecom.debug") && rforcecom.debug){ message(t$value()) }
    # END DEBUG
    
    # Parse XML
    x.root <- xmlRoot(xmlTreeParse(t$value(), asText=T))
    
    # Check whether it success or not
    errorcode <- NA
    errormessage <- NA
    try(errorcode <- iconv(xmlValue(x.root[['exceptionCode']]), from="UTF-8", to=""), TRUE)
    try(errormessage <- iconv(xmlValue(x.root[['exceptionMessage']]), from="UTF-8", to=""), TRUE)
    if(!is.na(errorcode) && !is.na(errormessage)){
      stop(paste(errorcode, errormessage, sep=": "))
    }
    
    # Return XML response as list
    return(xmlToList(x.root))
  }
