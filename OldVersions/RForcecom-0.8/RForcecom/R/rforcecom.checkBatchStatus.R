#' Checking the Status of a Batch in a Bulk API Job 
#' 
#' This function checks on and returns status information on an existing batch
#' which has already been submitted to Bulk API Job
#'
#' @usage rforcecom.checkBatchStatus(session, jobId, batchId)
#' @concept bulk batch salesforce api
#' @references \url{https://developer.salesforce.com/docs/atlas.en-us.api_asynch.meta/api_asynch/}
#' @param session a named character vector defining parameters of the api connection as returned by \link{rforcecom.login}
#' @param jobId a character string defining the salesforce id assigned to a submitted job as returned by \link{rforcecom.createBulkJob}
#' @param batchId a character string defining the salesforce id assigned to a submitted batch as returned by \link{rforcecom.createBulkBatch}
#' @return A \code{list} of parameters defining the batch identified by the batchId
#' @examples
#' \dontrun{
#' batch_status <- rforcecom.checkBatchStatus(session, jobId=job_info$id, batchId=batches_info[[1]]$id)
#' }
#' @export
rforcecom.checkBatchStatus <- 
  function(session, jobId, batchId){
    
    # Send request
    h <- basicHeaderGatherer() 
    t <- basicTextGatherer()
    endpointPath <- rforcecom.api.getBulkEndpoint(session['apiVersion'])
    URL <- paste(session['instanceURL'], endpointPath, '/job/', jobId, '/batch/', batchId, sep="")
    OAuthString <- unname(session['sessionID'])
    httpHeader <- c("X-SFDC-Session"=OAuthString, "Accept"="application/xml", 'Content-Type'="application/xml")
    curlPerform(url=URL, httpheader=httpHeader, headerfunction = h$update, writefunction = t$update, ssl.verifypeer=F)
    
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