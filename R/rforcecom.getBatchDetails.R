#' Returning the Details of a Batch in a Bulk API Job 
#' 
#' This function returns detailed (row-by-row) information on an existing batch
#' which has already been submitted to Bulk API Job
#'
#' @usage rforcecom.getBatchDetails(session, jobId, batchId)
#' @concept bulk batch salesforce api
#' @references \url{https://developer.salesforce.com/docs/atlas.en-us.api_asynch.meta/api_asynch/}
#' @param session a named character vector defining parameters of the api connection as returned by \link{rforcecom.login}
#' @param jobId a character string defining the salesforce id assigned to a submitted job as returned by \link{rforcecom.createBulkJob}
#' @param batchId a character string defining the salesforce id assigned to a submitted batch as returned by \link{rforcecom.createBulkBatch}
#' @return A \code{data.frame}, formatted by salesforce, with information containing the success or failure or certain rows in a submitted batch, 
#' unless the operation was query, then it is a data.frame containing the resultId for retrieving the recordset.
#' @examples
#' \dontrun{
#' batch_details <- rforcecom.getBatchDetails(session, jobId=job_info$id, batchId=batches_info[[1]]$id)
#' }
#' @export
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
    
    is_XML <- tryCatch({xmlRoot(xmlTreeParse(t$value(), asText=T)); TRUE}, error=function(e){return(FALSE)})
    # Check whether it success or not
    errorcode <- NA
    errormessage <- NA    
    if (is_XML) {
      x.root <- xmlRoot(xmlTreeParse(t$value(), asText=T))
      try(errorcode <- iconv(xmlValue(x.root[['exceptionCode']]), from="UTF-8", to=""), TRUE)
      try(errormessage <- iconv(xmlValue(x.root[['exceptionMessage']]), from="UTF-8", to=""), TRUE)
    }
    if(!is.na(errorcode) && !is.na(errormessage))
      stop(paste(errorcode, errormessage, sep=": "))
    
    if (is_XML) {
      res <- data.frame(xmlToList(x.root), stringsAsFactors=FALSE)
    } else {
      con <- textConnection(t$value())
      res <- read.csv(con, stringsAsFactors=FALSE)
    }
    return(res)
    
  }