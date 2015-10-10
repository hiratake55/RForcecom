#' Retrieving the Results of a Bulk Query Batch in a Bulk API Job 
#' 
#' This function returns the resultset of a Bulk Query batch
#' which has already been submitted to Bulk API Job and has Completed state
#'
#' @usage rforcecom.getBulkQueryResult(session, jobId, batchId, resultId)
#' @concept bulk batch salesforce api
#' @references \url{https://developer.salesforce.com/docs/atlas.en-us.api_asynch.meta/api_asynch/}
#' @param session a named character vector defining parameters of the api connection as returned by \link{rforcecom.login}
#' @param jobId a character string defining the salesforce id assigned to a submitted job as returned by \link{rforcecom.createBulkJob}
#' @param batchId a character string defining the salesforce id assigned to a submitted batch as returned by \link{rforcecom.createBulkBatch}
#' @param resultId a character string returned from \link{rforcecom.getBatchDetails} when a query has completed and specifies how to get the recordset
#' @return A \code{data.frame}, formatted by salesforce, containing query results
#' @examples
#' \dontrun{
#' result <- rforcecom.getBatchDetails(session, 
#'                                     jobId=batch_query_info$jobId, 
#'                                     batchId=batch_query_info$id)
#' recordset <- rforcecom.getBulkQueryResult(session, 
#'                                           jobId=batch_query_info$jobId, 
#'                                           batchId=batch_query_info$id, 
#'                                           resultId=result$result)
#' }
#' @export
rforcecom.getBulkQueryResult <- 
  function(session, jobId, batchId, resultId){
    
    # Send request
    h <- basicHeaderGatherer() 
    t <- basicTextGatherer()
    endpointPath <- rforcecom.api.getBulkEndpoint(session['apiVersion'])
    URL <- paste(session['instanceURL'], endpointPath, '/job/', jobId, '/batch/', batchId, '/result/', resultId, sep="")
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
      res <- xmlToList(x.root)
    } else {
      con <- textConnection(t$value())
      res <- read.csv(con, stringsAsFactors=FALSE)
    }
    return(res)
  }