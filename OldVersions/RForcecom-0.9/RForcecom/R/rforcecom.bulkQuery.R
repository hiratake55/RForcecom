#' Run Bulk Query 
#' 
#' This function is a convenience wrapper for submitting and retrieving 
#' bulk query API jobs
#'
#' @usage rforcecom.bulkQuery(session, 
#'                            soqlQuery,
#'                            object,
#'                            interval_seconds=5,
#'                            max_attempts=100, 
#'                            verbose=FALSE)
#' @include rforcecom.createBulkJob.R rforcecom.submitBulkQuery.R rforcecom.checkBatchStatus.R rforcecom.getBatchDetails.R rforcecom.getBulkQueryResult.R rforcecom.closeBulkJob.R
#' @concept bulk job salesforce api query
#' @references \url{https://developer.salesforce.com/docs/atlas.en-us.api_asynch.meta/api_asynch/}
#' @param session a named character vector defining parameters of the api connection as returned by \link{rforcecom.login}
#' @param soqlQuery a character string defining a SOQL query. (ex: "SELECT Id, Name FROM Account")
#' @param object a character string defining the target salesforce object that the operation will be performed on. 
#' This must match the target object in the query
#' @param interval_seconds an integer defining the seconds between attempts to check for job completion
#' @param max_attempts an integer defining then max number attempts to check for job completion before stopping
#' @param verbose a boolean on whether to print the API attempt numbers
#' @return A \code{data.frame} of the recordset returned by query
#' @examples
#' \dontrun{
#' # select all Ids from Account object
#' ids <- rforcecom.bulkQuery(session, soqlQuery='Select Id from Account', object='Account')
#' }
#' @export
rforcecom.bulkQuery <- function(session,
                               soqlQuery,
                               object,
                               interval_seconds=5,
                               max_attempts=100, 
                               verbose=FALSE){
  
  job_info <- rforcecom.createBulkJob(session, operation='query', object=object)
  batch_query_info <- rforcecom.submitBulkQuery(session,
                                                jobId=job_info$id,
                                                query=soqlQuery)
  status_complete <- FALSE
  z <- 1
  Sys.sleep(interval_seconds)
  while (z < max_attempts & !status_complete){
    if (verbose){
      message(paste0("Attempt #", z))
    }
    Sys.sleep(interval_seconds)
    batch_query_status <- rforcecom.checkBatchStatus(session,
                                                     jobId=batch_query_info$jobId,
                                                     batchId=batch_query_info$id)
    status_complete <- (batch_query_status$state=='Completed')
    z <- z + 1
  }
  if (!status_complete) {
    message('Issue with batches submitted.')
    batch_query_details <- NULL
    tryCatch({
      batch_query_details <- rforcecom.getBatchDetails(session,
                                                       jobId=batch_query_info$jobId,
                                                       batchId=batch_query_info$id)
    }, error=function(e){
    })
    # close the job
    close_job_info <- rforcecom.closeBulkJob(session, jobId=job_info$id)
    return(batch_query_details)
  }
  batch_query_details <- rforcecom.getBatchDetails(session,
                                                   jobId=batch_query_info$jobId,
                                                   batchId=batch_query_info$id)
  
  batch_query_recordset <- rforcecom.getBulkQueryResult(session,
                                                        jobId=batch_query_info$jobId,
                                                        batchId=batch_query_info$id,
                                                        resultId=batch_query_details$result)
  close_job_info <- rforcecom.closeBulkJob(session, jobId=job_info$id)
  
  return(batch_query_recordset)
  
}