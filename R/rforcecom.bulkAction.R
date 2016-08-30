#' Run Bulk Action
#'
#' This function is a convenience wrapper for submitting bulk API jobs
#'
#' @usage rforcecom.bulkAction(session,
#'                            operation=c('insert', 'delete',
#'                                        'upsert', 'update',
#'                                        'hardDelete'),
#'                            data,
#'                            object,
#'                            multiBatch=TRUE,
#'                            batchSize=10000,'
#'                            interval_seconds=5,
#'                            max_attempts=100,
#'                            verbose=FALSE)
#' @include rforcecom.createBulkJob.R rforcecom.createBulkBatch.R rforcecom.checkBatchStatus.R rforcecom.getBatchDetails.R rforcecom.closeBulkJob.R
#' @concept bulk job salesforce api
#' @references \url{https://developer.salesforce.com/docs/atlas.en-us.api_asynch.meta/api_asynch/}
#' @param session a named character vector defining parameters of the api connection as returned by \link{rforcecom.login}
#' @param operation a character string defining the type of operation being performed
#' @param data a matrix or data.frame that can be coerced into .csv file for submitting as batch request
#' @param object a character string defining the target salesforce object that the operation will be performed on
#' @param multiBatch a boolean value defining whether or not submit data in batches to the api
#' @param batchSize an integer value defining the number of records to submit if multiBatch is true.
#' The max value is 10000 in accordance with salesforce limits.
#' @param interval_seconds an integer defining the seconds between attempts to check for job completion
#' @param max_attempts an integer defining then max number attempts to check for job completion before stopping
#' @param verbose a boolean on whether to print the API attempt numbers
#' @return A \code{data.frame} of the results of the bulk job
#' @examples
#' \dontrun{
#' # update Account object
#' updates <- rforcecom.bulkAction(session, operation='update', data=my_data, object='Account')
#' }
#' @export
rforcecom.bulkAction <- function(session, operation=c('insert', 'delete', 'upsert',
                                'update', 'hardDelete'),
                                data,
                                object,
                                multiBatch=TRUE,
                                batchSize=10000,
                                interval_seconds=5,
                                max_attempts=100,
                                verbose=FALSE){

    job_info <- rforcecom.createBulkJob(session, operation, object)
    batch_query_info <- rforcecom.createBulkBatch(session,
                                                  jobId=job_info$id,
                                                  data=data,
                                                  multiBatch=multiBatch,
                                                  batchSize=batchSize)
    batch_query_details <- vector("list", nrow(batch_query_info))

    for(i in seq_along(batch_query_info)) {
        status_complete <- FALSE
        z <- 1
        Sys.sleep(interval_seconds)
        while (z < max_attempts & !status_complete){
            if (verbose){
                message(paste0("Batch #", i, ", Attempt #", z))
            }
            Sys.sleep(interval_seconds)
            batch_query_status <- rforcecom.checkBatchStatus(session,
                                                             jobId=batch_query_info[i, "jobId"],
                                                             batchId=batch_query_info[i, "id"])
            status_complete <- (batch_query_status$state=='Completed')
            z <- z + 1
        }
        if (!status_complete) {
            message('Issue with batches submitted.')
            # batch_query_details <- NULL
            tryCatch({
                batch_query_details[[i]] <- rforcecom.getBatchDetails(session,
                                                                 jobId=batch_query_info[i, "jobId"],
                                                                 batchId=batch_query_info[i, "id"])
            }, error=function(e){
            })
            # close the job
            # close_job_info <- rforcecom.closeBulkJob(session, jobId=job_info$id)
            # return(batch_query_details)
        }
        batch_query_details[[i]] <- rforcecom.getBatchDetails(session,
                                                         jobId=batch_query_info[i, "jobId"],
                                                         batchId=batch_query_info[i, "id"])
    }

    close_job_info <- rforcecom.closeBulkJob(session, jobId=job_info$id)

    return(batch_query_details)

}
