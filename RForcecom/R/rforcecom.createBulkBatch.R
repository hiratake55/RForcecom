#' Create and Add Batches to a Bulk API Job 
#' 
#' This function takes a data frame and submits it in batches to a 
#' an already existing Bulk API Job by chunking into temp files
#'
#' @usage rforcecom.createBulkBatch(session, jobId, data, multiBatch=TRUE, batchSize=10000)
#' @concept bulk batch salesforce api
#' @references \url{https://developer.salesforce.com/docs/atlas.en-us.api_asynch.meta/api_asynch/}
#' @param session a named character vector defining parameters of the api connection as returned by \link{rforcecom.login}
#' @param jobId a character string defining the salesforce id assigned to a submitted job as returned by \link{rforcecom.createBulkJob}
#' @param data a matrix or data.frame that can be coerced into .csv file for submitting as batch request
#' @param multiBatch a boolean value defining whether or not submit data in batches to the api
#' @param batchSize an integer value defining the number of records to submit if multiBatch is true. 
#' The max value is 10000 in accordance with salesforce limits.
#' @return A \code{list} of \code{list}s, one for each batch submitted, containing 10 parameters of the batch
#' @examples
#' \dontrun{
#' n <- 100
#' my_data <- data.frame(Name=paste('New Record:', 1:n), stringsAsFactors=FALSE)
#' batches_info <- rforcecom.createBulkBatch(session, 
#'                                           jobId=job_info$id, 
#'                                           data=my_data, 
#'                                           multiBatch=TRUE, 
#'                                           batchSize=50)
#' }
#' @export
rforcecom.createBulkBatch <- 
  function(session, jobId, data, multiBatch=TRUE, batchSize=10000){
    
    # parameter validation
    if (!is.matrix(data) & !is.data.frame(data)) stop("'data' must be either a matrix or a data frame")
    
    if (multiBatch == FALSE & nrow(data) > 10000) stop('multiBatch must be TRUE if the rows in data exceed the api limit of 10,000')

    if (batchSize > 10000) stop('batch size cannot exceed the api limit of 10,000')
    
    stopifnot(nrow(data) > 0, batchSize > 0)
      
    # batch the data if necessary
    batches_quotient <- seq.int(nrow(data)) %/% batchSize
    batches_remainder <- seq.int(nrow(data)) %% batchSize
    split_ind <- batches_quotient + 1
    split_ind[batches_remainder == 0] <- split_ind[batches_remainder == 0] - 1
    
    temp_file_list <- lapply(seq.int(max(split_ind)), FUN=function(x){
      f <- tempfile()
      rforcecom.write.csv(x=data[split_ind == x, , drop=FALSE], file=f)
      f
    })
    
    # request parameters
    endpointPath <- rforcecom.api.getBulkEndpoint(session['apiVersion'])
    URL <- paste(session['instanceURL'], endpointPath, '/job/', jobId, '/batch', sep="")
    OAuthString <- unname(session['sessionID'])
    
    batch_info <- lapply(temp_file_list, FUN=function(x){
      
      # cleanup the temp file
      on.exit(expr={unlink(x, force=TRUE)})
      
      #make request
      res <- httr::POST(URL, config = httr::add_headers('X-SFDC-Session'=OAuthString,
                                                        'Accept'="application/xml", 
                                                        'Content-Type'="text/csv; charset=UTF-8"),
                        body = httr::upload_file(path=x, type='text/csv'))
      closeAllConnections()
      # Parse XML 
      x.root <- xmlRoot(content(res, as='parsed'))
      
      # BEGIN DEBUG
      if(exists("rforcecom.debug") && rforcecom.debug){ message(URL) }
      if(exists("rforcecom.debug") && rforcecom.debug){ message(x.root) }
      # END DEBUG
      
      return(xmlToList(x.root))
    })
    
    return(batch_info)
  }
