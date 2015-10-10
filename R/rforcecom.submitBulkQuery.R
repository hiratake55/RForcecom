#' Submit Bulk Query Batch to a Bulk API Job 
#' 
#' This function takes a SOQL text string and submits the query to 
#' an already existing Bulk API Job of operation "query"
#'
#' @usage rforcecom.submitBulkQuery(session, jobId, query)
#' @concept bulk batch salesforce api
#' @references \url{https://developer.salesforce.com/docs/atlas.en-us.api_asynch.meta/api_asynch/}
#' @param session a named character vector defining parameters of the api connection as returned by \link{rforcecom.login}
#' @param jobId a character string defining the salesforce id assigned to a submitted job as returned by \link{rforcecom.createBulkJob}
#' @param query a character string defining a valid SOQL query on the Salesforce object associated with the job
#' @return A \code{list} parameters of the batch
#' @note Bulk API query doesn't support the following SOQL:
#' \itemize{
#'    \item COUNT
#'    \item ROLLUP
#'    \item SUM
#'    \item GROUP BY CUBE
#'    \item OFFSET
#'    \item Nested SOQL queries
#'    \item Relationship fields
#'    }
#'    Additionally, Bulk API can't access or query compound address or compound geolocation fields.
#' @examples
#' \dontrun{
#' my_query <- "SELECT Id, Name FROM Account LIMIT 10"
#' query_info <- rforcecom.submitBulkQuery(session, jobId=job_info$id, query=my_query)
#' }
#' @export
rforcecom.submitBulkQuery <- 
  function(session, jobId, query){
    
    f <- tempfile()
    cat(query, file=f)
    
    # request parameters
    endpointPath <- rforcecom.api.getBulkEndpoint(session['apiVersion'])
    URL <- paste(session['instanceURL'], endpointPath, '/job/', jobId, '/batch', sep="")
    OAuthString <- unname(session['sessionID'])
    
    res <- httr::POST(URL, config = httr::add_headers('X-SFDC-Session'=OAuthString,
                                                      'Accept'="application/xml", 
                                                      'Content-Type'="text/csv; charset=UTF-8"),
                      body = httr::upload_file(path=f, type='text/txt'))
      
    # Parse XML 
    x.root <- xmlRoot(content(res, as='parsed'))
    
    # BEGIN DEBUG
    if(exists("rforcecom.debug") && rforcecom.debug){ message(URL) }
    if(exists("rforcecom.debug") && rforcecom.debug){ message(x.root) }
    # END DEBUG
      
    return(xmlToList(x.root))
    
  }