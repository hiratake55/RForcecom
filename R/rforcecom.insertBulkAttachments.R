#' Insert Attachments via Bulk API Job
#' 
#' This function takes a file path to a structured .zip file of attachments and submits it to
#' an already existing Bulk API Job
#'
#' @usage rforcecom.insertBulkAttachments(session, jobId, file)
#' @concept bulk batch salesforce api
#' @references \url{https://developer.salesforce.com/docs/atlas.en-us.api_asynch.meta/api_asynch/}
#' @param session a named character vector defining parameters of the api connection as returned by \link{rforcecom.login}
#' @param jobId a character string defining the salesforce id assigned to a submitted job as returned by \link{rforcecom.createBulkJob}
#' @param file a file path to a .zip file containing request.txt manifest formatted as CSV and any binary attachments. It should 
#' have request.txt in the top-level (aka base) directory. Attachment files can be in subdirectories if desired. See Salesforce 
#' documentation for details on how to format the .zip file.
#' @return A \code{list} of parameters defining the created batch
#' @examples
#' \dontrun{
#' 
#' # sample .zip directory structure
#' request.zip
#'   request.txt
#'   attachment1.gif
#'   subdir/
#'     attachment2.doc
#'     
#' # sample format of request.txt
#' Name,ParentId,Body
#' attachment1.gif,TheTargetRecordIdHere,#attachment1.gif
#' subdir/attachment2.doc,TheTargetRecordIdHere,#subdir/attachment2.doc
#' 
#' f <- 'request.zip'
#' batch_attachment_info <- rforcecom.insertBulkAttachments(session, jobId=job_info$id, file=f)
#' }
#' @export
rforcecom.insertBulkAttachments <- 
  function(session, jobId, file){

    stopifnot(grepl("\\.zip$", file))
    
    # request parameters
    endpointPath <- rforcecom.api.getBulkEndpoint(session['apiVersion'])
    URL <- paste(session['instanceURL'], endpointPath, '/job/', jobId, '/batch', sep="")
    OAuthString <- unname(session['sessionID'])
    
    #make request
    res <- httr::POST(URL, config = httr::add_headers('X-SFDC-Session'=OAuthString,
                                                      'Accept'="application/xml", 
                                                      'Content-Type'="zip/csv; charset=UTF-8"),
                      body = httr::upload_file(path=file, type='zip/csv'))
    
    #cleanup the fileupload connection
    tryCatch({
      this_con <- as.integer(rownames(showConnections())[which(showConnections()[,'description', drop=F] == file)])
      close.connection(getConnection(this_con))
    }, error=function(e){
      message('Warning: Could not close file connection.')
      message('Having too many unclosed file handles may lead to error. Close manually with close.connection')
    })
    
    # Parse XML 
    x.root <- xmlRoot(content(res, as='parsed'))
    
    # BEGIN DEBUG
    if(exists("rforcecom.debug") && rforcecom.debug){ message(URL) }
    if(exists("rforcecom.debug") && rforcecom.debug){ message(x.root) }
    # END DEBUG
      
    return(xmlToList(x.root))

  }
