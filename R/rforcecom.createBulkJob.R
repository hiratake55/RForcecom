#' Create Bulk API Job 
#' 
#' This function initializes a Job in the Salesforce Bulk API
#'
#' @usage rforcecom.createBulkJob(session, 
#'                                operation=c('insert', 'delete', 
#'                                            'query', 'upsert', 
#'                                            'update', 'hardDelete'),
#'                                object='Account',
#'                                externalIdFieldName=NULL,
#'                                concurrencyMode='Parallel')
#' @concept bulk job salesforce api
#' @references \url{https://developer.salesforce.com/docs/atlas.en-us.api_asynch.meta/api_asynch/}
#' @param session a named character vector defining parameters of the api connection as returned by \link{rforcecom.login}
#' @param operation a character string defining the type of operation being performed
#' @param object a character string defining the target salesforce object that the operation will be performed on
#' @param externalIdFieldName a character string identifying a custom field that has the “External ID” attribute on the target object. 
#' This field is only used when performing upserts. It will be ignored for all other operations.
#' @param concurrencyMode a character string either "Parallel" or "Serial" that specifies whether batches should be completed
#' sequentially or in parallel. Use "Serial" only if Lock contentions persist with in "Parallel" mode.
#' @return A \code{list} parameters defining the created job, including id
#' @examples
#' \dontrun{
#' # insert into Account
#' job_info <- rforcecom.createBulkJob(session, operation='update', object='Account')
#' 
#' # delete from Account
#' job_info <- rforcecom.createBulkJob(session, operation='delete', object='Account')
#' 
#' # update into Account
#' job_info <- rforcecom.createBulkJob(session, operation='update', object='Account')
#' 
#' # upsert into Account
#' job_info <- rforcecom.createBulkJob(session, operation='upsert', externalIdFieldName='My_External_Id__c', object='Account')
#' 
#' # insert attachments
#' job_info <- rforcecom.createBulkJob(session, operation='insert', object='Attachment')
#' }
#' @export
rforcecom.createBulkJob <-
  function(session, operation=c('insert', 'delete', 'query',
                                'upsert', 'update', 'hardDelete'), 
           object='Account', 
           externalIdFieldName=NULL,
           concurrencyMode='Parallel'){
    
    if(operation=='upsert'){
      stopifnot(!is.null(externalIdFieldName))
    }
    
    if (object == 'Attachment') {
      if (operation != 'insert') stop('only insert operations are supported for the Attachment object')
      contentType <- 'ZIP_CSV'
    } else {
      contentType <- 'CSV' # user cannot control type  
    }
    
    # XML Body
    xmlBody <- paste0('<?xml version="1.0" encoding="UTF-8"?>
                      <jobInfo xmlns="http://www.force.com/2009/06/asyncapi/dataload">',
                        '<operation>', operation, '</operation>',
                        '<object>', object, '</object>',
                        if(operation=='upsert') paste0('<externalIdFieldName>', externalIdFieldName, '</externalIdFieldName>') else '',
                        '<concurrencyMode>', concurrencyMode, '</concurrencyMode>',
                        '<contentType>', contentType, '</contentType>',
                      '</jobInfo>')
    
    # Send request
    h <- basicHeaderGatherer() 
    t <- basicTextGatherer()
    endpointPath <- rforcecom.api.getBulkEndpoint(session['apiVersion'])
    URL <- paste(session['instanceURL'], endpointPath, '/job', sep="")
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
