rforcecom.createBulkBatch <- 
  function(session, jobId, data, multiBatch = TRUE, batchSize=10000){
    
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