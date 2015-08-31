#' @export
rforcecom.queryMore <-
function(session, nextRecordsUrl){

 # Trim the first slash
 nextRecordsUrl <- sub("^/", "", nextRecordsUrl)

 # Retrieve XML via REST API
 h <- basicHeaderGatherer()
 t <- basicTextGatherer()
 endpointPath <- rforcecom.api.getSoqlEndpoint(session['apiVersion'])
 URL <- paste(session['instanceURL'], nextRecordsUrl, sep="")
 OAuthString <- paste("Bearer", session['sessionID'])
 httpHeader <- c("Authorization"=OAuthString, "Accept"="application/xml")
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
 try(errorcode <- iconv(xmlValue(x.root[['Error']][['errorCode']]), from="UTF-8", to=""), TRUE)
 try(errormessage <- iconv(xmlValue(x.root[['Error']][['message']]), from="UTF-8", to=""), TRUE)
 if(!is.na(errorcode) && !is.na(errormessage)){
  stop(paste(errorcode, errormessage, sep=": "))
 }
 
 # Convert XML to data frame
 xns <- getNodeSet(xmlParse(t$value()),'//records')
 xls <- lapply(lapply(xns, xmlToList), unlist)
 xls.rows <- length(xls)
 xls.colnames <- unique(unlist(lapply(xls, names)))
 xls.cols <- length(xls.colnames)
 xdf <- as.data.frame(replicate(xls.cols, rep(as.character(NA), xls.rows), simplify = FALSE), stringsAsFactors = FALSE)
 names(xdf) <- xls.colnames
 
 # When it is a empty data set
 if(nrow(xdf) == 0){ return(xdf) }
 
 # Fill data from retrieved XML
 for(i in 1:length(xls)){
  xdf[i, names(xls[[i]])] <- t(xls[[i]])
 }
 # Remove field attributes
 xdf <- subset(xdf, select=!grepl('\\.attrs\\.', names(xdf)))
 
 # Convert charset from UTF-8
 xdf.iconv <- data.frame(lapply(xdf, iconv, from="UTF-8", to=""), stringsAsFactors=FALSE)
 
 # Convert strings to correct data types
 xdf.iconv <- lapply(xdf.iconv, type.convert)
 
 # Check whether it has next record
 try(nextRecordsUrl <- iconv(xmlValue(x.root[['nextRecordsUrl']]), from="UTF-8", to=""), TRUE)
 if(!is.na(nextRecordsUrl)){
  nextRecords <- rforcecom.queryMore(session, nextRecordsUrl)
  xdf.iconv <- rbind.fill(xdf.iconv,nextRecords)
 }
 
 return(data.frame(xdf.iconv))
}

