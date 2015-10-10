#' @export
rforcecom.getObjectDescription <-
function(session, objectName){
  
 # Send a query
 h <- basicHeaderGatherer()
 t <- basicTextGatherer()
 endpointPath <- rforcecom.api.getObjectDescriptionEndpoint(session['apiVersion'], objectName)
 URL <- paste(session['instanceURL'], endpointPath, sep="")
 OAuthString <- paste("Bearer", session['sessionID'])
 httpHeader <- c("Authorization"=OAuthString, "Accept"="application/xml", 'Content-Type'="application/xml")
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
 
 # Parse XML
 xdf <- getNodeSet(xmlParse(t$value()), "//fields")
 xdfList <- sapply(xdf,xmlToList)
 xdfDFList <- sapply(xdfList,data.frame)
 set <- function(x,y){
  coln <- unique(c(colnames(x),colnames(y)))
  x[coln[!coln %in% colnames(x)]] <- NA
  y[coln[!coln %in% colnames(y)]] <- NA
  rbind(x,y)
 }
 for(i in seq(xdfDFList)[-1]){
  xdfDFList[[1]] <- set(xdfDFList[[1]], xdfDFList[[i]])
 }
 xdf <- xdfDFList[[1]]
 
 return(xdf)
 
}

