#' @export
rforcecom.retrieve <-
function(session, objectName, fields, limit=NULL, id=NULL, offset=NULL, order=NULL, inverse=NULL, nullsLast=NULL){

 # Make SOQL
 fieldList <- paste(fields, collapse=", ")
 soqlQuery <- paste("SELECT", fieldList, "FROM", objectName, sep=" ")
 
 # Add an id
 if(!is.null(id)){
  soqlQuery <- paste(soqlQuery, " WHERE Id ='", id, "'", sep="")
 }
 
 # Add order phrase
 if(!is.null(order)){
  if(is.list(order)){ orderList <- paste(order, collapse=", ") }
  else{ orderList <- order }
  soqlQuery <- paste(soqlQuery, " ORDER BY ", orderList, sep="")
  if(!is.null(inverse) && inverse == T){
   soqlQuery <- paste(soqlQuery, " DESC", sep="")
  }
  if(!is.null(nullsLast) && nullsLast == T){
   soqlQuery <- paste(soqlQuery, " NULLS LAST", sep="")
  }
 }
 
 # Add limit phrase
 if(!is.null(limit)){
  soqlQuery <- paste(soqlQuery, " LIMIT ",limit, sep="")
 }
 
 # Add offset phrase
 if(!is.null(offset)){
  soqlQuery <- paste(soqlQuery, " OFFSET ",offset, sep="")
 }
 # BEGIN DEBUG
 if(exists("rforcecom.debug") && rforcecom.debug){ message(soqlQuery) }
 # END DEBUG
 
 # Send a query
 resultSet <- rforcecom.query(session, soqlQuery)
 return(resultSet)
}

