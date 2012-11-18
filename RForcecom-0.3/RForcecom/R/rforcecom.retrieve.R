rforcecom.retrieve <-
function(session, objectName, fields, limit=NULL, id=NULL){
 # Load packages
 require(XML)
 require(RCurl)
 
 # Make SOQL
 fieldList <- paste(fields, collapse=", ")
 soqlQuery <- paste("SELECT", fieldList, "FROM", objectName, sep=" ")
 
 # Add an id
 if(!is.null(id)){
  soqlQuery <- paste(soqlQuery, " WHERE Id ='", id, "'", sep="")
 }
 
 # Add limit phrase
 if(!is.null(limit)){
  soqlQuery <- paste(soqlQuery, " LIMIT ",limit, sep="")
 }
 
 # BEGIN DEBUG
 if(exists("rforcecom.debug") && rforcecom.debug){ message(soqlQuery) }
 # END DEBUG
 
 # Send a query
 resultSet <- rforcecom.query(session, soqlQuery)
 return(resultSet)
}

