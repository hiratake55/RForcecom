rforcecom.retrieve <-
function(session, objectName, fields, ...){
 # Make SOQL
 fieldList <- paste(fields, collapse=", ")
 soqlQuery <- paste("SELECT", fieldList, "FROM", objectName, sep=" ")
 
 # Add an id
 if(length(list(...)$id) > 0){
  soqlQuery <- paste(soqlQuery, " WHERE Id ='", list(...)$id, "'", sep="")
 }
 
 # Add limit phrase
 if(length(list(...)$limit) > 0){
  soqlQuery <- paste(soqlQuery, " LIMIT ",list(...)$limit, sep="")
 }
 
 # Send a query
 resultSet <- rforcecom.query(session, soqlQuery)
 return(resultSet)
}

