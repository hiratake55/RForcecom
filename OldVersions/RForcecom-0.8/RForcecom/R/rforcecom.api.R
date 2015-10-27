##################################
# Endpoint URLs
# * Internal Use Only 
##################################

rforcecom.api.getSoapEndpoint <- function(apiVersion){
 return(paste("services/Soap/u/", apiVersion, sep=""))
}

rforcecom.api.getRestEndpoint <- function(apiVersion){
 return(paste("services/data/v", apiVersion, sep=""))
}

rforcecom.api.getSoqlEndpoint <- function(apiVersion){
 return(paste("services/data/v", apiVersion, "/query/?q=", sep=""))
}

rforcecom.api.getSoqlAllEndpoint <- function(apiVersion){
  return(paste("services/data/v", apiVersion, "/queryAll/?q=", sep=""))
}

rforcecom.api.getSoslEndpoint <- function(apiVersion){
 return(paste("services/data/v", apiVersion, "/search/?q=", sep=""))
}

rforcecom.api.getObjectListEndpoint <- function(apiVersion){
 return(paste("services/data/v", apiVersion, "/sobjects/", sep=""))
}

rforcecom.api.getObjectDescriptionEndpoint <- function(apiVersion, objectName){
 objectName <- gsub(" ", "%20", objectName)
 return(paste("services/data/v", apiVersion, "/sobjects/", objectName, "/describe", sep=""))
}

rforcecom.api.getObjectEndpoint <- function(apiVersion, objectName){
 objectName <- gsub(" ", "%20", objectName)
 return(paste("services/data/v", apiVersion, "/sobjects/", objectName, "/", sep=""))
}

rforcecom.api.getRecordEndpoint <- function(apiVersion, objectName, id){
 objectName <- gsub(" ", "%20", objectName)
 id <- gsub(" ", "%20", id)
 return(paste("services/data/v", apiVersion, "/sobjects/", objectName, "/", id, "/", sep=""))
}

rforcecom.api.getFieldEndpoint <- function(apiVersion, objectName, field){
 objectName <- gsub(" ", "%20", objectName)
 field <- gsub(" ", "%20", field)
 return(paste("services/data/v", apiVersion, "/sobjects/", objectName, "/", field, "/", sep=""))
}

rforcecom.api.getExternalIdFieldEndpoint <- function(apiVersion, objectName, field, id){
 objectName <- gsub(" ", "%20", objectName)
 field <- gsub(" ", "%20", field)
 id <- gsub(" ", "%20", id)
 return(paste("services/data/v", apiVersion, "/sobjects/", objectName, "/", field, "/", id, "/", sep=""))
}

rforcecom.api.getBulkEndpoint <- function(apiVersion){
  return(paste("services/async/", apiVersion, sep=""))
}

