##################################
# Endpoint URLs
# * Internal use 
##################################
rforcecom.api.getSoapEndpoint <- function(apiVersion){
 return(paste("services/Soap/u/", apiVersion, sep=""))
}
rforcecom.api.getSoqlEndpoint <- function(apiVersion){
 return(paste("services/data/v", apiVersion, "/query/?q=", sep=""))
}
rforcecom.api.getSoslEndpoint <- function(apiVersion){
 return(paste("services/data/v", apiVersion, "/search/?q=", sep=""))
}
rforcecom.api.getObjectEndpoint <- function(apiVersion, objectName){
 return(paste("services/data/v", apiVersion, "/sobjects/", objectName, "/", sep=""))
}
rforcecom.api.getRecordEndpoint <- function(apiVersion, objectName, id){
 return(paste("services/data/v", apiVersion, "/sobjects/", objectName, "/", id, "/", sep=""))
}
rforcecom.api.getFieldEndpoint <- function(apiVersion, objectName, field){
 return(paste("services/data/v", apiVersion, "/sobjects/", objectName, "/", field, "/", sep=""))
}
rforcecom.api.getExternalIdFieldEndpoint <- function(apiVersion, objectName, field, id){
 return(paste("services/data/v", apiVersion, "/sobjects/", objectName, "/", field, "/", id, "/", sep=""))
}


