##################################
# Endpoint URLs
# * Internal use 
##################################
#' @export
rforcecom.api.getSoapEndpoint <- function(apiVersion){
 return(paste("services/Soap/u/", apiVersion, sep=""))
}
#' @export
rforcecom.api.getRestEndpoint <- function(apiVersion){
 return(paste("services/data/v", apiVersion, sep=""))
}
#' @export
rforcecom.api.getSoqlEndpoint <- function(apiVersion){
 return(paste("services/data/v", apiVersion, "/query/?q=", sep=""))
}
#' @export
rforcecom.api.getSoslEndpoint <- function(apiVersion){
 return(paste("services/data/v", apiVersion, "/search/?q=", sep=""))
}
#' @export
rforcecom.api.getObjectListEndpoint <- function(apiVersion){
 return(paste("services/data/v", apiVersion, "/sobjects/", sep=""))
}
#' @export
rforcecom.api.getObjectDescriptionEndpoint <- function(apiVersion, objectName){
 objectName <- gsub(" ", "%20", objectName)
 return(paste("services/data/v", apiVersion, "/sobjects/", objectName, "/describe", sep=""))
}
#' @export
rforcecom.api.getObjectEndpoint <- function(apiVersion, objectName){
 objectName <- gsub(" ", "%20", objectName)
 return(paste("services/data/v", apiVersion, "/sobjects/", objectName, "/", sep=""))
}
#' @export
rforcecom.api.getRecordEndpoint <- function(apiVersion, objectName, id){
 objectName <- gsub(" ", "%20", objectName)
 id <- gsub(" ", "%20", id)
 return(paste("services/data/v", apiVersion, "/sobjects/", objectName, "/", id, "/", sep=""))
}
#' @export
rforcecom.api.getFieldEndpoint <- function(apiVersion, objectName, field){
 objectName <- gsub(" ", "%20", objectName)
 field <- gsub(" ", "%20", field)
 return(paste("services/data/v", apiVersion, "/sobjects/", objectName, "/", field, "/", sep=""))
}
#' @export
rforcecom.api.getExternalIdFieldEndpoint <- function(apiVersion, objectName, field, id){
 objectName <- gsub(" ", "%20", objectName)
 field <- gsub(" ", "%20", field)
 id <- gsub(" ", "%20", id)
 return(paste("services/data/v", apiVersion, "/sobjects/", objectName, "/", field, "/", id, "/", sep=""))
}
#' @export
rforcecom.api.getBulkEndpoint <- function(apiVersion){
  return(paste("services/async/", apiVersion, sep=""))
}

