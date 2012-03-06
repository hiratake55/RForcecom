rforcecom.api.getExternalIdFieldEndpoint <-
function(apiVersion, objectName, field, id){
 return(paste("services/data/v", apiVersion, "/sobjects/", objectName, "/", field, "/", id, "/", sep=""))
}

