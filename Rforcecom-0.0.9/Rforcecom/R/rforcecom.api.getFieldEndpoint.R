rforcecom.api.getFieldEndpoint <-
function(apiVersion, objectName, field){
 return(paste("services/data/v", apiVersion, "/sobjects/", objectName, "/", field, "/", sep=""))
}

