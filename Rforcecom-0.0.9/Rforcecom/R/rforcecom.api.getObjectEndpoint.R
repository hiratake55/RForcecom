rforcecom.api.getObjectEndpoint <-
function(apiVersion, objectName){
 return(paste("services/data/v", apiVersion, "/sobjects/", objectName, "/", sep=""))
}

