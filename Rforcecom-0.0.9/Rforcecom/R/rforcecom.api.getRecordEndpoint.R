rforcecom.api.getRecordEndpoint <-
function(apiVersion, objectName, id){
 return(paste("services/data/v", apiVersion, "/sobjects/", objectName, "/", id, "/", sep=""))
}

