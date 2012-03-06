rforcecom.api.getSoqlEndpoint <-
function(apiVersion){
 return(paste("services/data/v", apiVersion, "/query/?q=", sep=""))
}

