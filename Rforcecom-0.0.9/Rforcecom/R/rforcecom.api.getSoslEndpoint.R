rforcecom.api.getSoslEndpoint <-
function(apiVersion){
 return(paste("services/data/v", apiVersion, "/search/?q=", sep=""))
}

