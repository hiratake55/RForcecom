rforcecom.setProxy <-
function(proxyhost, port=NULL, username=NULL, password=NULL){
  # curlopts
  if(!exists("rforcecom.settings.curlopts")){
   rforcecom.settings.curlopts <<- list()
  }
  rforcecom.settings.curlopts['proxy'] <<- ""
  
  # username/password
  if(!is.null(username)){
   rforcecom.settings.curlopts['proxy'] <<- paste0(rforcecom.settings.curlopts['proxy'], username)
   if(!is.null(password)){
    rforcecom.settings.curlopts['proxy'] <<- paste0(rforcecom.settings.curlopts['proxy'], ":", password)
   }
   rforcecom.settings.curlopts['proxy'] <<- paste0(rforcecom.settings.curlopts['proxy'], "@")
  }
  # proxyhost
  if(!is.null(proxyhost)){
   rforcecom.settings.curlopts['proxy'] <<- paste0(rforcecom.settings.curlopts['proxy'], proxyhost)
  }
  # proxyhost
  if(!is.null(port)){
   rforcecom.settings.curlopts['proxy'] <<- paste0(rforcecom.settings.curlopts['proxy'], ":", port)
  }
}
