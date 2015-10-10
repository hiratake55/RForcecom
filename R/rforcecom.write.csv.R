##################################
# Temp File Format for Bulk Loading
# * Internal Use Only
##################################
rforcecom.write.csv <-
  function(...) { 

    Call <- match.call(expand.dots = TRUE)
    for(argname in c("sep", "quote", "row.names", "col.names", "na", "qmethod", "fileEncoding"))
      if(!is.null(Call[[argname]]))
        warning(gettextf("attempt to set '%s' ignored", argname),
                domain = NA)
    Call$sep <- ","
    Call$quote <- TRUE
    Call$row.names <- FALSE
    Call$col.names <- TRUE
    Call$na <- "#N/A"
    Call$qmethod <- "double"
    Call$fileEncoding <- "UTF-8"
    Call[[1L]] <- as.name("write.table")
    eval.parent(Call)
    
  }