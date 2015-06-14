rforcecom.write.csv <-
  function(...) { 

    Call <- match.call(expand.dots = TRUE)
    for(argname in c("sep", "quote", "row.names", "col.names", "na", "fileEncoding"))
      if(!is.null(Call[[argname]]))
        warning(gettextf("attempt to set '%s' ignored", argname),
                domain = NA)
    Call$sep <- ","
    Call$quote <- FALSE
    Call$row.names <- FALSE
    Call$col.names <- TRUE
    Call$na <- ""
    Call$fileEncoding <- "UTF-8"
    Call[[1L]] <- as.name("write.table")
    eval.parent(Call)
    
  }