
#' xmlToList2
#' 
#' This function is an early and simple approach to converting an 
#' XML node or document into a more typical R list containing the data values. 
#' It differs from xmlToList by not including attributes at all in the output.
#' 
#' @usage xmlToList2(node)
#' @importFrom XML xmlApply xmlSApply xmlValue xmlAttrs xmlParse xmlSize xmlRoot
#' @param node the XML node or document to be converted to an R list
#' @return \code{list} parsed from the supplied node
xmlToList2 <- function(node){
  if (is.character(node)) 
    node = xmlParse(node)
  if (inherits(node, "XMLAbstractDocument")) 
    node = xmlRoot(node)
  if (any(inherits(node, c("XMLTextNode", "XMLInternalTextNode")))) 
    xmlValue(node)
  else if (xmlSize(node) == 0) 
    xmlAttrs(node)
  else {
    if (is.list(node)) {
      tmp = vals = xmlSApply(node, xmlToList2)
      tt = xmlSApply(node, inherits, c("XMLTextNode", "XMLInternalTextNode"))
    }
    else {
      tmp = vals = xmlApply(node, xmlToList2)
      tt = xmlSApply(node, inherits, c("XMLTextNode", "XMLInternalTextNode"))
    }
    vals[tt] = lapply(vals[tt], function(x) x[[1]])
    if (any(tt) && length(vals) == 1) 
      vals[[1]]
    else vals
  }
}

#' query_parser
#' 
#' A function specifically for parsing SOQL query XML into data.frames
#' 
#' @usage query_parser(xml)
#' @importFrom xml2 xml_find_all
#' @importFrom dplyr %>%
#' @importFrom purrr map_df
#' @param xml a \code{xml_document}
#' @return \code{data.frame} parsed from the supplied xml
query_parser <- function(xml){
  
  dat <- xml %>% 
    xml_find_all('records') %>%
    map_df(function(x){
      # capture any xmlToList grumblings about Namespace prefix
      invisible(capture.output(x_vals <- unlist(xmlToList2(as.character(x)))))
      return(as.data.frame(t(x_vals), stringsAsFactors=FALSE))
    }) %>% 
    as.data.frame()
  
  return(dat)
}
