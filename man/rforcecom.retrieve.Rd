\name{rforcecom.retrieve}
\alias{rforcecom.retrieve}

\title{
Retrieve a record
}

\description{
Retrieve a record
}

\usage{
rforcecom.retrieve(session, objectName, fields, limit=NULL, id=NULL,
                   offset=NULL, order=NULL, inverse=NULL, nullsLast=NULL)
}

\arguments{
  \item{session}{ Session data. It can be retrieve from \code{\link{rforcecom.login}}. }
  \item{objectName}{ An object name. (ex: "Account", "Contact", "CustomObject__c") }
  \item{fields}{ A List of field names.  (ex: c("Id", "Name", "Industry", "AnnualRevenue)") )}
  \item{limit}{ Number of the records to retrieve. (ex: 5) }
  \item{id}{ Record ID to retrieve. (ex: "999x000000xxxxxZZZ") }
  \item{offset}{ Specifies the starting row offset. (ex: "100") }
  \item{order}{ A list for controling the order of query results. (ex: "c("Industry","Name")") }
  \item{inverse}{ If it is TRUE, the results are orderd in decending order. This parameter works when order parameter has been set. (Default: FALSE) }
  \item{nullsLast}{ If it is TRUE, null records list in last. If not null records list in first. This parameter works when order parameter has been set. (Default: FALSE) }
}

\value{
Result dataset.
}

\author{
Takekatsu Hiramura <thira@plavox.info>
}

\seealso{
 \code{\link{rforcecom.login}}
}

\examples{
\dontrun{
 # Retrieving a record
 objectName <- "Account"
 fields <- c("name", "Industry", "AnnualRevenue")
 rforcecom.retrieve(session, objectName, fields)
 rforcecom.retrieve(session, objectName, fields, limit = 5)
 rforcecom.retrieve(session, objectName, fields, id = "999x000000xxxxxZZZ")
 rforcecom.retrieve(session, objectName, fields)
 rforcecom.retrieve(session, objectName, fields, order = c("Industry","Name"))
 rforcecom.retrieve(session, objectName, fields, order = c("Industry","Name"),
                    inverse=TRUE, nullsLast=TRUE)
 }
}

\keyword{connection}


