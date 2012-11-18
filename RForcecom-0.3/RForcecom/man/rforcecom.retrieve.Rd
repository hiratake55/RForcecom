\name{rforcecom.retrieve}
\alias{rforcecom.retrieve}

\title{
Retrieving a record
}

\description{
Retrieving a record
}

\usage{
rforcecom.retrieve(session, objectName, fields, limit=NULL, id=NULL)
}

\arguments{
  \item{session}{ Session data. It can be retrieve from \code{\link{rforcecom.login}}. }
  \item{objectName}{ An object name. (e.g. "Account", "Contact", "CustomObject__c") }
  \item{fields}{ Field names. (e.g. "Id", "Name", "Industry", "AnnualRevenue" )}
  \item{limit}{ Number of the records to retrieve. (e.g. 5) }
  \item{id}{ Record ID to retrieve. (e.g. "999x000000xxxxxZZZ") }
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
 }
}

\keyword{connection}


