\name{rforcecom.delete}
\alias{rforcecom.delete}

\title{
Delete a record
}

\description{
Delete a record
}

\usage{
rforcecom.delete(session, objectName, id)
}

\arguments{
  \item{session}{ Session data. It can be retrieve from \code{\link{rforcecom.login}}. }
  \item{objectName}{ An object name. (ex: "Account", "Contact", "CustomObject__c") }
  \item{id}{ Record ID to retrieve. (ex: "999x000000xxxxxZZZ") }
}

\value{ No data. }

\author{
Takekatsu Hiramura <thira@plavox.info>
}

\examples{
\dontrun{
 # Deleting a record
 objectName <- "Account";
 id <- "999x000000xxxxxZZZ" # Record's Id
 rforcecom.delete(session, objectName, id)
 }
}

\keyword{connection}

