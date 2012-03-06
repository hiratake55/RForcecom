\name{rforcecom.upsert}
\alias{rforcecom.upsert}

\title{
Upsert a record
}

\description{
Upsert a record
}

\usage{
rforcecom.upsert(session, objectName, externalIdField, externalId, fields)
}

\arguments{
  \item{session}{ Session data. It can be retrieve from \code{\link{rforcecom.login}}. }
  \item{objectName}{ An object name. (e.g. "Account", "Contact", "CustomObject__c") }
  \item{externalIdField}{ An External Key's field name. (e.g. "AccountMaster__c") }
  \item{externalId}{ An External Key's ID. }
  \item{fields}{ Sets of field name and its value. }
}


\value{
No data.
}

\author{
Takekatsu Hiramura <thira@plavox.info>
}

\examples{
\dontrun{
 # Upsert a record
 objectName <- "Account";
 externalIdField <- "AccountMaster__c"
 externalId <- "AM-00000151"
 fields <- c(Name="ABC Network Company", Phone="3333-3333-3333")
 rforcecom.upsert(session, objectName, externalIdField, externalId, fields)
 }
}

\keyword{connection}

