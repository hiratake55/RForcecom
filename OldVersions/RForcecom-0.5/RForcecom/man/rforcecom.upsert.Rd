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
  \item{objectName}{ An object name. (ex: "Account", "Contact", "CustomObject__c") }
  \item{externalIdField}{ An external Key's field name. (ex: "AccountMaster__c") }
  \item{externalId}{ An external Key's ID. (ex: "999x000000xxxxxZZZ") }
  \item{fields}{ Sets of field name and its value. (ex: Name="CompanyName", Phone="000-000-000" ) }
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

