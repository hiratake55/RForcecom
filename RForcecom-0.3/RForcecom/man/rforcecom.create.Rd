\name{rforcecom.create}
\alias{rforcecom.create}

\title{
Creating a record
}

\description{
Creating a record
}

\usage{
rforcecom.create(session, objectName, fields)
}

\arguments{
  \item{session}{ Session data. It can be retrieve from \code{\link{rforcecom.login}}. }
  \item{objectName}{ An object name. (e.g. "Account", "Contact", "CustomObject__c") }
  \item{fields}{ A list of field names and values. (e.g. Name="CompanyName", Phone="000-000-000" ) }
}

\value{
 No data.
}

\author{
Takekatsu Hiramura <thira@plavox.info>
}

\examples{
\dontrun{
 objectName <- "Account"
 fields <- c(Name="R Analytics Service Ltd", Phone="5555-5555-5555")
 rforcecom.create(session, objectName, fields)
 }
}

\keyword{connection}
