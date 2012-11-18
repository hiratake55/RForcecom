\name{rforcecom.query}
\alias{rforcecom.query}

\title{
Execute a SOQL
}

\description{
Execute a SOQL
}

\usage{
rforcecom.query(session, soqlQuery)
}

\arguments{
  \item{session}{ Session data. It can be retrieve from \code{\link{rforcecom.login}}. }
  \item{soqlQuery}{ A SOQL query. (e.g. "SELECT Id, Name FROM Account") }
}

\value{
Result dataset.
}

\author{
Takekatsu Hiramura <thira@plavox.info>
}

\examples{
\dontrun{
 # Execute a SOQL
 soqlQuery <- "SELECT Id, Name, Industry, AnnualRevenue FROM Account"
 rforcecom.query(session, soqlQuery)
 }
}

\keyword{connection}


