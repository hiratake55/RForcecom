\name{rforcecom.query}
\alias{rforcecom.query}

\title{
Execute a SOQL
}

\description{
Execute a SOQL
}

\usage{
rforcecom.query(session, soqlQuery, queryAll=FALSE)
}

\arguments{
  \item{session}{ Session data. It can be retrieve from \code{\link{rforcecom.login}}. }
  \item{soqlQuery}{ A SOQL query. (ex: "SELECT Id, Name FROM Account") }
  \item{queryAll}{ A boolean. Indicate if query should include deleted and archived records 
  (available only on Task and Event records)}
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
 
 # Include records where IsDeleted=true
 soqlQuery <- "SELECT Id, IsDeleted, Subject, Description FROM Task"
 rforcecom.query(session, soqlQuery, queryAll=TRUE)
 }
}

\keyword{connection}


