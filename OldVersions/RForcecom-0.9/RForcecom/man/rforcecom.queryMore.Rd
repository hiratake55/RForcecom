\name{rforcecom.queryMore}
\alias{rforcecom.queryMore}

\title{
Retrieve remain records when executing a SOQL
}

\description{
Retrieve remain records when executing a SOQL
}

\usage{
rforcecom.queryMore(session, nextRecordsUrl)
}

\arguments{
  \item{session}{ Session data. It can be retrieve from \code{\link{rforcecom.login}}. }
  \item{nextRecordsUrl}{ An URL for next records. (ex: "/services/data/v26.0/query/999999999999999ZZZ-2000" ) }
}

\value{
Result dataset.
}

\author{
Takekatsu Hiramura <thira@plavox.info>
}

\examples{
\dontrun{
 # Retrieve remain records when executing a SOQL
 nextRecordsUrl <- "/services/data/v26.0/query/999999999999999ZZZ-2000"
 rforcecom.queryMore(session, soqlQuery)
 }
}

\keyword{connection}


