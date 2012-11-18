\name{rforcecom.getServerTimestamp}
\alias{rforcecom.getServerTimestamp}

\title{
Retrieve a server timestamp
}

\description{
Retrieve a server timestamp
}

\usage{
rforcecom.getServerTimestamp(session)
}

\arguments{
  \item{session}{ Session data. It can be retrieve from \code{\link{rforcecom.login}}. }
}

\value{
Server timestamp (POSIXlt format).
}

\author{
Takekatsu Hiramura <thira@plavox.info>
}

\examples{
\dontrun{
 # Retrieve a server timestamp
 rforcecom.getServerTimestamp(session)
 # [1] "2012-04-01 11:30:05 GMT"
 }
}

\keyword{connection}


