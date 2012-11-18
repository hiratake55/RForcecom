\name{rforcecom.search}
\alias{rforcecom.search}

\title{
Execute a SOSL
}

\description{
Execute a SOSL
}

\usage{
rforcecom.search(session, queryString)
}

\arguments{
  \item{session}{ Session data. It can be retrieve from \code{\link{rforcecom.login}}. }
  \item{queryString}{ Query strings to search. (e.g. "United", "Electoronics") }
}

\value{
Result dataset.
}

\author{
Takekatsu Hiramura <thira@plavox.info>
}

\examples{
\dontrun{
 # Execute a SOSL
 queryString <- "United"
 rforcecom.search(session, queryString)
 }
}

\keyword{connection}

