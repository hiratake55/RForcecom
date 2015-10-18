\name{rforcecom.logout}
\alias{rforcecom.logout}

\title{
Sign out of the Force.com (Salesforce.com)
}

\description{
Sign out of the Force.com (Salesforce.com)
}

\usage{
rforcecom.logout(session)
}

\arguments{
  \item{session}{ Session data. It can be retrieve from \code{\link{rforcecom.login}}. }
}

\value{
  \item{success}{ boolean }
}

\seealso{
 \code{\link{rforcecom.login}}
}

\examples{
\dontrun{
 # Sign out of the Force.com
 rforcecom.logout(session)
 }
}


\keyword{connection}
